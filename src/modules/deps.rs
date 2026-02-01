//! Dependency graph and resolution for multi-module compilation.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::syntax::form::ModulePath;
use crate::utils::Interner;

/// Error when a circular dependency is detected.
#[derive(Debug, Clone)]
pub struct CycleError {
    /// The cycle path (each element is a module path string)
    pub cycle: Vec<String>,
}

impl std::fmt::Display for CycleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Circular dependency: {}", self.cycle.join(" -> "))
    }
}

/// Information about a module in the dependency graph.
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// The module path
    pub path: String,
    /// Whether this module has been processed
    pub processed: bool,
}

/// Dependency graph for resolving module compilation order.
///
/// The graph tracks which modules depend on which other modules,
/// and can produce a topological ordering for compilation.
pub struct DependencyGraph {
    /// Module information
    nodes: HashMap<String, ModuleInfo>,
    /// Edges: module -> modules it depends on
    edges: HashMap<String, Vec<String>>,
}

impl DependencyGraph {
    /// Create a new empty dependency graph.
    pub fn new() -> Self {
        DependencyGraph {
            nodes: HashMap::new(),
            edges: HashMap::new(),
        }
    }

    /// Add a module to the graph with its dependencies.
    pub fn add_module(&mut self, path: &str, dependencies: Vec<String>) {
        // Add node if not present
        if !self.nodes.contains_key(path) {
            self.nodes.insert(
                path.to_string(),
                ModuleInfo {
                    path: path.to_string(),
                    processed: false,
                },
            );
        }

        // Add edges
        self.edges.insert(path.to_string(), dependencies.clone());

        // Ensure all dependencies are in the graph
        for dep in dependencies {
            if !self.nodes.contains_key(&dep) {
                self.nodes.insert(
                    dep.clone(),
                    ModuleInfo {
                        path: dep,
                        processed: false,
                    },
                );
            }
        }
    }

    /// Add a module using ModulePath (convenience method).
    pub fn add_module_path(
        &mut self,
        path: &ModulePath,
        dependencies: &[ModulePath],
        interner: &Interner,
    ) {
        let path_str = path.to_string_with(interner);
        let dep_strs: Vec<String> = dependencies
            .iter()
            .map(|p| p.to_string_with(interner))
            .collect();
        self.add_module(&path_str, dep_strs);
    }

    /// Get the dependencies of a module.
    pub fn get_dependencies(&self, path: &str) -> Option<&Vec<String>> {
        self.edges.get(path)
    }

    /// Check if a module exists in the graph.
    pub fn contains(&self, path: &str) -> bool {
        self.nodes.contains_key(path)
    }

    /// Get all module paths in the graph.
    pub fn modules(&self) -> impl Iterator<Item = &str> {
        self.nodes.keys().map(|s| s.as_str())
    }

    /// Perform topological sort to get compilation order.
    ///
    /// Returns modules in order such that dependencies come before dependents.
    /// Returns an error if a circular dependency is detected.
    pub fn topological_sort(&self) -> Result<Vec<String>, CycleError> {
        // Kahn's algorithm
        // edges[node] = [deps] means node depends on deps
        // In the "must come before" graph: dep -> node for each dep
        // So node has in-degree = number of dependencies

        let mut in_degree: HashMap<&str, usize> = HashMap::new();
        // reverse_edges[dep] = [nodes that depend on dep]
        let mut dependents: HashMap<&str, Vec<&str>> = HashMap::new();

        // Initialize
        for node in self.nodes.keys() {
            in_degree.insert(node.as_str(), 0);
            dependents.insert(node.as_str(), Vec::new());
        }

        // Calculate in-degrees: node has in-degree = len(deps)
        // Build dependents map: for each dep, add node to dependents[dep]
        for (node, deps) in &self.edges {
            // node depends on deps, so node has in-degree += len(deps)
            if let Some(deg) = in_degree.get_mut(node.as_str()) {
                *deg = deps.len();
            }
            // Each dep is depended upon by node
            for dep in deps {
                if let Some(dep_list) = dependents.get_mut(dep.as_str()) {
                    dep_list.push(node.as_str());
                }
            }
        }

        // Start with nodes that have no dependencies (in-degree 0)
        let mut queue: VecDeque<&str> = in_degree
            .iter()
            .filter(|(_, &deg)| deg == 0)
            .map(|(&node, _)| node)
            .collect();

        let mut result: Vec<String> = Vec::new();

        while let Some(node) = queue.pop_front() {
            result.push(node.to_string());

            // For each node that depends on this one, reduce its in-degree
            if let Some(deps) = dependents.get(node) {
                for &dependent in deps {
                    if let Some(deg) = in_degree.get_mut(dependent) {
                        *deg -= 1;
                        if *deg == 0 {
                            queue.push_back(dependent);
                        }
                    }
                }
            }
        }

        // Check for cycles (not all nodes processed)
        if result.len() != self.nodes.len() {
            // Find the cycle using DFS
            let cycle = self.find_cycle();
            return Err(CycleError { cycle });
        }

        Ok(result)
    }

    /// Find a cycle in the graph (for error reporting).
    fn find_cycle(&self) -> Vec<String> {
        let mut visited: HashSet<String> = HashSet::new();
        let mut rec_stack: HashSet<String> = HashSet::new();
        let mut path: Vec<String> = Vec::new();

        for node in self.nodes.keys() {
            if self.find_cycle_dfs(node, &mut visited, &mut rec_stack, &mut path) {
                return path;
            }
        }

        // Shouldn't happen if we detected a cycle in topological_sort
        vec!["unknown cycle".to_string()]
    }

    fn find_cycle_dfs(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
        path: &mut Vec<String>,
    ) -> bool {
        if rec_stack.contains(node) {
            // Found cycle - reconstruct it
            path.push(node.to_string());
            return true;
        }

        if visited.contains(node) {
            return false;
        }

        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());
        path.push(node.to_string());

        if let Some(deps) = self.edges.get(node) {
            for dep in deps {
                if self.find_cycle_dfs(dep.as_str(), visited, rec_stack, path) {
                    return true;
                }
            }
        }

        path.pop();
        rec_stack.remove(node);
        false
    }

    /// Get modules in reverse topological order (dependencies last).
    ///
    /// This is useful for codegen where we want to emit dependent modules first.
    pub fn reverse_topological_sort(&self) -> Result<Vec<String>, CycleError> {
        let mut result = self.topological_sort()?;
        result.reverse();
        Ok(result)
    }
}

impl Default for DependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_dependency() {
        let mut graph = DependencyGraph::new();
        graph.add_module("main", vec!["utils".to_string()]);
        graph.add_module("utils", vec![]);

        let order = graph.topological_sort().unwrap();
        assert_eq!(order, vec!["utils", "main"]);
    }

    #[test]
    fn test_diamond_dependency() {
        let mut graph = DependencyGraph::new();
        graph.add_module("main", vec!["a".to_string(), "b".to_string()]);
        graph.add_module("a", vec!["common".to_string()]);
        graph.add_module("b", vec!["common".to_string()]);
        graph.add_module("common", vec![]);

        let order = graph.topological_sort().unwrap();

        // common must come before a and b, which must come before main
        let common_pos = order.iter().position(|x| x == "common").unwrap();
        let a_pos = order.iter().position(|x| x == "a").unwrap();
        let b_pos = order.iter().position(|x| x == "b").unwrap();
        let main_pos = order.iter().position(|x| x == "main").unwrap();

        assert!(common_pos < a_pos);
        assert!(common_pos < b_pos);
        assert!(a_pos < main_pos);
        assert!(b_pos < main_pos);
    }

    #[test]
    fn test_circular_dependency() {
        let mut graph = DependencyGraph::new();
        graph.add_module("a", vec!["b".to_string()]);
        graph.add_module("b", vec!["c".to_string()]);
        graph.add_module("c", vec!["a".to_string()]);

        let result = graph.topological_sort();
        assert!(result.is_err());
    }

    #[test]
    fn test_no_dependencies() {
        let mut graph = DependencyGraph::new();
        graph.add_module("a", vec![]);
        graph.add_module("b", vec![]);
        graph.add_module("c", vec![]);

        let order = graph.topological_sort().unwrap();
        assert_eq!(order.len(), 3);
    }

    #[test]
    fn test_reverse_order() {
        let mut graph = DependencyGraph::new();
        graph.add_module("main", vec!["utils".to_string()]);
        graph.add_module("utils", vec![]);

        let order = graph.reverse_topological_sort().unwrap();
        assert_eq!(order, vec!["main", "utils"]);
    }
}

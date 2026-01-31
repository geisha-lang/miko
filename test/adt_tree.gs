data Tree a {
    Leaf(a),
    Node(Tree a, Tree a)
}

def sumTree(t) = match t {
    Leaf(x) -> x,
    Node(l, r) -> sumTree(l) + sumTree(r)
}

def countLeaves(t) = match t {
    Leaf(x) -> 1,
    Node(l, r) -> countLeaves(l) + countLeaves(r)
}

def main() = putNumber(sumTree(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))))

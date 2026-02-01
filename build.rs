use std::env;
use std::path::PathBuf;

fn main() {
    // Get output directory from cargo
    let out_dir = env::var("OUT_DIR").unwrap();

    // Compile the Geisha runtime library
    cc::Build::new()
        .file("src/lib/base.c")
        .file("src/lib/runtime/gc_bitmap.c")
        .include("src/lib/include")
        .opt_level(2)
        .warnings(true)
        .compile("geisha_runtime");

    // Tell cargo to link against the runtime
    println!("cargo:rustc-link-search=native={}", out_dir);
    println!("cargo:rustc-link-lib=static=geisha_runtime");

    // Re-run build script if runtime sources change
    println!("cargo:rerun-if-changed=src/lib/base.c");
    println!("cargo:rerun-if-changed=src/lib/runtime/gc_bitmap.c");
    println!("cargo:rerun-if-changed=src/lib/include/gsvalue.h");
    println!("cargo:rerun-if-changed=src/lib/include/gc.h");
    println!("cargo:rerun-if-changed=src/lib/include/gc_bitmap.h");

    // Export the runtime library path for the compiler to use at link time
    let runtime_path = PathBuf::from(&out_dir).join("libgeisha_runtime.a");
    println!("cargo:rustc-env=GEISHA_RUNTIME_PATH={}", runtime_path.display());
}

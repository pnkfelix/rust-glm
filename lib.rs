#![crate_id="glm#0.0.1"]
#![crate_type = "lib"]

#![desc = "GLM port"]
#![license = "MIT"]

#![feature(macro_rules)]

extern crate test;

pub use src::typedefs::{vec1,vec2,vec3,vec4};
pub use src::typedefs::{dvec1,dvec2,dvec3,dvec4};
pub use src::typedefs::{bvec1,bvec2,bvec3,bvec4};
pub use src::typedefs::{ivec1,ivec2,ivec3,ivec4};
pub use src::typedefs::{uvec1,uvec2,uvec3,uvec4};

pub use src::typedefs::{mat2,mat3,mat4};
pub use src::typedefs::{mat2x2,mat2x3,mat2x4};
pub use src::typedefs::{mat3x2,mat3x3,mat3x4};
pub use src::typedefs::{mat4x2,mat4x3,mat4x4};

pub use src::vector::vec2;

mod src {
    // all non-lib.rs files go into src/ subdirectory and are thus
    // listed here.

    pub mod typedefs;
    pub mod matrix;
    pub mod vector;
}

/// This is to work around bugs in our reachability analysis that can
/// cause false lint-warnings (Issue #14421), or worse, link failures
/// (Issue #14422).
pub mod dont_use {
    pub use src::vector;
}

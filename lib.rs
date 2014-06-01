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
pub use src::matrix::{mat2,mat2x2,mat2x3,mat2x4};
pub use src::matrix::{mat3,mat3x2,mat3x3,mat3x4};
pub use src::matrix::{mat4,mat4x2,mat4x3,mat4x4};
pub use src::matrix::inverse;

macro_rules! double_dispatch_T {
    ( $trait_:ident for $LHS_type:ident $method:ident via $RHS_trait:ident $rev_method:ident ) =>
    {
        pub trait $RHS_trait<T,RET> {
            fn $rev_method(&self, lhs: & $LHS_type<T>) -> RET;
        }

        impl<T,RET,RHS:$RHS_trait<T,RET>> $trait_<RHS,RET> for $LHS_type<T> {
            fn $method(&self, rhs: &RHS) -> RET { rhs.$rev_method(self) }
        }
    };
    ( $trait_:ident for mut $LHS_type:ident $method:ident via $RHS_trait:ident $rev_method:ident ) =>
    {
        pub trait $RHS_trait<T> {
            fn $rev_method(&self, lhs: &mut $LHS_type<T>);
        }

        impl<T,RHS:$RHS_trait<T>> $trait_<RHS> for $LHS_type<T> {
            fn $method(&mut self, rhs: &RHS) { rhs.$rev_method(self) }
        }
    };
}

macro_rules! all_choices {
    ( $m:ident :
      todo: {}
      done: { $( ( $($i:ident),* ) )* } ) =>
    {
        $( $m!( $($i),* ) )*
    };
    ( $m:ident :
      todo: { ($a:ident | $b:ident) $( ($p:ident | $q:ident) )* }
      done: { $( ( $($i:ident),* ) )* } ) =>
    {
        all_choices!( $m :
                      todo: { $( ( $p | $q ) )* }
                      done: { $( ( $a, $( $i ),* ) )* $( ( $b, $( $i ),* ) )* } )
    };
}

mod src {
    // all non-lib.rs files go into src/ subdirectory and are thus
    // listed here.

    pub mod typedefs;
    pub mod operators;
    pub mod matrix;
    pub mod vector;
    pub mod scalar;
}

/// This is to work around bugs in our reachability analysis that can
/// cause false lint-warnings (Issue #14421), or worse, link failures
/// (Issue #14422).
pub mod dont_use {
    pub use src::vector;
}

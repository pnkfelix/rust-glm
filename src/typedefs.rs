#![allow(non_camel_case_types)]

use super::vector::{TVec1, TVec2, TVec3, TVec4};
use super::matrix::{TMat2, TMat3, TMat4};
use super::matrix::{TMat2x3, TMat2x4, TMat3x2, TMat3x4, TMat4x2, TMat4x3};

/// One-component single-precision floating-point vector
pub type vec1 = TVec1<f32>;
/// Two-component single-precision floating-point vector
pub type vec2 = TVec2<f32>;
/// Three-component single-precision floating-point vector
pub type vec3 = TVec3<f32>;
/// Four-component single-precision floating-point vector
pub type vec4 = TVec4<f32>;

/// One-component double-precision floating-point vector
pub type dvec1 = TVec1<f64>;
/// Two-component double-precision floating-point vector
pub type dvec2 = TVec2<f64>;
/// Three-component double-precision floating-point vector
pub type dvec3 = TVec3<f64>;
/// Four-component double-precision floating-point vector
pub type dvec4 = TVec4<f64>;

/// One-component boolean vector
pub type bvec1 = TVec1<bool>;
/// Two-component boolean vector
pub type bvec2 = TVec2<bool>;
/// Three-component boolean vector
pub type bvec3 = TVec3<bool>;
/// Four-component boolean vector
pub type bvec4 = TVec4<bool>;

/// One-component signed integer vector
pub type ivec1 = TVec1<i32>;
/// Two-component signed integer vector
pub type ivec2 = TVec2<i32>;
/// Three-component signed integer vector
pub type ivec3 = TVec3<i32>;
/// Four-component signed integer vector
pub type ivec4 = TVec4<i32>;

/// One-component unsigned integer vector
pub type uvec1 = TVec1<u32>;
/// Two-component unsigned integer vector
pub type uvec2 = TVec2<u32>;
/// Three-component unsigned integer vector
pub type uvec3 = TVec3<u32>;
/// Four-component unsigned integer vector
pub type uvec4 = TVec4<u32>;

// A 2x2 single-precision floating-point matrix
pub type mat2 = TMat2<f32>;
// A 3x3 single-precision floating-point matrix
pub type mat3 = TMat3<f32>;
// A 4x4 single-precision floating-point matrix
pub type mat4 = TMat4<f32>;

// Same as a mat2
pub type mat2x2 = mat2;
// A single-precision floating-point matrix with 2 columns and 3 rows
pub type mat2x3 = TMat2x3<f32>;
// A single-precision floating-point matrix with 2 columns and 4 rows
pub type mat2x4 = TMat2x4<f32>;
// A single-precision floating-point matrix with 3 columns and 2 rows
pub type mat3x2 = TMat3x2<f32>;
// Same as a mat3
pub type mat3x3 = mat3;
// A single-precision floating-point matrix with 3 columns and 4 rows
pub type mat3x4 = TMat3x4<f32>;
// A single-precision floating-point matrix with 3 columns and 2 rows
pub type mat4x2 = TMat4x2<f32>;
// Same as a mat3
pub type mat4x3 = TMat4x3<f32>;
// A single-precision floating-point matrix with 3 columns and 4 rows
pub type mat4x4 = mat4;

// A 2x2 double-precision floating-point matrix
pub type dmat2 = TMat2<f64>;
// A 3x3 double-precision floating-point matrix
pub type dmat3 = TMat3<f64>;
// A 4x4 double-precision floating-point matrix
pub type dmat4 = TMat4<f64>;

// Same as a dmat2
pub type dmat2x2 = dmat2;
// A double-precision floating-point matrix with 2 columns and 3 rows
pub type dmat2x3 = TMat2x3<f64>;
// A double-precision floating-point matrix with 2 columns and 4 rows
pub type dmat2x4 = TMat2x4<f64>;
// A double-precision floating-point matrix with 3 columns and 2 rows
pub type dmat3x2 = TMat3x2<f64>;
// Same as a dmat3
pub type dmat3x3 = mat3;
// A double-precision floating-point matrix with 3 columns and 4 rows
pub type dmat3x4 = TMat3x4<f64>;
// A double-precision floating-point matrix with 3 columns and 2 rows
pub type dmat4x2 = TMat4x2<f64>;
// Same as a dmat3
pub type dmat4x3 = TMat4x3<f64>;
// A double-precision floating-point matrix with 3 columns and 4 rows
pub type dmat4x4 = mat4;

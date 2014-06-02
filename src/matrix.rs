pub use src::typedefs::{vec2,vec3,vec4};
pub use src::typedefs::{mat2x2,mat2x3,mat2x4};
pub use src::typedefs::{mat3x2,mat3x3,mat3x4};
pub use src::typedefs::{mat4x2,mat4x3,mat4x4};

use src::vector::{TVec2,TVec2MulRHS};
use src::vector::{TVec3,TVec3MulRHS};
use src::vector::{TVec4,TVec4MulRHS};
use src::vector::Summable;
use src::scalar::{S,SMulRHS,SDivRHS};
use std::fmt;

trait Columns<'a,ColRefVec> {
    fn num_cols(&self) -> uint;
    fn col_ref(&'a self, i: uint) -> ColRefVec;
}

trait ColumnsData<'a,ColVec,ColRefVec> : Columns<'a,ColRefVec> {
    fn col(&self, i: uint) -> ColVec;
}

trait Rows<'a,RowRefVec> {
    fn num_rows(&self) -> uint;
    fn row_ref(&'a self, j: uint) -> RowRefVec;
}

trait RowsData<'a,RowVec,RowRefVec> : Rows<'a,RowRefVec> {
    fn row(&self, j: uint) -> RowVec;
}


trait Matrix<T> {
    fn cr<'a>(&'a self, c: uint, r: uint) -> &'a T;
    fn rc<'a>(&'a self, r: uint, c: uint) -> &'a T { self.cr(c,r) }
    fn cr_mut<'a>(&'a mut self, c: uint, r: uint) -> &'a mut T;
    fn rc_mut<'a>(&'a mut self, r: uint, c: uint) -> &'a mut T {
        self.cr_mut(c,r)
    }
}

trait Invertible {
    // (should this be Result<Self> ?) 
    // (also, should this take self by value?)
    fn inverse(&self) -> Self;
}

trait Mappable<T,U,SelfU> {
    fn map(&self, |&T| -> U) -> SelfU;
}

pub fn inverse<M:Invertible>(m: &M) -> M { m.inverse() }

// All the matrix representations use column-major order:
// namely, [[a, b, c], [d, e, f]]
// should be read as corresponding to the 2 x 3 matrix:
//
// ( a d |
// | b e |
// | c f )

pub struct TMat2<T> {
    elems: [[T, ..2], ..2]
}
pub struct TMat3<T> {
    elems: [[T, ..3], ..3]
}

pub struct TMat4<T> {
    elems: [[T, ..4], ..4]
}

pub struct TMat2x3<T> {
    elems: [[T, ..3], ..2]
}
pub struct TMat2x4<T> {
    elems: [[T, ..4], ..2]
}
pub struct TMat3x2<T> {
    elems: [[T, ..2], ..3]
}
pub struct TMat3x4<T> {
    elems: [[T, ..4], ..3]
}
pub struct TMat4x2<T> {
    elems: [[T, ..2], ..4]
}
pub struct TMat4x3<T> {
    elems: [[T, ..3], ..4]
}

macro_rules! impl_Matrix_for {
    ($TMat:ident) =>
    {
        impl<T> Matrix<T> for $TMat<T> {
            fn cr<'a>(&'a self, c: uint, r: uint) -> &'a T {
                &self.elems[c][r]
            }
            fn cr_mut<'a>(&'a mut self, c: uint, r: uint) -> &'a mut T {
                &mut self.elems[c][r]
            }
        }
    };
}

impl_Matrix_for!(TMat2)
impl_Matrix_for!(TMat2x3)
impl_Matrix_for!(TMat2x4)
impl_Matrix_for!(TMat3x2)
impl_Matrix_for!(TMat3)
impl_Matrix_for!(TMat3x4)
impl_Matrix_for!(TMat4x2)
impl_Matrix_for!(TMat4x3)
impl_Matrix_for!(TMat4)

macro_rules! tvec_of_len {
    ($T:ident, 2) => { TVec2<$T> };
    ($T:ident, 3) => { TVec3<$T> };
    ($T:ident, 4) => { TVec4<$T> };
}

macro_rules! impl_Columns_for {
    ( $TMat:ident $ncols:expr 2 ) =>
    {
        impl<'a,T> Columns<'a,TVec2<&'a T>> for $TMat<T> {
            fn num_cols(&self) -> uint { $ncols }
            fn col_ref(&'a self, i: uint) -> TVec2<&'a T> {
                assert!(i < $ncols);
                TVec2 { x: &self.elems[i][0],
                        y: &self.elems[i][1],
                }
            }
        }
        impl<'a,T:Clone> ColumnsData<'a, TVec2<T>,TVec2<&'a T>> for $TMat<T> {
            fn col(&self, i: uint) -> TVec2<T> {
                assert!(i < $ncols);
                TVec2 { x: self.elems[i][0].clone(),
                        y: self.elems[i][1].clone(),
                }
            }
        }
    };
    ( $TMat:ident $ncols:expr 3 ) =>
    {
        impl<'a,T> Columns<'a,TVec3<&'a T>> for $TMat<T> {
            fn num_cols(&self) -> uint { $ncols }
            fn col_ref(&'a self, i: uint) -> TVec3<&'a T> {
                assert!(i < $ncols);
                TVec3 { x: &self.elems[i][0],
                        y: &self.elems[i][1],
                        z: &self.elems[i][2],
                }
            }
        }
        impl<'a,T:Clone> ColumnsData<'a,TVec3<T>,TVec3<&'a T>> for $TMat<T> {
            fn col(&self, i: uint) -> TVec3<T> {
                assert!(i < $ncols);
                TVec3 { x: self.elems[i][0].clone(),
                        y: self.elems[i][1].clone(),
                        z: self.elems[i][2].clone(),
                }
            }
        }
    };
    ( $TMat:ident $ncols:expr 4 ) =>
    {
        impl<'a,T> Columns<'a,TVec4<&'a T>> for $TMat<T> {
            fn num_cols(&self) -> uint { $ncols }
            fn col_ref(&'a self, i: uint) -> TVec4<&'a T> {
                assert!(i < $ncols);
                TVec4 { x: &self.elems[i][0],
                        y: &self.elems[i][1],
                        z: &self.elems[i][2],
                        w: &self.elems[i][3],
                }
            }
        }
        impl<'a,T:Clone> ColumnsData<'a,TVec4<T>,TVec4<&'a T>> for $TMat<T> {
            fn col(&self, i: uint) -> TVec4<T> {
                assert!(i < $ncols);
                TVec4 { x: self.elems[i][0].clone(),
                        y: self.elems[i][1].clone(),
                        z: self.elems[i][2].clone(),
                        w: self.elems[i][3].clone(),
                }
            }
        }
    };
}

macro_rules! impl_Rows_for {
    ( $TMat:ident 2 $nrows:expr ) =>
    {
        impl<'a,T> Rows<'a,TVec2<&'a T>> for $TMat<T> {
            fn num_rows(&self) -> uint { $nrows }
            fn row_ref(&'a self, j: uint) -> TVec2<&'a T> {
                assert!(j < $nrows);
                TVec2 { x: &self.elems[0][j],
                        y: &self.elems[1][j],
                }
            }
        }
        impl<'a,T:Clone> RowsData<'a,TVec2<T>,TVec2<&'a T>> for $TMat<T> {
            fn row(&self, j: uint) -> TVec2<T> {
                assert!(j < $nrows);
                TVec2 { x: self.elems[0][j].clone(),
                        y: self.elems[1][j].clone(),
                }
            }
        }
    };
    ( $TMat:ident 3 $nrows:expr ) =>
    {
        impl<'a,T> Rows<'a,TVec3<&'a T>> for $TMat<T> {
            fn num_rows(&self) -> uint { $nrows }
            fn row_ref(&'a self, j: uint) -> TVec3<&'a T> {
                assert!(j < $nrows);
                TVec3 { x: &self.elems[0][j],
                        y: &self.elems[1][j],
                        z: &self.elems[2][j],
                }
            }
        }
        impl<'a,T:Clone> RowsData<'a,TVec3<T>,TVec3<&'a T>> for $TMat<T> {
            fn row(&self, j: uint) -> TVec3<T> {
                assert!(j < $nrows);
                TVec3 { x: self.elems[0][j].clone(),
                        y: self.elems[1][j].clone(),
                        z: self.elems[2][j].clone(),
                }
            }
        }
    };
    ( $TMat:ident 4 $nrows:expr ) =>
    {
        impl<'a,T> Rows<'a,TVec4<&'a T>> for $TMat<T> {
            fn num_rows(&self) -> uint { $nrows }
            fn row_ref(&'a self, j: uint) -> TVec4<&'a T> {
                assert!(j < $nrows);
                TVec4 { x: &self.elems[0][j],
                        y: &self.elems[1][j],
                        z: &self.elems[2][j],
                        w: &self.elems[3][j],
                }
            }
        }
        impl<'a,T:Clone> RowsData<'a,TVec4<T>,TVec4<&'a T>> for $TMat<T> {
            fn row(&self, j: uint) -> TVec4<T> {
                assert!(j < $nrows);
                TVec4 { x: self.elems[0][j].clone(),
                        y: self.elems[1][j].clone(),
                        z: self.elems[2][j].clone(),
                        w: self.elems[3][j].clone(),
                }
            }
        }
    };
}

macro_rules! impl_ColRow_for {
    ( $TMat:ident $ncols:tt $nrows:tt ) =>
    {
        impl_Columns_for!    ($TMat $ncols $nrows)
        impl_Rows_for!       ($TMat $ncols $nrows)
    }
}

impl_ColRow_for!(TMat2   2 2)
impl_ColRow_for!(TMat2x3 2 3)
impl_ColRow_for!(TMat2x4 2 4)
impl_ColRow_for!(TMat3   3 3)
impl_ColRow_for!(TMat3x2 3 2)
impl_ColRow_for!(TMat3x4 3 4)
impl_ColRow_for!(TMat4   4 4)
impl_ColRow_for!(TMat4x2 4 2)
impl_ColRow_for!(TMat4x3 4 3)

impl<T:Num> Invertible for TMat2<T> {
    fn inverse(&self) -> TMat2<T> {
        #![allow(uppercase_variables)]
        use std::num::One;
        let m = &self.elems;
        let one : T = One::one();
        let OneOverDeterminant = one / (m[0][0] * m[1][1] - m[1][0] * m[0][1]);
        TMat2 { elems: [[m[1][1] * OneOverDeterminant, m[0][1] * OneOverDeterminant],
                        [m[1][0] * OneOverDeterminant, m[0][0] * OneOverDeterminant]] }
    }
}

macro_rules! impl_Show_for {
    ($TMat:ident $ncols:expr $nrows:expr) =>
    {
        impl<T:fmt::Show> fmt::Show for $TMat<T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                try!(write!(f, "["));
                for u in range(0u, $ncols-1) {
                    try!(self.elems[u].as_slice().fmt(f));
                    try!(write!(f, "\n "));
                }
                try!(self.elems[$ncols-1].as_slice().fmt(f));
                write!(f, "]")
            }
        }
    }
}

macro_rules! impl_Eq_for {
    ($TMat:ident $ncols:expr $nrows:expr) =>
    {
        impl<T:Eq> Eq for $TMat<T> {
            fn eq(&self, rhs: &$TMat<T>) -> bool {
                for u in range(0u, $ncols) {
                    if self.elems[u] != rhs.elems[u] {
                        return false;
                    }
                }
                return true;
            }
        }
    }
}

macro_rules! easy_impls_for {
    ($TMat:ident $ncols:expr $nrows:expr) =>
    {
        impl_Show_for!($TMat $ncols $nrows)
        impl_Eq_for!($TMat $ncols $nrows)
    }
}

easy_impls_for!(TMat2   2 2)
easy_impls_for!(TMat2x3 2 3)
easy_impls_for!(TMat2x4 2 4)
easy_impls_for!(TMat3   3 3)
easy_impls_for!(TMat3x2 3 2)
easy_impls_for!(TMat3x4 3 4)
easy_impls_for!(TMat4   4 4)
easy_impls_for!(TMat4x2 4 2)
easy_impls_for!(TMat4x3 4 3)

impl<T,U> Mappable<T,U,TMat2<U>> for TMat2<T> {
    fn map(&self, f: |&T| -> U) -> TMat2<U> {
        TMat2 { elems: [[f(&self.elems[0][0]), f(&self.elems[0][1])],
                        [f(&self.elems[1][0]), f(&self.elems[1][1])]] }
    }
}
impl<T,U> Mappable<T,U,TMat2x3<U>> for TMat2x3<T> {
    fn map(&self, f: |&T| -> U) -> TMat2x3<U> {
        TMat2x3 { elems: [[f(&self.elems[0][0]), f(&self.elems[0][1]), f(&self.elems[0][2])],
                          [f(&self.elems[1][0]), f(&self.elems[1][1]), f(&self.elems[1][2])]] }
    }
}

impl<T,U> Mappable<T,U,TMat2x4<U>> for TMat2x4<T> {
    fn map(&self, f: |&T| -> U) -> TMat2x4<U> {
        TMat2x4 { elems: [[f(&self.elems[0][0]), f(&self.elems[0][1]), f(&self.elems[0][2]), f(&self.elems[0][3])],
                          [f(&self.elems[1][0]), f(&self.elems[1][1]), f(&self.elems[1][2]), f(&self.elems[1][3])]] }
    }
}

impl<T,U> Mappable<T,U,TMat3x2<U>> for TMat3x2<T> {
    fn map(&self, f: |&T| -> U) -> TMat3x2<U> {
        TMat3x2 { elems: [[f(&self.elems[0][0]), f(&self.elems[0][1])],
                          [f(&self.elems[1][0]), f(&self.elems[1][1])],
                          [f(&self.elems[2][0]), f(&self.elems[2][1])]] }
    }
}

pub trait Mat2x2Args { fn make(self) -> mat2x2; }
pub trait Mat2x3Args { fn make(self) -> mat2x3; }
pub trait Mat2x4Args { fn make(self) -> mat2x4; }
pub trait Mat3x2Args { fn make(self) -> mat3x2; }
pub trait Mat3x3Args { fn make(self) -> mat3x3; }
pub trait Mat3x4Args { fn make(self) -> mat3x4; }
pub trait Mat4x2Args { fn make(self) -> mat4x2; }
pub trait Mat4x3Args { fn make(self) -> mat4x3; }
pub trait Mat4x4Args { fn make(self) -> mat4x4; }

pub fn mat2<Args:Mat2x2Args>(args: Args) -> mat2x2 { args.make() }
pub fn mat2x2<Args:Mat2x2Args>(args: Args) -> mat2x2 { args.make() }
pub fn mat2x3<Args:Mat2x3Args>(args: Args) -> mat2x3 { args.make() }
pub fn mat2x4<Args:Mat2x4Args>(args: Args) -> mat2x4 { args.make() }
pub fn mat3<Args:Mat3x3Args>(args: Args) -> mat3x3 { args.make() }
pub fn mat3x2<Args:Mat3x2Args>(args: Args) -> mat3x2 { args.make() }
pub fn mat3x3<Args:Mat3x3Args>(args: Args) -> mat3x3 { args.make() }
pub fn mat3x4<Args:Mat3x4Args>(args: Args) -> mat3x4 { args.make() }
pub fn mat4<Args:Mat4x4Args>(args: Args) -> mat4x4 { args.make() }
pub fn mat4x2<Args:Mat4x2Args>(args: Args) -> mat4x2 { args.make() }
pub fn mat4x3<Args:Mat4x3Args>(args: Args) -> mat4x3 { args.make() }
pub fn mat4x4<Args:Mat4x4Args>(args: Args) -> mat4x4 { args.make() }

macro_rules! impl_Mat2x2Args_for {
    ($a:ident copy) => {
        impl Mat2x2Args for $a {
            fn make(self) -> mat2x2 {
                let x = self;
                TMat2 { elems: [[x as f32,x as f32],
                                [x as f32,x as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,$c:ident,$d:ident) => {
        impl Mat2x2Args for ($a,$b,$c,$d) {
            fn make(self) -> mat2x2 {
                let (a,b,c,d) = self;
                TMat2 { elems: [[a as f32,b as f32],
                                [c as f32,d as f32]] }
            }
        }
        impl Mat2x2Args for (($a,$b),$c,$d) {
            fn make(self) -> mat2x2 {
                let ((a,b),c,d) = self;
                TMat2 { elems: [[a as f32,b as f32],
                                [c as f32,d as f32]] }
            }
        }
        impl Mat2x2Args for ($a,$b,($c,$d)) {
            fn make(self) -> mat2x2 {
                let (a,b,(c,d)) = self;
                TMat2 { elems: [[a as f32,b as f32],
                                [c as f32,d as f32]] }
            }
        }
        impl Mat2x2Args for (($a,$b),($c,$d)) {
            fn make(self) -> mat2x2 {
                let ((a,b),(c,d)) = self;
                TMat2 { elems: [[a as f32,b as f32],
                                [c as f32,d as f32]] }
            }
        }
    };
    ($a:ident 2,$b:ident,$c:ident) => {
        impl Mat2x2Args for ($a,$b,$c) {
            fn make(self) -> mat2x2 {
                let (a,b,c) = self;
                TMat2 { elems: [[a.x as f32,a.y as f32],
                                [b as f32,  c as f32]] }
            }
        }
        impl Mat2x2Args for ($a,($b,$c)) {
            fn make(self) -> mat2x2 {
                let (a,(b,c)) = self;
                TMat2 { elems: [[a.x as f32,a.y as f32],
                                [b as f32,  c as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,$c:ident 2) => {
        impl Mat2x2Args for ($a,$b,$c) {
            fn make(self) -> mat2x2 {
                let (a,b,c) = self;
                TMat2 { elems: [[a as f32,  b as f32],
                                [c.x as f32,c.y as f32]] }
            }
        }
        impl Mat2x2Args for (($a,$b),$c) {
            fn make(self) -> mat2x2 {
                let ((a,b),c) = self;
                TMat2 { elems: [[a as f32,  b as f32],
                                [c.x as f32,c.y as f32]] }
            }
        }
    };
    ($a:ident 2,$b:ident 2) => {
        impl Mat2x2Args for ($a,$b) {
            fn make(self) -> mat2x2 {
                let (a,b) = self;
                TMat2 { elems: [[a.x as f32,a.y as f32],
                                [b.x as f32,b.y as f32]] }
            }
        }
    };
}

macro_rules! impl_Mat2x3Args_for {
    ($a:ident copy) => {
        impl Mat2x3Args for $a {
            fn make(self) -> mat2x3 {
                let x = self;
                TMat2x3 { elems: [[x as f32,x as f32,x as f32],
                                  [x as f32,x as f32,x as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,$c:ident,
     $d:ident,$e:ident,$f:ident) => {
        impl Mat2x3Args for ($a,$b,$c,
                             $d,$e,$f) {
            fn make(self) -> mat2x3 {
                let (a,b,c,d,e,f) = self;
                TMat2x3 { elems: [[a as f32,b as f32,c as f32],
                                  [d as f32,e as f32,f as f32]] }
            }
        }
        impl Mat2x3Args for (($a,$b,$c),
                             ($d,$e,$f)) {
            fn make(self) -> mat2x3 {
                let ((a,b,c),(d,e,f)) = self;
                TMat2x3 { elems: [[a as f32,b as f32,c as f32],
                                  [d as f32,e as f32,f as f32]] }
            }
        }
        impl Mat2x3Args for ($a,$b,$c,
                             ($d,$e,$f)) {
            fn make(self) -> mat2x3 {
                let (a,b,c,(d,e,f)) = self;
                TMat2x3 { elems: [[a as f32,b as f32,c as f32],
                                  [d as f32,e as f32,f as f32]] }
            }
        }
        impl Mat2x3Args for (($a,$b,$c),
                             $d,$e,$f) {
            fn make(self) -> mat2x3 {
                let ((a,b,c),d,e,f) = self;
                TMat2x3 { elems: [[a as f32,b as f32,c as f32],
                                  [d as f32,e as f32,f as f32]] }
            }
        }
    };
    ($a:ident 3,
     $d:ident,$e:ident,$f:ident) => {
        impl Mat2x3Args for ($a,
                             $d,$e,$f) {
            fn make(self) -> mat2x3 {
                let (a,
                     d,e,f) = self;
                TMat2x3 { elems: [[a.x as f32,a.y as f32,a.z as f32],
                                  [d as f32,e as f32,f as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,$c:ident,
     $d:ident 3) => {
        impl Mat2x3Args for ($a,$b,$c,
                             $d) {
            fn make(self) -> mat2x3 {
                let (a,b,c,
                     d) = self;
                TMat2x3 { elems: [[a as f32,b as f32,c as f32],
                                  [d.x as f32,d.y as f32,d.z as f32]] }
            }
        }
    };
    ($a:ident 3,
     $d:ident,$e:ident,$f:ident) => {
        impl Mat2x3Args for ($a,
                             $d,$e,$f) {
            fn make(self) -> mat2x3 {
                let (a,
                     d,e,f) = self;
                TMat2x3 { elems: [[a.x as f32,a.y as f32,a.z as f32],
                                  [d as f32,e as f32,f as f32]] }
            }
        }
    };
    ($a:ident 3,
     $d:ident 3) => {
        impl Mat2x3Args for ($a,
                             $d) {
            fn make(self) -> mat2x3 {
                let (a,
                     d) = self;
                TMat2x3 { elems: [[a.x as f32,a.y as f32,a.z as f32],
                                  [d.x as f32,d.y as f32,d.z as f32]] }
            }
        }
    };

}

macro_rules! impl_Mat3x2Args_for {
    ($a:ident copy) => {
        impl Mat3x2Args for $a {
            fn make(self) -> mat3x2 {
                let x = self;
                TMat3x2 { elems: [[x as f32,x as f32],
                                  [x as f32,x as f32],
                                  [x as f32,x as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,
     $c:ident,$d:ident,
     $e:ident,$f:ident) => {
        impl Mat3x2Args for ($a,$b,
                             $c,$d,
                             $e,$f) {
            fn make(self) -> mat3x2 {
                let (a,b,c,d,e,f) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
        impl Mat3x2Args for (($a,$b),
                             $c,$d,
                             $e,$f) {
            fn make(self) -> mat3x2 {
                let ((a,b),c,d,e,f) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
        impl Mat3x2Args for ($a,$b,
                             ($c,$d),
                             $e,$f) {
            fn make(self) -> mat3x2 {
                let (a,b,(c,d),e,f) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
        impl Mat3x2Args for ($a,$b,
                             $c,$d,
                             ($e,$f)) {
            fn make(self) -> mat3x2 {
                let (a,b,c,d,(e,f)) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
        impl Mat3x2Args for (($a,$b),
                             ($c,$d),
                             $e,$f) {
            fn make(self) -> mat3x2 {
                let ((a,b),(c,d),e,f) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
        impl Mat3x2Args for (($a,$b),
                             $c,$d,
                             ($e,$f)) {
            fn make(self) -> mat3x2 {
                let ((a,b),c,d,(e,f)) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
        impl Mat3x2Args for ($a,$b,
                             ($c,$d),
                             ($e,$f)) {
            fn make(self) -> mat3x2 {
                let (a,b,(c,d),(e,f)) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
        impl Mat3x2Args for (($a,$b),
                             ($c,$d),
                             ($e,$f)) {
            fn make(self) -> mat3x2 {
                let ((a,b),(c,d),(e,f)) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
    };
    ($a:ident 2,
     $c:ident,$d:ident,
     $e:ident,$f:ident) => {
        impl Mat3x2Args for ($a,
                             $c,$d,
                             $e,$f) {
            fn make(self) -> mat3x2 {
                let (a,  c,d,e,f) = self;
                TMat3x2 { elems: [[a.x as f32,a.y as f32],
                                  [c as f32,d as f32],
                                  [e as f32,f as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,
     $c:ident 2,
     $e:ident,$f:ident) => {
        impl Mat3x2Args for ($a,$b,
                             $c,
                             $e,$f) {
            fn make(self) -> mat3x2 {
                let (a,b,c  ,e,f) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c.x as f32,c.y as f32],
                                  [e as f32,f as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,
     $c:ident,$d:ident,
     $e:ident 2) => {
        impl Mat3x2Args for ($a,$b,
                             $c,$d,
                             $e) {
            fn make(self) -> mat3x2 {
                let (a,b,c,d,e  ) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c as f32,d as f32],
                                  [e.x as f32,e.y as f32]] }
            }
        }
    };
    ($a:ident 2,
     $c:ident,$d:ident,
     $e:ident 2) => {
        impl Mat3x2Args for ($a,
                             $c,$d,
                             $e) {
            fn make(self) -> mat3x2 {
                let (a  ,c,d,e  ) = self;
                TMat3x2 { elems: [[a.x as f32,a.y as f32],
                                  [c as f32,d as f32],
                                  [e.x as f32,e.y as f32]] }
            }
        }
    };
    ($a:ident 2,
     $c:ident 2,
     $e:ident,$f:ident) => {
        impl Mat3x2Args for ($a,
                             $c,
                             $e,$f) {
            fn make(self) -> mat3x2 {
                let (a,  c,  e,f) = self;
                TMat3x2 { elems: [[a.x as f32,a.y as f32],
                                  [c.x as f32,c.y as f32],
                                  [e as f32,f as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,
     $c:ident 2,
     $e:ident 2) => {
        impl Mat3x2Args for ($a,$b,
                             $c,
                             $e) {
            fn make(self) -> mat3x2 {
                let (a,b,c  ,e  ) = self;
                TMat3x2 { elems: [[a as f32,b as f32],
                                  [c.x as f32,c.y as f32],
                                  [e.x as f32,e.y as f32]] }
            }
        }
    };
    ($a:ident 2,
     $c:ident 2,
     $e:ident 2) => {
        impl Mat3x2Args for ($a,
                             $c,
                             $e) {
            fn make(self) -> mat3x2 {
                let (a  ,c  ,e  ) = self;
                TMat3x2 { elems: [[a.x as f32,a.y as f32],
                                  [c.x as f32,c.y as f32],
                                  [e.x as f32,e.y as f32]] }
            }
        }
    };
}


impl_Mat2x2Args_for!(f32 copy)
impl_Mat2x2Args_for!(int copy)

macro_rules! impl_Mat2x2Args_for_choice {
    ( $a:ident, $b:ident, $c:ident, $d:ident, $($ignore:ident),*) => {
        impl_Mat2x2Args_for!($a, $b, $c, $d)
    }
}

all_choices!( impl_Mat2x2Args_for_choice :
              todo: { (int | f32) (int | f32)
                      (int | f32) (int | f32) }
              done: { (ignored) } )

impl_Mat2x2Args_for!(vec2 2,vec2 2)


macro_rules! impl_Mat2x3Args_for_choice {
    ( $a:ident, $b:ident, $c:ident, $d:ident, $e:ident, $f:ident, $($ignore:ident),*) => {
        impl_Mat2x3Args_for!($a, $b, $c, $d, $e, $f)
    }
}

all_choices!( impl_Mat2x3Args_for_choice :
              todo: { (int | f32) (int | f32) (int | f32)
                      (int | f32) (int | f32) (int | f32) }
              done: { (ignored) } )

impl_Mat2x3Args_for!(f32 copy)
impl_Mat2x3Args_for!(int copy)

impl_Mat2x3Args_for!(vec3 3,vec3 3)
impl_Mat2x3Args_for!(vec3 3,f32,f32,f32)
impl_Mat2x3Args_for!(vec3 3,f32,f32,int)
impl_Mat2x3Args_for!(vec3 3,f32,int,f32)
impl_Mat2x3Args_for!(vec3 3,f32,int,int)
impl_Mat2x3Args_for!(vec3 3,int,f32,f32)
impl_Mat2x3Args_for!(vec3 3,int,f32,int)
impl_Mat2x3Args_for!(vec3 3,int,int,f32)
impl_Mat2x3Args_for!(vec3 3,int,int,int)
impl_Mat2x3Args_for!(f32,f32,f32,vec3 3)
impl_Mat2x3Args_for!(f32,f32,int,vec3 3)
impl_Mat2x3Args_for!(f32,int,f32,vec3 3)
impl_Mat2x3Args_for!(f32,int,int,vec3 3)
impl_Mat2x3Args_for!(int,f32,f32,vec3 3)
impl_Mat2x3Args_for!(int,f32,int,vec3 3)
impl_Mat2x3Args_for!(int,int,f32,vec3 3)
impl_Mat2x3Args_for!(int,int,int,vec3 3)

macro_rules! impl_Mat2x4Args_for {
    ($a:ident copy) => {
        impl Mat2x4Args for $a {
            fn make(self) -> mat2x4 {
                let x = self;
                TMat2x4 { elems: [[x as f32,x as f32,x as f32,x as f32],
                                  [x as f32,x as f32,x as f32,x as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,$c:ident,$d:ident,
     $e:ident,$f:ident,$g:ident,$h:ident) => {
        impl Mat2x4Args for ($a,$b,$c,$d,
                             $e,$f,$g,$h) {
            fn make(self) -> mat2x4 {
                let (a,b,c,d,
                     e,f,g,h) = self;
                TMat2x4 { elems: [[a as f32,b as f32,c as f32,d as f32],
                                  [e as f32,f as f32,g as f32,h as f32]] }
            }
        }
        impl Mat2x4Args for (($a,$b,$c,$d),
                             $e,$f,$g,$h) {
            fn make(self) -> mat2x4 {
                let ((a,b,c,d),
                     e,f,g,h) = self;
                TMat2x4 { elems: [[a as f32,b as f32,c as f32,d as f32],
                                  [e as f32,f as f32,g as f32,h as f32]] }
            }
        }
        impl Mat2x4Args for ($a,$b,$c,$d,
                             ($e,$f,$g,$h)) {
            fn make(self) -> mat2x4 {
                let (a,b,c,d,
                     (e,f,g,h)) = self;
                TMat2x4 { elems: [[a as f32,b as f32,c as f32,d as f32],
                                  [e as f32,f as f32,g as f32,h as f32]] }
            }
        }
        impl Mat2x4Args for (($a,$b,$c,$d),
                             ($e,$f,$g,$h)) {
            fn make(self) -> mat2x4 {
                let ((a,b,c,d),
                     (e,f,g,h)) = self;
                TMat2x4 { elems: [[a as f32,b as f32,c as f32,d as f32],
                                  [e as f32,f as f32,g as f32,h as f32]] }
            }
        }
    };
    ($a:ident 4,
     $e:ident,$f:ident,$g:ident,$h:ident) => {
        impl Mat2x4Args for ($a,
                             $e,$f,$g,$h) {
            fn make(self) -> mat2x4 {
                let (a,
                     e,f,g,h) = self;
                TMat2x4 { elems: [[a.x as f32,a.y as f32,a.z as f32,a.w as f32],
                                  [e as f32,f as f32,g as f32,h as f32]] }
            }
        }
        impl Mat2x4Args for ($a,
                             ($e,$f,$g,$h)) {
            fn make(self) -> mat2x4 {
                let (a,
                     (e,f,g,h)) = self;
                TMat2x4 { elems: [[a.x as f32,a.y as f32,a.z as f32,a.w as f32],
                                  [e as f32,f as f32,g as f32,h as f32]] }
            }
        }
    };
    ($a:ident,$b:ident,$c:ident,$d:ident,
     $e:ident 4) => {
        impl Mat2x4Args for ($a,$b,$c,$d,
                             $e) {
            fn make(self) -> mat2x4 {
                let (a,b,c,d,
                     e) = self;
                TMat2x4 { elems: [[a as f32,b as f32,c as f32,d as f32],
                                  [e.x as f32,e.y as f32,e.z as f32,e.w as f32]] }
            }
        }
        impl Mat2x4Args for (($a,$b,$c,$d),
                             $e) {
            fn make(self) -> mat2x4 {
                let ((a,b,c,d),
                     e) = self;
                TMat2x4 { elems: [[a as f32,b as f32,c as f32,d as f32],
                                  [e.x as f32,e.y as f32,e.z as f32,e.w as f32]] }
            }
        }
    };
    ($a:ident 4,
     $e:ident 4) => {
        impl Mat2x4Args for ($a,
                             $e) {
            fn make(self) -> mat2x4 {
                let (a,
                     e) = self;
                TMat2x4 { elems: [[a.x as f32,a.y as f32,a.z as f32,a.w as f32],
                                  [e.x as f32,e.y as f32,e.z as f32,e.w as f32]] }
            }
        }
    };
}

macro_rules! impl_Mat2x4Args_for_choice {
    ( $a:ident, $b:ident, $c:ident, $d:ident,
      $e:ident, $f:ident, $g:ident, $h:ident, $($ignore:ident),*) => {
        impl_Mat2x4Args_for!($a, $b, $c, $d, $e, $f, $g, $h)
    }
}

impl_Mat2x4Args_for!(f32 copy)
impl_Mat2x4Args_for!(int copy)

all_choices!(impl_Mat2x4Args_for_choice :
             todo: {  (int | f32) (int | f32) (int | f32) (int | f32)
                      (int | f32) (int | f32) (int | f32) (int | f32) }
             done: { (ignored) } )

impl_Mat2x4Args_for!(vec4 4,vec4 4)

macro_rules! impl_Mat3x2Args_for_choice {
    ( $a:ident, $b:ident,
      $c:ident, $d:ident,
      $e:ident, $f:ident, $($ignore:ident),*) => {
        impl_Mat3x2Args_for!($a, $b, $c, $d, $e, $f)
    }
}

impl_Mat3x2Args_for!(f32 copy)
impl_Mat3x2Args_for!(int copy)

all_choices!(impl_Mat3x2Args_for_choice :
             todo: {  (int | f32) (int | f32)
                      (int | f32) (int | f32)
                      (int | f32) (int | f32) }
             done: { (ignored) } )


macro_rules! impl_Mat3x2Args_for_choice_1st_vec2 {
    ( $c:ident, $d:ident, $e:ident, $f:ident, $($ignore:ident),*) => {
        impl_Mat3x2Args_for!(vec2 2, $c, $d, $e, $f)
    }
}
macro_rules! impl_Mat3x2Args_for_choice_2nd_vec2 {
    ( $a:ident, $b:ident, $e:ident, $f:ident, $($ignore:ident),*) => {
        impl_Mat3x2Args_for!($a, $b, vec2 2, $e, $f)
    }
}
macro_rules! impl_Mat3x2Args_for_choice_3rd_vec2 {
    ( $a:ident, $b:ident, $c:ident, $d:ident, $($ignore:ident),*) => {
        impl_Mat3x2Args_for!($a, $b, $c, $d, vec2 2)
    }
}

all_choices!(impl_Mat3x2Args_for_choice_1st_vec2 :
             todo: {  (int | f32) (int | f32) (int | f32) (int | f32) }
             done: { (ignored) } )
all_choices!(impl_Mat3x2Args_for_choice_2nd_vec2 :
             todo: {  (int | f32) (int | f32) (int | f32) (int | f32) }
             done: { (ignored) } )
all_choices!(impl_Mat3x2Args_for_choice_3rd_vec2 :
             todo: {  (int | f32) (int | f32) (int | f32) (int | f32) }
             done: { (ignored) } )

impl_Mat3x2Args_for!(vec2 2, vec2 2, f32, f32)
impl_Mat3x2Args_for!(vec2 2, vec2 2, f32, int)
impl_Mat3x2Args_for!(vec2 2, vec2 2, int, f32)
impl_Mat3x2Args_for!(vec2 2, vec2 2, int, int)
impl_Mat3x2Args_for!(f32, f32, vec2 2, vec2 2)
impl_Mat3x2Args_for!(f32, int, vec2 2, vec2 2)
impl_Mat3x2Args_for!(int, f32, vec2 2, vec2 2)
impl_Mat3x2Args_for!(int, int, vec2 2, vec2 2)
impl_Mat3x2Args_for!(vec2 2, f32, f32, vec2 2)
impl_Mat3x2Args_for!(vec2 2, f32, int, vec2 2)
impl_Mat3x2Args_for!(vec2 2, int, f32, vec2 2)
impl_Mat3x2Args_for!(vec2 2, int, int, vec2 2)
impl_Mat3x2Args_for!(vec2 2, vec2 2, vec2 2)

double_dispatch_T!{Mul for TMat2   mul via TMat2x2MulRHS rev_mul}
double_dispatch_T!{Div for TMat2   div via TMat2x2DivRHS rev_div}
double_dispatch_T!{Mul for TMat2x3 mul via TMat2x3MulRHS rev_mul}
double_dispatch_T!{Div for TMat2x3 div via TMat2x3DivRHS rev_div}
double_dispatch_T!{Mul for TMat2x4 mul via TMat2x4MulRHS rev_mul}
double_dispatch_T!{Div for TMat2x4 div via TMat2x4DivRHS rev_div}
double_dispatch_T!{Mul for TMat3   mul via TMat3x3MulRHS rev_mul}
double_dispatch_T!{Div for TMat3   div via TMat3x3DivRHS rev_div}
double_dispatch_T!{Mul for TMat3x2 mul via TMat3x2MulRHS rev_mul}
double_dispatch_T!{Div for TMat3x2 div via TMat3x2DivRHS rev_div}
double_dispatch_T!{Mul for TMat3x4 mul via TMat3x4MulRHS rev_mul}
double_dispatch_T!{Div for TMat3x4 div via TMat3x4DivRHS rev_div}
double_dispatch_T!{Mul for TMat4   mul via TMat4x4MulRHS rev_mul}
double_dispatch_T!{Div for TMat4   div via TMat4x4DivRHS rev_div}
double_dispatch_T!{Mul for TMat4x2 mul via TMat4x2MulRHS rev_mul}
double_dispatch_T!{Div for TMat4x2 div via TMat4x2DivRHS rev_div}
double_dispatch_T!{Mul for TMat4x3 mul via TMat4x3MulRHS rev_mul}
double_dispatch_T!{Div for TMat4x3 div via TMat4x3DivRHS rev_div}

macro_rules! impl_Scalar_MulDiv_for {
    ($TMat:ident $TMatMulRHS:ident $TMatDivRHS:ident) => {

        impl<T:Num> $TMatMulRHS<T,$TMat<T>> for S<T> {
            fn rev_mul(&self, lhs: &$TMat<T>) -> $TMat<T> {
                let S(ref f) = *self;
                lhs.map(|x|*x * *f)
            }
        }

        impl<T:Num> $TMatDivRHS<T,$TMat<T>> for S<T> {
            fn rev_div(&self, lhs: &$TMat<T>) -> $TMat<T> {
                let S(ref f) = *self;
                lhs.map(|x|*x / *f)
            }
        }

        impl<T:Num> SMulRHS<T,$TMat<T>> for $TMat<T> {
            fn rev_mul(&self, lhs: &S<T>) -> $TMat<T> {
                let S(ref f) = *lhs;
                self.map(|x|*f * *x)
            }
        }

        impl<T:Num> SDivRHS<T,$TMat<T>> for $TMat<T> {
            fn rev_div(&self, lhs: &S<T>) -> $TMat<T> {
                let S(ref f) = *lhs;
                self.map(|x|*f / *x)
            }
        }
    }
}

impl_Scalar_MulDiv_for!(TMat2 TMat2x2MulRHS TMat2x2DivRHS)
impl_Scalar_MulDiv_for!(TMat2x3 TMat2x3MulRHS TMat2x3DivRHS)
impl_Scalar_MulDiv_for!(TMat2x4 TMat2x4MulRHS TMat2x4DivRHS)
impl_Scalar_MulDiv_for!(TMat3x2 TMat3x2MulRHS TMat3x2DivRHS)

macro_rules! impl_MulRHS_for {
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 2 2 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat2<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat2<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();

                TMat2 { elems: [[c00, c01],
                                [c10, c11]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 2 3 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat2x3<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat2x3<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();
                let c02 = (a.row(2) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();
                let c12 = (a.row(2) * b.col(1)).sum();

                TMat2x3 { elems: [[c00, c01, c02],
                                  [c10, c11, c12]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 2 4 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat2x4<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat2x4<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();
                let c02 = (a.row(2) * b.col(0)).sum();
                let c03 = (a.row(3) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();
                let c12 = (a.row(2) * b.col(1)).sum();
                let c13 = (a.row(3) * b.col(1)).sum();

                TMat2x4 { elems: [[c00, c01, c02, c03],
                                  [c10, c11, c12, c13]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 3 2 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat3x2<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat3x2<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();

                let c20 = (a.row(0) * b.col(2)).sum();
                let c21 = (a.row(1) * b.col(2)).sum();

                TMat3x2 { elems: [[c00, c01],
                                  [c10, c11],
                                  [c20, c21]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 3 3 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat3<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat3<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();
                let c02 = (a.row(2) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();
                let c12 = (a.row(2) * b.col(1)).sum();

                let c20 = (a.row(0) * b.col(2)).sum();
                let c21 = (a.row(1) * b.col(2)).sum();
                let c22 = (a.row(2) * b.col(2)).sum();

                TMat3 { elems: [[c00, c01, c02],
                                [c10, c11, c12],
                                [c20, c21, c22]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 3 4 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat3x4<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat3x4<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();
                let c02 = (a.row(2) * b.col(0)).sum();
                let c03 = (a.row(3) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();
                let c12 = (a.row(2) * b.col(1)).sum();
                let c13 = (a.row(3) * b.col(1)).sum();

                let c20 = (a.row(0) * b.col(2)).sum();
                let c21 = (a.row(1) * b.col(2)).sum();
                let c22 = (a.row(2) * b.col(2)).sum();
                let c23 = (a.row(3) * b.col(2)).sum();

                TMat3x4 { elems: [[c00, c01, c02, c03],
                                  [c10, c11, c12, c13],
                                  [c20, c21, c22, c23]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 4 2 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat4x2<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat4x2<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();

                let c20 = (a.row(0) * b.col(2)).sum();
                let c21 = (a.row(1) * b.col(2)).sum();

                let c30 = (a.row(0) * b.col(3)).sum();
                let c31 = (a.row(1) * b.col(3)).sum();

                TMat4x2 { elems: [[c00, c01],
                                  [c10, c11],
                                  [c20, c21],
                                  [c30, c31]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 4 3 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat4x3<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat4x3<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();
                let c02 = (a.row(2) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();
                let c12 = (a.row(2) * b.col(1)).sum();

                let c20 = (a.row(0) * b.col(2)).sum();
                let c21 = (a.row(1) * b.col(2)).sum();
                let c22 = (a.row(2) * b.col(2)).sum();

                let c30 = (a.row(0) * b.col(3)).sum();
                let c31 = (a.row(1) * b.col(3)).sum();
                let c32 = (a.row(2) * b.col(3)).sum();

                TMat4x3 { elems: [[c00, c01, c02],
                                  [c10, c11, c12],
                                  [c20, c21, c22],
                                  [c30, c31, c32]] }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TMatRHS:ident 4 4 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TMat4<T>> for $TMatRHS<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TMat4<T> {
                let a = lhs;
                let b = self;
                let c00 = (a.row(0) * b.col(0)).sum();
                let c01 = (a.row(1) * b.col(0)).sum();
                let c02 = (a.row(2) * b.col(0)).sum();
                let c03 = (a.row(3) * b.col(0)).sum();

                let c10 = (a.row(0) * b.col(1)).sum();
                let c11 = (a.row(1) * b.col(1)).sum();
                let c12 = (a.row(2) * b.col(1)).sum();
                let c13 = (a.row(3) * b.col(1)).sum();

                let c20 = (a.row(0) * b.col(2)).sum();
                let c21 = (a.row(1) * b.col(2)).sum();
                let c22 = (a.row(2) * b.col(2)).sum();
                let c23 = (a.row(3) * b.col(2)).sum();

                let c30 = (a.row(0) * b.col(3)).sum();
                let c31 = (a.row(1) * b.col(3)).sum();
                let c32 = (a.row(2) * b.col(3)).sum();
                let c33 = (a.row(3) * b.col(3)).sum();

                TMat4 { elems: [[c00, c01, c02, c03],
                                [c10, c11, c12, c13],
                                [c20, c21, c22, c23],
                                [c30, c31, c32, c33]] }
            }
        }
    };
}

impl_MulRHS_for!( TMat2   TMat2x2MulRHS TMat2   2 2 )
impl_MulRHS_for!( TMat2   TMat2x2MulRHS TMat3x2 3 2 )
impl_MulRHS_for!( TMat2   TMat2x2MulRHS TMat4x2 4 2 )

impl_MulRHS_for!( TMat2x3 TMat2x3MulRHS TMat2   2 3 )
impl_MulRHS_for!( TMat2x3 TMat2x3MulRHS TMat3x2 3 3 )
impl_MulRHS_for!( TMat2x3 TMat2x3MulRHS TMat4x2 4 3 )

impl_MulRHS_for!( TMat2x4 TMat2x4MulRHS TMat2   2 4 )
impl_MulRHS_for!( TMat2x4 TMat2x4MulRHS TMat3x2 3 4 )
impl_MulRHS_for!( TMat2x4 TMat2x4MulRHS TMat4x2 4 4 )

impl_MulRHS_for!( TMat3x2 TMat3x2MulRHS TMat2x3 2 2 )
impl_MulRHS_for!( TMat3x2 TMat3x2MulRHS TMat3   3 2 )
impl_MulRHS_for!( TMat3x2 TMat3x2MulRHS TMat4x3 4 2 )

impl_MulRHS_for!( TMat3   TMat3x3MulRHS TMat2x3 2 3 )
impl_MulRHS_for!( TMat3   TMat3x3MulRHS TMat3   3 3 )
impl_MulRHS_for!( TMat3   TMat3x3MulRHS TMat4x3 4 3 )

impl_MulRHS_for!( TMat3x4 TMat3x4MulRHS TMat2x3 2 4 )
impl_MulRHS_for!( TMat3x4 TMat3x4MulRHS TMat3   3 4 )
impl_MulRHS_for!( TMat3x4 TMat3x4MulRHS TMat4x3 4 4 )

impl_MulRHS_for!( TMat4x2 TMat4x2MulRHS TMat2x4 2 2 )
impl_MulRHS_for!( TMat4x2 TMat4x2MulRHS TMat3x4 3 2 )
impl_MulRHS_for!( TMat4x2 TMat4x2MulRHS TMat4   4 2 )

impl_MulRHS_for!( TMat4x3 TMat4x3MulRHS TMat2x4 2 3 )
impl_MulRHS_for!( TMat4x3 TMat4x3MulRHS TMat3x4 3 3 )
impl_MulRHS_for!( TMat4x3 TMat4x3MulRHS TMat4   4 3 )

impl_MulRHS_for!( TMat4   TMat4x4MulRHS TMat2x4 2 4 )
impl_MulRHS_for!( TMat4   TMat4x4MulRHS TMat3x4 3 4 )
impl_MulRHS_for!( TMat4   TMat4x4MulRHS TMat4   4 4 )

macro_rules! impl_mat_mul_vec_for {
    ( $TMatLHS:ident $TMatMul:ident $TVec:ident 2 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TVec2<T>> for $TVec<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TVec2<T> {
                TVec2{ x: (lhs.row(0) * *self).sum(),
                       y: (lhs.row(1) * *self).sum(), }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TVec:ident 3 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TVec3<T>> for $TVec<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TVec3<T> {
                TVec3{ x: (lhs.row(0) * *self).sum(),
                       y: (lhs.row(1) * *self).sum(),
                       z: (lhs.row(2) * *self).sum(), }
            }
        }
    };
    ( $TMatLHS:ident $TMatMul:ident $TVec:ident 4 ) =>
    {
        impl<T:Num+Clone> $TMatMul<T,TVec4<T>> for $TVec<T> {
            fn rev_mul(&self, lhs: &$TMatLHS<T>) -> TVec4<T> {
                TVec4{ x: (lhs.row(0) * *self).sum(),
                       y: (lhs.row(1) * *self).sum(),
                       z: (lhs.row(2) * *self).sum(),
                       w: (lhs.row(3) * *self).sum(), }
            }
        }
    };
}

macro_rules! impl_vec_mul_mat_for {
    ( $TVecLHS:ident $TVecMul:ident $TMat:ident 2 ) =>
    {
        impl<T:Num+Clone> $TVecMul<T,TVec2<T>> for $TMat<T> {
            fn rev_mul(&self, lhs: &$TVecLHS<T>) -> TVec2<T> {
                TVec2{ x: (lhs * self.col(0)).sum(),
                       y: (lhs * self.col(1)).sum(), }
            }
        }
    };
    ( $TVecLHS:ident $TVecMul:ident $TMat:ident 3 ) =>
    {
        impl<T:Num+Clone> $TVecMul<T,TVec3<T>> for $TMat<T> {
            fn rev_mul(&self, lhs: &$TVecLHS<T>) -> TVec3<T> {
                TVec3{ x: (*lhs * self.col(0)).sum(),
                       y: (*lhs * self.col(1)).sum(),
                       z: (*lhs * self.col(2)).sum(), }
            }
        }
    };
    ( $TVecLHS:ident $TVecMul:ident $TMat:ident 4 ) =>
    {
        impl<T:Num+Clone> $TVecMul<T,TVec4<T>> for $TMat<T> {
            fn rev_mul(&self, lhs: &$TVecLHS<T>) -> TVec4<T> {
                TVec4{ x: (*lhs * self.col(0)).sum(),
                       y: (*lhs * self.col(1)).sum(),
                       z: (*lhs * self.col(2)).sum(),
                       w: (*lhs * self.col(3)).sum(), }
            }
        }
    };
}

impl_mat_mul_vec_for!(TMat2   TMat2x2MulRHS TVec2 2)
impl_mat_mul_vec_for!(TMat2x3 TMat2x3MulRHS TVec2 3)
impl_mat_mul_vec_for!(TMat2x4 TMat2x4MulRHS TVec2 4)

impl_mat_mul_vec_for!(TMat3x2 TMat3x2MulRHS TVec3 2)
impl_mat_mul_vec_for!(TMat3   TMat3x3MulRHS TVec3 3)
impl_mat_mul_vec_for!(TMat3x4 TMat3x4MulRHS TVec3 4)

impl_mat_mul_vec_for!(TMat4x2 TMat4x2MulRHS TVec4 2)
impl_mat_mul_vec_for!(TMat4x3 TMat4x3MulRHS TVec4 3)
impl_mat_mul_vec_for!(TMat4   TMat4x4MulRHS TVec4 4)

impl_vec_mul_mat_for!(TVec2 TVec2MulRHS TMat2   2)
impl_vec_mul_mat_for!(TVec2 TVec2MulRHS TMat3x2 3)
impl_vec_mul_mat_for!(TVec2 TVec2MulRHS TMat4x2 4)

impl_vec_mul_mat_for!(TVec3 TVec3MulRHS TMat2x3 2)
impl_vec_mul_mat_for!(TVec3 TVec3MulRHS TMat3   3)
impl_vec_mul_mat_for!(TVec3 TVec3MulRHS TMat4x3 4)

impl_vec_mul_mat_for!(TVec4 TVec4MulRHS TMat2x4 2)
impl_vec_mul_mat_for!(TVec4 TVec4MulRHS TMat3x4 3)
impl_vec_mul_mat_for!(TVec4 TVec4MulRHS TMat4   4)

#[cfg(test)]
mod mat2x2_tests {
    #![allow(uppercase_variables)]

    use super::{mat2x2,mat2,mat2x3};
    use super::{inverse};
    use super::{RowsData,ColumnsData};

    use src::operators::{EpsilonEq};
    use src::typedefs::{vec2};
    use src::vector::{vec2,vec3};
    use src::scalar::S;

    #[test]
    fn basics() {
        let m123_456 = mat2x3(((1,2,3),
                               (4,5,6)));
        assert_eq!(m123_456.row(0), vec2((1,4)));
        assert_eq!(m123_456.row(1), vec2((2,5)));
        assert_eq!(m123_456.row(2), vec2((3,6)));

        assert_eq!(m123_456.col(0), vec3((1,2,3)));
        assert_eq!(m123_456.col(1), vec3((4,5,6)));
    }

    #[test]
    fn test_operators() {
        let l = mat2x2(1.0f32);
        let m = mat2x2(1.0f32);
        let u = vec2(1.0f32);
        let v = vec2(1.0f32);
        let x = S(1.0f32);

        let a : vec2 = m * u;
        let b : vec2 = v * m;
        let n : mat2x2 = x / m;
        let o : mat2x2 = m / x;
        let p : mat2x2 = x * m;
        let q : mat2x2 = m * x;

        let _ = (a,b,n,o,p);
        assert_eq!(m, q);
        assert_eq!(m, l);
    }

    #[test]
    fn test_inverse() {
        let Matrix = mat2((1, 2, 3, 4));
        let Inverse = inverse(&Matrix);
        let Identity = Matrix * Inverse;

        assert!(Identity.row(0).epsilon_eq(&vec2((1f32, 0f32)), &vec2(0.01f32)));
        assert!(Identity.row(1).epsilon_eq(&vec2((0f32, 1f32)), &vec2(0.01f32)));
    }

    #[test]
    fn test_ctr() {
        let m0 = mat2x2((vec2((0,1)),
                         vec2((2, 3))));
        let m1 = mat2x2((0, 1,
                         2, 3));
        let m2 = mat2x2(((0, 1),
                         (2, 3)));
        assert_eq!(m0, m2);
        assert_eq!(m1, m2);
    }
}

#[cfg(test)]
mod mat2x3_tests {
    use super::{mat2x3};
    use src::scalar::{S};
    use src::typedefs::{vec2,vec3};
    use src::vector::{vec2,vec3};

    #[test]
    fn test_operators() {
        let l = mat2x3(1.0f32);
        let m = mat2x3(1.0f32);
        let u = vec2(1.0f32);
        let v = vec3(1.0f32);
        let x = S(1.0f32);

        let a : vec3 = m * u;
        let b : vec2 = v * m;
        let n : mat2x3 = x / m;
        let o : mat2x3 = m / x;
        let p : mat2x3 = x * m;
        let q : mat2x3 = m * x;

        let _ = (a,b,n,o,p);
        assert_eq!(m, q);
        assert_eq!(m, l);
    }

    #[test]
    fn test_ctr() {
        let m0 = mat2x3((vec3((0, 1, 2)),
                         vec3((3, 4, 5))));
        let m1 = mat2x3((0, 1, 2,
                         3, 4, 5));
        let m2 = mat2x3(((0, 1, 2),
                         (3, 4, 5)));
        assert_eq!(m0, m2);
        assert_eq!(m1, m2);
    }
}

#[cfg(test)]
mod mat2x4_tests {
    use super::{mat2x4};

    use src::scalar::S;
    use src::typedefs::{vec2,vec4};
    use src::vector::{vec2,vec4};

    #[test]
    fn test_operators() {
        let l = mat2x4(1.0f32);
        let m = mat2x4(1.0f32);
        let u = vec2(1.0f32);
        let v = vec4(1.0f32);
        let x = S(1.0f32);
        let a : vec4 = m * u;
        let b : vec2 = v * m;
        let n : mat2x4 = x / m;
        let o : mat2x4 = m / x;
        let p : mat2x4 = x * m;
        let q : mat2x4 = m * x;
        let _ = (a,b,n,o,p);
        assert_eq!(m, q);
        assert_eq!(m, l);
    }

    #[test]
    fn test_ctr() {
        let m0 = mat2x4((vec4((0, 1, 2, 3)),
                         vec4((4, 5, 6, 7))));
        let m1 = mat2x4((0, 1, 2, 3,
                         4, 5, 6, 7));
        let m2 = mat2x4(((0, 1, 2, 3),
                         (4, 5, 6, 7)));

        assert_eq!(m0, m2);
        assert_eq!(m1, m2);
    }
}

#[cfg(test)]
mod mat3x2_tests {
    use super::{mat3x2};

    use src::scalar::S;
    use src::typedefs::{vec2,vec3};
    use src::vector::{vec2,vec3};

    #[test]
    fn test_operators() {
        let l = mat3x2(1.0f32);
        let m = mat3x2(1.0f32);
        let u = vec3(1.0f32);
        let v = vec2(1.0f32);
        let x = S(1.0f32);
        let a : vec2 = m * u;
        let b : vec3 = v * m ;
        let n : mat3x2 = x / m;
        let o : mat3x2 = m / x;
        let p : mat3x2 = x * m;
        let q : mat3x2 = m * x;
        let _ = (a,b,n,o,p);
        assert_eq!(m, q);
        assert_eq!(m, l);
    }
}

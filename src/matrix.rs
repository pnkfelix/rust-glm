pub use src::typedefs::{mat2x2};

use src::vector::{TVec2,TVec2MulRHS};
use src::scalar::{S,SMulRHS,SDivRHS};
use std::fmt;

trait Matrix<RowVec,ColVec> {
    fn col(&self, i: uint) -> ColVec;
    fn row(&self, j: uint) -> RowVec;
}

trait Invertible {
    // (should this be Result<Self> ?) 
    // (also, should this take self by value?)
    fn inverse(&self) -> Self;
}

pub fn inverse<M:Invertible>(m: &M) -> M { m.inverse() }

pub struct TMat2<T> {
    elems: [[T, ..2], ..2]
}
#[deriving(Eq,Show)]
pub struct TMat3<T>; // FIXME
#[deriving(Eq,Show)]
pub struct TMat4<T>; // FIXME

#[deriving(Eq,Show)]
pub struct TMat2x3<T>; // FIXME
#[deriving(Eq,Show)]
pub struct TMat2x4<T>; // FIXME
#[deriving(Eq,Show)]
pub struct TMat3x2<T>; // FIXME
#[deriving(Eq,Show)]
pub struct TMat3x4<T>; // FIXME
#[deriving(Eq,Show)]
pub struct TMat4x2<T>; // FIXME
#[deriving(Eq,Show)]
pub struct TMat4x3<T>; // FIXME

impl<T:Clone> Matrix<TVec2<T>,TVec2<T>> for TMat2<T> {
    fn col(&self, i: uint) -> TVec2<T> {
        assert!(i <= 1);
        TVec2 { x: self.elems[0][i].clone(), y: self.elems[1][i].clone() }
    }
    fn row(&self, j: uint) -> TVec2<T> {
        assert!(j <= 1);
        TVec2 { x: self.elems[j][0].clone(), y: self.elems[j][1].clone() }
    }
}

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

impl<T:fmt::Show> fmt::Show for TMat2<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "["));
        try!(self.elems[0].as_slice().fmt(f));
        try!(write!(f, "\n "));
        write!(f, "]")
    }
}

impl<T:Eq> Eq for TMat2<T> {
    fn eq(&self, rhs: &TMat2<T>) -> bool {
        self.elems[0] == rhs.elems[0] && self.elems[1] == rhs.elems[1]
    }
}

impl<T> TMat2<T> {
    fn map<U>(&self, f: |&T| -> U) -> TMat2<U> {
        TMat2 { elems: [[f(&self.elems[0][0]), f(&self.elems[0][1])],
                        [f(&self.elems[1][0]), f(&self.elems[1][1])]] }
    }
}

pub trait Mat2x2Args { fn make(self) -> mat2x2; }

pub fn mat2x2<Args:Mat2x2Args>(args: Args) -> mat2x2 { args.make() }
pub fn mat2<Args:Mat2x2Args>(args: Args) -> mat2x2 { args.make() }

macro_rules! impl_Mat2x2Args_for {
    ($a:ident copy) => {
        impl Mat2x2Args for $a {
            fn make(self) -> mat2x2 {
                let x = self;
                TMat2 { elems: [[x as f32,x as f32],
                                [x as f32,x as f32]] }
            }
        }
    }
    ;
    ($a:ident,$b:ident,$c:ident,$d:ident) => {
        impl Mat2x2Args for ($a,$b,$c,$d) {
            fn make(self) -> mat2x2 {
                let (a,b,c,d) = self;
                TMat2 { elems: [[a as f32,b as f32],
                                [c as f32,d as f32]] }
            }
        }
    }
    ;
}


impl_Mat2x2Args_for!(f32 copy)
impl_Mat2x2Args_for!(int,int,int,int)
impl_Mat2x2Args_for!(int,int,int,f32)
impl_Mat2x2Args_for!(int,int,f32,int)
impl_Mat2x2Args_for!(int,int,f32,f32)
impl_Mat2x2Args_for!(int,f32,int,int)
impl_Mat2x2Args_for!(int,f32,int,f32)
impl_Mat2x2Args_for!(int,f32,f32,int)
impl_Mat2x2Args_for!(int,f32,f32,f32)
impl_Mat2x2Args_for!(f32,int,int,int)
impl_Mat2x2Args_for!(f32,int,int,f32)
impl_Mat2x2Args_for!(f32,int,f32,int)
impl_Mat2x2Args_for!(f32,int,f32,f32)
impl_Mat2x2Args_for!(f32,f32,int,int)
impl_Mat2x2Args_for!(f32,f32,int,f32)
impl_Mat2x2Args_for!(f32,f32,f32,int)
impl_Mat2x2Args_for!(f32,f32,f32,f32)

double_dispatch_T!{Mul for TMat2 mul via TMat2MulRHS rev_mul}
double_dispatch_T!{Div for TMat2 div via TMat2DivRHS rev_div}

impl<T:Num> TMat2MulRHS<T,TMat2<T>> for TMat2<T> {
    fn rev_mul(&self, lhs: &TMat2<T>) -> TMat2<T> {
        let l = &lhs.elems;
        let r = &self.elems;
        let c00 = l[0][0] * r[0][0] + l[0][1] * r[1][0];
        let c01 = l[0][0] * r[0][1] + l[0][1] * r[1][1];
        let c10 = l[1][0] * r[0][0] + l[1][1] * r[1][0];
        let c11 = l[1][0] * r[0][1] + l[1][1] * r[1][1];

        TMat2 { elems: [[c00, c01],
                        [c10, c11]] }
    }
}

impl<T:Num> TMat2MulRHS<T,TVec2<T>> for TVec2<T> {
    fn rev_mul(&self, lhs: &TMat2<T>) -> TVec2<T> {
        let a00 = &lhs.elems[0][0]; let a01 = &lhs.elems[0][1];
        let a10 = &lhs.elems[1][0]; let a11 = &lhs.elems[1][1];
        TVec2{ x: *a00 * self.x + *a01 * self.y,
               y: *a10 * self.x + *a11 * self.y }
    }
}

impl<T:Num> TVec2MulRHS<T,TVec2<T>> for TMat2<T> {
    fn rev_mul(&self, lhs: &TVec2<T>) -> TVec2<T> {
        let b00 = &self.elems[0][0]; let b01 = &self.elems[0][1];
        let b10 = &self.elems[1][0]; let b11 = &self.elems[1][1];
        TVec2{ x: lhs.x * *b00 + lhs.y * *b10,
               y: lhs.x * *b01 + lhs.y * *b11 }
    }
}

impl<T:Num> TMat2MulRHS<T,TMat2<T>> for S<T> {
    fn rev_mul(&self, lhs: &TMat2<T>) -> TMat2<T> {
        let S(ref f) = *self;
        lhs.map(|x|*x * *f)
    }
}

impl<T:Num> TMat2DivRHS<T,TMat2<T>> for S<T> {
    fn rev_div(&self, lhs: &TMat2<T>) -> TMat2<T> {
        let S(ref f) = *self;
        lhs.map(|x|*x / *f)
    }
}

impl<T:Num> SMulRHS<T,TMat2<T>> for TMat2<T> {
    fn rev_mul(&self, lhs: &S<T>) -> TMat2<T> {
        let S(ref f) = *lhs;
        self.map(|x|*f * *x)
    }
}

impl<T:Num> SDivRHS<T,TMat2<T>> for TMat2<T> {
    fn rev_div(&self, lhs: &S<T>) -> TMat2<T> {
        let S(ref f) = *lhs;
        self.map(|x|*f / *x)
    }
}

#[cfg(test)]
mod mat2x2_tests {
    #![allow(uppercase_variables)]

    use super::{mat2x2,mat2};
    use super::{inverse};
    use super::{Matrix};

    use src::operators::{EpsilonEq};
    use src::typedefs::{vec2};
    use src::vector::{vec2};
    use src::scalar::S;

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
    }
}

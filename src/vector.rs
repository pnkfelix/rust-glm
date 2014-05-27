use super::typedefs::{vec1,vec2};
use super::typedefs::{ivec1};

/// Scalar value of type T.  (New type wrapper to ease operator overloading.)
pub struct S<T>(pub T);

pub trait SAddRHS<T,Result> { fn rev_add(&self, lhs: &S<T>) -> Result; }
pub trait SSubRHS<T,Result> { fn rev_sub(&self, lhs: &S<T>) -> Result; }
pub trait SMulRHS<T,Result> { fn rev_mul(&self, lhs: &S<T>) -> Result; }
pub trait SDivRHS<T,Result> { fn rev_div(&self, lhs: &S<T>) -> Result; }

impl<T,Result,RHS:SAddRHS<T,Result>> Add<RHS,Result> for S<T> {
    fn add(&self, rhs: &RHS) -> Result {
        rhs.rev_add(self)
    }
}

impl<T,Result,RHS:SSubRHS<T,Result>> Sub<RHS,Result> for S<T> {
    fn sub(&self, rhs: &RHS) -> Result {
        rhs.rev_sub(self)
    }
}

impl<T,Result,RHS:SMulRHS<T,Result>> Mul<RHS,Result> for S<T> {
    fn mul(&self, rhs: &RHS) -> Result {
        rhs.rev_mul(self)
    }
}
impl<T,Result,RHS:SDivRHS<T,Result>> Div<RHS,Result> for S<T> {
    fn div(&self, rhs: &RHS) -> Result {
        rhs.rev_div(self)
    }
}

#[deriving(Eq, Show)]
pub struct TVec1<T> { x: T, }
#[deriving(Eq, Show)]
pub struct TVec2<T> { x: T, y: T, }
#[deriving(Eq, Show)]
pub struct TVec3<T> { x: T, y: T, z: T, }
#[deriving(Eq, Show)]
pub struct TVec4<T> { x: T, y: T, z: T, w: T, }

pub trait Vec1Args { fn make(self) -> vec1; }
pub fn vec1<Args:Vec1Args>(args: Args) -> vec1 { args.make() }

impl Vec1Args for f32 { fn make(self) -> vec1 { TVec1 { x: self } } }

pub trait IVec1Args { fn make(self) -> ivec1; }
pub fn ivec1<Args:IVec1Args>(args: Args) -> ivec1 { args.make() }

impl IVec1Args for i32 { fn make(self) -> ivec1 { TVec1 { x: self } } }
impl IVec1Args for TVec1<i32> { fn make(self) -> ivec1 { TVec1 { x: self.x } } }

pub trait Vec2Args { fn make(self) -> vec2; }

pub fn vec2<Args:Vec2Args>(args: Args) -> vec2 { args.make() }

impl Vec2Args for f32 { fn make(self) -> vec2 { TVec2 { x: self, y: self } } }
impl Vec2Args for (f32,f32) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x, y: y } } }
impl Vec2Args for (int,int) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x as f32, y: y as f32 } } }
impl Vec2Args for (int,f32) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x as f32, y: y } } }
impl Vec2Args for (f32,int) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x, y: y as f32 } } }

impl<T:Neg<T>> Neg<TVec2<T>> for TVec2<T> {
    fn neg(&self) -> TVec2<T> {
        TVec2 { x: -self.x, y: -self.y }
    }
}

pub trait TVec2AddRHS<T> { fn rev_add(&self, lhs: &TVec2<T>) -> TVec2<T>; }

impl<T,RHS:TVec2AddRHS<T>> Add<RHS,TVec2<T>> for TVec2<T> {
    fn add(&self, rhs: &RHS) -> TVec2<T> { rhs.rev_add(self) }
}

impl TVec2AddRHS<f32> for f32 {
    fn rev_add(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: self + lhs.x, y: self + lhs.y }
    }
}

impl SAddRHS<f32,TVec2<f32>> for TVec2<f32> {
    fn rev_add(&self, lhs: &S<f32>) -> TVec2<f32> {
        let &S(lhs) = lhs;
        TVec2 { x: lhs + self.x, y: lhs + self.y }
    }
}

impl TVec2AddRHS<f32> for TVec2<f32> {
    fn rev_add(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: self.x + lhs.x, y: self.y + lhs.y }
    }
}

pub trait TVec2SubRHS<T> { fn rev_sub(&self, lhs: &TVec2<T>) -> TVec2<T>; }

impl<T,RHS:TVec2SubRHS<T>> Sub<RHS,TVec2<T>> for TVec2<T> {
    fn sub(&self, rhs: &RHS) -> TVec2<T> { rhs.rev_sub(self) }
}

impl TVec2SubRHS<f32> for f32 {
    fn rev_sub(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: lhs.x - *self, y: lhs.y - *self }
    }
}

impl SSubRHS<f32,TVec2<f32>> for TVec2<f32> {
    fn rev_sub(&self, lhs: &S<f32>) -> TVec2<f32> {
        let &S(lhs) = lhs;
        TVec2 { x: lhs - self.x, y: lhs - self.y }
    }
}

impl TVec2SubRHS<f32> for TVec2<f32> {
    fn rev_sub(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: lhs.x - self.x, y: lhs.y - self.y }
    }
}

pub trait TVec2MulRHS<T> { fn rev_mul(&self, lhs: &TVec2<T>) -> TVec2<T>; }

impl<T,RHS:TVec2MulRHS<T>> Mul<RHS,TVec2<T>> for TVec2<T> {
    fn mul(&self, rhs: &RHS) -> TVec2<T> { rhs.rev_mul(self) }
}

impl TVec2MulRHS<f32> for f32 {
    fn rev_mul(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: lhs.x * *self, y: lhs.y * *self }
    }
}

impl SMulRHS<f32, TVec2<f32>> for TVec2<f32> {
    fn rev_mul(&self, lhs: &S<f32>) -> TVec2<f32> {
        let &S(lhs) = lhs;
        TVec2 { x: lhs * self.x, y: lhs * self.y }
    }
}

impl TVec2MulRHS<f32> for TVec2<f32> {
    fn rev_mul(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: lhs.x * self.x, y: lhs.y * self.y }
    }
}

pub trait TVec2DivRHS<T> { fn rev_div(&self, lhs: &TVec2<T>) -> TVec2<T>; }

impl<T,RHS:TVec2DivRHS<T>> Div<RHS,TVec2<T>> for TVec2<T> {
    fn div(&self, rhs: &RHS) -> TVec2<T> { rhs.rev_div(self) }
}

impl TVec2DivRHS<f32> for f32 {
    fn rev_div(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: lhs.x / *self, y: lhs.y / *self }
    }
}

impl TVec2DivRHS<f32> for TVec2<f32> {
    fn rev_div(&self, lhs: &TVec2<f32>) -> TVec2<f32> {
        TVec2 { x: lhs.x / self.x, y: lhs.y / self.y }
    }
}

impl<T:Num + Clone> TVec1<T> {
    pub fn postdecrement(&mut self) -> TVec1<T> {
        use std::num::One;
        let ret = TVec1{ x: self.x.clone() };
        self.x = self.x - One::one();
        ret
    }

    pub fn predecrement<'a>(&'a mut self) -> &'a mut TVec1<T> {
        use std::num::One;
        self.x = self.x - One::one();
        self
    }

    pub fn postincrement(&mut self) -> TVec1<T> {
        use std::num::One;
        let ret = TVec1{ x: self.x.clone() };
        self.x = self.x + One::one();
        ret
    }

    pub fn preincrement<'a>(&'a mut self) -> &'a mut TVec1<T> {
        use std::num::One;
        self.x = self.x + One::one();
        self
    }
}

impl<T:Num + Clone> TVec2<T> {
    pub fn postdecrement(&mut self) -> TVec2<T> {
        use std::num::One;
        let ret = TVec2{ x: self.x.clone(), y: self.y.clone() };
        self.x = self.x - One::one();
        self.y = self.y - One::one();
        ret
    }

    pub fn predecrement<'a>(&'a mut self) -> &'a mut TVec2<T> {
        use std::num::One;
        self.x = self.x - One::one();
        self.y = self.y - One::one();
        self
    }

    pub fn postincrement(&mut self) -> TVec2<T> {
        use std::num::One;
        let ret = TVec2{ x: self.x.clone(), y: self.y.clone() };
        self.x = self.x + One::one();
        self.y = self.y + One::one();
        ret
    }

    pub fn preincrement<'a>(&'a mut self) -> &'a mut TVec2<T> {
        use std::num::One;
        self.x = self.x + One::one();
        self.y = self.y + One::one();
        self
    }
}

pub trait TVec2AddAssignRHS<T> { fn add_into(&self, recv: &mut TVec2<T>); }

impl<T,RHS:TVec2AddAssignRHS<T>> TVec2<T> {
    /// Placeholder for an assumed future `+=` operator.
    pub fn add_assign(&mut self, rhs: &RHS) {
        rhs.add_into(self)
    }
}

impl TVec2AddAssignRHS<f32> for f32 {
    fn add_into(&self, recv: &mut TVec2<f32>) {
        recv.x += *self;
        recv.y += *self;
    }
}

impl TVec2AddAssignRHS<f32> for TVec2<f32> {
    fn add_into(&self, recv: &mut TVec2<f32>) {
        recv.x += self.x;
        recv.y += self.y;
    }
}

pub trait TVec2SubAssignRHS<T> { fn rsb_into(&self, recv: &mut TVec2<T>); }

impl<T,RHS:TVec2SubAssignRHS<T>> TVec2<T> {
    /// Placeholder for an assumed future `+=` operator.
    pub fn sub_assign(&mut self, rhs: &RHS) {
        rhs.rsb_into(self)
    }
}

impl TVec2SubAssignRHS<f32> for f32 {
    fn rsb_into(&self, recv: &mut TVec2<f32>) {
        recv.x -= *self;
        recv.y -= *self;
    }
}

impl TVec2SubAssignRHS<f32> for TVec2<f32> {
    fn rsb_into(&self, recv: &mut TVec2<f32>) {
        recv.x -= self.x;
        recv.y -= self.y;
    }
}

pub trait TVec2MulAssignRHS<T> { fn mul_into(&self, recv: &mut TVec2<T>); }

impl<T,RHS:TVec2MulAssignRHS<T>> TVec2<T> {
    /// Placeholder for an assumed future `+=` operator.
    pub fn mul_assign(&mut self, rhs: &RHS) {
        rhs.mul_into(self)
    }
}

impl TVec2MulAssignRHS<f32> for f32 {
    fn mul_into(&self, recv: &mut TVec2<f32>) {
        recv.x *= *self;
        recv.y *= *self;
    }
}

impl TVec2MulAssignRHS<f32> for TVec2<f32> {
    fn mul_into(&self, recv: &mut TVec2<f32>) {
        recv.x *= self.x;
        recv.y *= self.y;
    }
}

pub trait TVec2DivAssignRHS<T> { fn div_into(&self, recv: &mut TVec2<T>); }

impl<T,RHS:TVec2DivAssignRHS<T>> TVec2<T> {
    /// Placeholder for an assumed future `+=` operator.
    pub fn div_assign(&mut self, rhs: &RHS) {
        rhs.div_into(self)
    }
}

impl TVec2DivAssignRHS<f32> for f32 {
    fn div_into(&self, recv: &mut TVec2<f32>) {
        recv.x /= *self;
        recv.y /= *self;
    }
}

impl SDivRHS<f32, TVec2<f32>> for TVec2<f32> {
    fn rev_div(&self, lhs: &S<f32>) -> TVec2<f32> {
        let &S(lhs) = lhs;
        TVec2 { x: lhs / self.x, y: lhs / self.y }
    }
}

impl TVec2DivAssignRHS<f32> for TVec2<f32> {
    fn div_into(&self, recv: &mut TVec2<f32>) {
        recv.x /= self.x;
        recv.y /= self.y;
    }
}

#[cfg(test)]
mod vec1_tests {
    #![allow(uppercase_variables)]
    use super::vec1;
    use super::ivec1;

    #[test]
    fn test_operators() {
        let A = vec1(1.0f32);
        let B = vec1(2.0f32);
        assert!(A != B);
    }

    #[test]
    fn test_operator_increment() {
        let v0 = ivec1(1i32);
        let mut v1 = ivec1(v0);
        let mut v2 = ivec1(v0);
        let v3 = v1.preincrement();
        let v4 = v2.postincrement();
        let v1 = v1;
        let v2 = v2;

        assert_eq!(v0, v4);
        assert_eq!(v1, v2);
        assert_eq!(v1, *v3);
    }
}

#[cfg(test)]
mod vec2_tests {
    #![allow(uppercase_variables)]
    use super::vec2;
    use super::S;

    #[test]
    fn test_operators() {
        let A = vec2(1.0f32);
        let B = vec2(1.0f32);
        assert!(A == B);

        let A = vec2(1.0f32);
        let C = A + 1.0f32;
        let mut A = A;
        A.add_assign(&1.0f32);
        assert!(A.x == 2.0 && A.y == 2.0);
        assert!(A == C);

        let A = vec2(1.0f32);
        let B = vec2((2.0f32, -1.0f32));
        let C = A + B;
        let mut A = A;
        A.add_assign(&B);
        assert!(A.x == 3.0 && A.y == 0.0);
        assert!(A == C);

        let A = vec2(1.0f32);
        let C = A - 1.0f32;
        let mut A = A;
        A.sub_assign(&1.0f32);
        assert!(A.x == 0.0 && A.y == 0.0);
        assert!(A == C);

        let A = vec2(1.0f32);
        let C = A * 2.0f32;
        let mut A = A;
        A.mul_assign(&2.0f32);
        assert!(A.x == 2.0 && A.y == 2.0);
        assert!(A == C);

        let A = vec2(2.0f32);
        let B = vec2(2.0f32);
        let C = A / B;
        let mut A = A;
        A.div_assign(&B);
        assert!(A.x == 1.0 && A.y == 1.0);
        assert!(A == C);

        let A = vec2((1.0f32, 2.0f32));
        let B = vec2((4.0f32, 5.0f32));
        let C = A + B;
        assert_eq!(C, vec2(( 5,  7 )));

        let D = B - A;
        assert_eq!(D, vec2(( 3,  3 )));

        let E = A * B;
        assert_eq!(E, vec2(( 4, 10 )));

        let F = B / A;
        assert_eq!(F, vec2(( 4, 2.5f32 )));

        let G = A + 1.0f32;
        assert_eq!(G, vec2(( 2,  3 )));

        let H = B - 1.0f32;
        assert_eq!(H, vec2(( 3,  4 )));

        let I = A * 2.0f32;
        assert_eq!(I, vec2(( 2,  4 )));

        let J = B / 2.0f32;
        assert_eq!(J, vec2(( 2, 2.5f32 )));

        let K = S(1.0f32) + A;
        assert_eq!(K, vec2(( 2,  3 )));

        let L = S(1.0f32) - B;
        assert_eq!(L, vec2((-3, -4 )));

        let M = S(2.0f32) * A;
        assert_eq!(M, vec2(( 2,  4 )));

        let N = S(2.0f32) / B;
        assert_eq!(N, vec2(( 0.5f32,  2.0f32/5.0f32 )));

        let A = vec2(( 1,  2 ));
        let B = vec2(( 4,  5 ));
        let mut A = A;
        A.add_assign(&B);
        assert_eq!(A, vec2(( 5, 7 )));

        A.add_assign(&1.0f32);
        assert_eq!(A, vec2(( 6, 8 )));

        let A = vec2(( 1,  2 ));
        let B = vec2(( 4,  5 ));
        let mut B = B;
        B.sub_assign(&A);
        assert_eq!(B, vec2(( 3, 3 )));

        B.sub_assign(&1.0f32);
        assert_eq!(B, vec2(( 2, 2 )));

        let A = vec2(( 1,  2 ));
        let B = vec2(( 4,  5 ));
        let mut A = A;
        A.mul_assign(&B);
        assert_eq!(A, vec2(( 4, 10 )));

        A.mul_assign(&2.0f32);
        assert_eq!(A, vec2(( 8, 20 )));

        let A = vec2(( 1,  2 ));
        let B = vec2(( 4,  5 ));
        let mut B = B;
        B.div_assign(&A);
        assert_eq!(B, vec2(( 4, 2.5f32 )));

        B.div_assign(&2.0f32);
        assert_eq!(B, vec2(( 2, 1.25f32 )));

        let B = vec2(2.0f32);
        let mut B = B;
        let B_y = B.y;
        B.div_assign(&B_y);
        assert_eq!(B, vec2( 1.0f32 ));

        let A = vec2(( 1.0f32, 2.0f32 ));
        let B = -A;
        assert_eq!(B, vec2(( -1.0f32, -2.0f32 )));
    }

    #[test]
    fn test_incr_decr_operators() {
        let A = vec2(( 1.0f32, 2.0f32 ));
        let mut A = A;
        {
            let B = A.predecrement(); // Rust does not have operator--(int).
            assert_eq!(*B, vec2(( 0.0f32, 1.0f32 )));
        }
        assert_eq!(A, vec2(( 0.0f32, 1.0f32 )));

        let A = vec2(( 1.0f32, 2.0f32 ));
        let mut A = A;
        let B = A.postdecrement(); // Rust does not have operator--().
        assert_eq!(B, vec2(( 1.0f32, 2.0f32 )));
        assert_eq!(A, vec2(( 0.0f32, 1.0f32 )));

        let A = vec2(( 1.0f32, 2.0f32 ));
        let mut A = A;
        {
            let B = A.preincrement(); // Rust does not have operator++(int).
            assert_eq!(*B, vec2(( 2.0f32, 3.0f32 )));
        }
        assert_eq!(A, vec2(( 2.0f32, 3.0f32 )));

        let A = vec2(( 1.0f32, 2.0f32 ));
        let mut A = A;
        let B = A.postincrement(); // Rust does not have operator++().
        assert_eq!(B, vec2(( 1.0f32, 2.0f32 )));
        assert_eq!(A, vec2(( 2.0f32, 3.0f32 )));

    }
}

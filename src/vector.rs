use super::typedefs::{vec1,vec2,vec3,vec4};
use super::typedefs::{ivec1,ivec2,ivec3};

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

pub trait Increment {
    fn postincrement(&mut self) -> Self;
    fn preincrement<'a>(&'a mut self) -> &'a mut Self;
}

pub trait Decrement {
    fn postdecrement(&mut self) -> Self;
    fn predecrement<'a>(&'a mut self) -> &'a mut Self;
}

impl Increment for i32 {
    fn postincrement(&mut self) -> i32 { let r = *self; *self = *self + 1; r }
    fn preincrement<'a>(&'a mut self) -> &'a mut i32 { *self = *self + 1; self }
}
impl Increment for i64 {
    fn postincrement(&mut self) -> i64 { let r = *self; *self = *self + 1; r }
    fn preincrement<'a>(&'a mut self) -> &'a mut i64 { *self = *self + 1; self }
}
impl Increment for int {
    fn postincrement(&mut self) -> int { let r = *self; *self = *self + 1; r }
    fn preincrement<'a>(&'a mut self) -> &'a mut int { *self = *self + 1; self }
}

impl Decrement for i32 {
    fn postdecrement(&mut self) -> i32 { let r = *self; *self = *self + 1; r }
    fn predecrement<'a>(&'a mut self) -> &'a mut i32 { *self = *self + 1; self }
}
impl Decrement for i64 {
    fn postdecrement(&mut self) -> i64 { let r = *self; *self = *self + 1; r }
    fn predecrement<'a>(&'a mut self) -> &'a mut i64 { *self = *self + 1; self }
}
impl Decrement for int {
    fn postdecrement(&mut self) -> int { let r = *self; *self = *self + 1; r }
    fn predecrement<'a>(&'a mut self) -> &'a mut int { *self = *self + 1; self }
}

pub trait AddAssign<RHS> {
    /// Placeholder for an assumed future `+=` operator.
    fn add_assign(&mut self, rhs: &RHS);
}

pub trait SubAssign<RHS> {
    /// Placeholder for an assumed future `-=` operator.
    fn sub_assign(&mut self, rhs: &RHS);
}

pub trait MulAssign<RHS> {
    /// Placeholder for an assumed future `*=` operator.
    fn mul_assign(&mut self, rhs: &RHS);
}

pub trait DivAssign<RHS> {
    /// Placeholder for an assumed future `/=` operator.
    fn div_assign(&mut self, rhs: &RHS);
}

pub trait DotProduct<T> {
    fn dot(&self, &Self) -> T;
}

pub fn dot<T,V:DotProduct<T>>(x: V, y: V) -> T { x.dot(&y) }

trait Swizzle2<T> {
    fn x(&self) -> T;
    fn y(&self) -> T;

    fn xx(&self) -> TVec2<T> { TVec2 { x: self.x(), y: self.x() } }
    fn xy(&self) -> TVec2<T> { TVec2 { x: self.x(), y: self.y() } }
    fn yx(&self) -> TVec2<T> { TVec2 { x: self.y(), y: self.x() } }
    fn yy(&self) -> TVec2<T> { TVec2 { x: self.y(), y: self.y() } }

    fn xxx(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.x(), z: self.x() } }
    fn xxy(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.x(), z: self.y() } }
    fn xyx(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.y(), z: self.x() } }
    fn xyy(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.y(), z: self.y() } }
    fn yxx(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.x(), z: self.x() } }
    fn yxy(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.x(), z: self.y() } }
    fn yyx(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.y(), z: self.x() } }
    fn yyy(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.y(), z: self.y() } }

    fn xxxx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.x(), w: self.x() } }
    fn xxxy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.x(), w: self.y() } }
    fn xxyx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.y(), w: self.x() } }
    fn xxyy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.y(), w: self.y() } }
    fn xyxx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.x(), w: self.x() } }
    fn xyxy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.x(), w: self.y() } }
    fn xyyx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.y(), w: self.x() } }
    fn xyyy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.y(), w: self.y() } }
    fn yxxx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.x(), w: self.x() } }
    fn yxxy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.x(), w: self.y() } }
    fn yxyx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.y(), w: self.x() } }
    fn yxyy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.y(), w: self.y() } }
    fn yyxx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.x(), w: self.x() } }
    fn yyxy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.x(), w: self.y() } }
    fn yyyx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.y(), w: self.x() } }
    fn yyyy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.y(), w: self.y() } }
}
trait Swizzle3<T> : Swizzle2<T> {
    fn z(&self) -> T;

    fn xz(&self) -> TVec2<T> { TVec2 { x: self.x(), y: self.z() } }
    fn yz(&self) -> TVec2<T> { TVec2 { x: self.y(), y: self.z() } }
    fn zx(&self) -> TVec2<T> { TVec2 { x: self.z(), y: self.x() } }
    fn zy(&self) -> TVec2<T> { TVec2 { x: self.z(), y: self.y() } }
    fn zz(&self) -> TVec2<T> { TVec2 { x: self.z(), y: self.z() } }

    fn xxz(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.x(), z: self.z() } }
    fn xyz(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.y(), z: self.z() } }
    fn xzx(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.z(), z: self.x() } }
    fn xzy(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.z(), z: self.y() } }
    fn xzz(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.z(), z: self.z() } }
    fn yxz(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.x(), z: self.z() } }
    fn yyz(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.y(), z: self.z() } }
    fn yzx(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.z(), z: self.x() } }
    fn yzy(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.z(), z: self.y() } }
    fn yzz(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.z(), z: self.z() } }
    fn zxx(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.x(), z: self.x() } }
    fn zxy(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.x(), z: self.y() } }
    fn zxz(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.x(), z: self.z() } }
    fn zyx(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.y(), z: self.x() } }
    fn zyy(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.y(), z: self.y() } }
    fn zyz(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.y(), z: self.z() } }
    fn zzx(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.z(), z: self.x() } }
    fn zzy(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.z(), z: self.y() } }
    fn zzz(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.z(), z: self.z() } }


    fn xxxz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.x(), w: self.z() } }
    fn xxyz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.y(), w: self.z() } }
    fn xxzx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.z(), w: self.x() } }
    fn xxzy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.z(), w: self.y() } }
    fn xxzz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.z(), w: self.z() } }
    fn xyxz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.x(), w: self.z() } }
    fn xyyz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.y(), w: self.z() } }
    fn xyzx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.z(), w: self.x() } }
    fn xyzy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.z(), w: self.y() } }
    fn xyzz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.z(), w: self.z() } }
    fn xzxx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.x(), w: self.x() } }
    fn xzxy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.x(), w: self.y() } }
    fn xzxz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.x(), w: self.z() } }
    fn xzyx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.y(), w: self.x() } }
    fn xzyy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.y(), w: self.y() } }
    fn xzyz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.y(), w: self.z() } }
    fn xzzx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.z(), w: self.x() } }
    fn xzzy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.z(), w: self.y() } }
    fn xzzz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.z(), w: self.z() } }

    fn yxxz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.x(), w: self.z() } }
    fn yxyz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.y(), w: self.z() } }
    fn yxzx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.z(), w: self.x() } }
    fn yxzy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.z(), w: self.y() } }
    fn yxzz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.z(), w: self.z() } }
    fn yyxz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.x(), w: self.z() } }
    fn yyyz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.y(), w: self.z() } }
    fn yyzx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.z(), w: self.x() } }
    fn yyzy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.z(), w: self.y() } }
    fn yyzz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.z(), w: self.z() } }
    fn yzxx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.x(), w: self.x() } }
    fn yzxy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.x(), w: self.y() } }
    fn yzxz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.x(), w: self.z() } }
    fn yzyx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.y(), w: self.x() } }
    fn yzyy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.y(), w: self.y() } }
    fn yzyz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.y(), w: self.z() } }
    fn yzzx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.z(), w: self.x() } }
    fn yzzy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.z(), w: self.y() } }
    fn yzzz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.z(), w: self.z() } }

    fn zxxx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.x(), w: self.x() } }
    fn zxxy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.x(), w: self.y() } }
    fn zxxz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.x(), w: self.z() } }
    fn zxyx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.y(), w: self.x() } }
    fn zxyz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.y(), w: self.z() } }
    fn zxzx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.z(), w: self.x() } }
    fn zxzy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.z(), w: self.y() } }
    fn zxzz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.z(), w: self.z() } }
    fn zyxx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.x(), w: self.x() } }
    fn zyxy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.x(), w: self.y() } }
    fn zyxz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.x(), w: self.z() } }
    fn zyyx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.y(), w: self.x() } }
    fn zyyy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.y(), w: self.y() } }
    fn zyyz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.y(), w: self.z() } }
    fn zyzx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.z(), w: self.x() } }
    fn zyzy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.z(), w: self.y() } }
    fn zyzz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.z(), w: self.z() } }
    fn zzxx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.x(), w: self.x() } }
    fn zzxy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.x(), w: self.y() } }
    fn zzxz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.x(), w: self.z() } }
    fn zzyx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.y(), w: self.x() } }
    fn zzyy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.y(), w: self.y() } }
    fn zzyz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.y(), w: self.z() } }
    fn zzzx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.z(), w: self.x() } }
    fn zzzy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.z(), w: self.y() } }
    fn zzzz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.z(), w: self.z() } }
}
trait Swizzle4<T> : Swizzle3<T> {
    fn w(&self) -> T;
    fn xw(&self) -> TVec2<T> { TVec2 { x: self.x(), y: self.y() } }
    fn yw(&self) -> TVec2<T> { TVec2 { x: self.x(), y: self.y() } }
    fn zw(&self) -> TVec2<T> { TVec2 { x: self.x(), y: self.y() } }

    fn xxw(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.x(), z: self.w() } }
    fn xyw(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.y(), z: self.w() } }
    fn xzw(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.z(), z: self.w() } }
    fn xwx(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.w(), z: self.x() } }
    fn xwy(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.w(), z: self.y() } }
    fn xwz(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.w(), z: self.z() } }
    fn xww(&self) -> TVec3<T> { TVec3 { x: self.x(), y: self.w(), z: self.w() } }

    fn yxw(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.x(), z: self.w() } }
    fn yyw(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.y(), z: self.w() } }
    fn yzw(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.z(), z: self.w() } }
    fn ywx(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.w(), z: self.x() } }
    fn ywy(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.w(), z: self.y() } }
    fn ywz(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.w(), z: self.z() } }
    fn yww(&self) -> TVec3<T> { TVec3 { x: self.y(), y: self.w(), z: self.w() } }

    fn zxw(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.x(), z: self.w() } }
    fn zyw(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.y(), z: self.w() } }
    fn zzw(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.z(), z: self.w() } }
    fn zwx(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.w(), z: self.x() } }
    fn zwy(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.w(), z: self.y() } }
    fn zwz(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.w(), z: self.z() } }
    fn zww(&self) -> TVec3<T> { TVec3 { x: self.z(), y: self.w(), z: self.w() } }

    fn wxx(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.x(), z: self.x() } }
    fn wxy(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.x(), z: self.y() } }
    fn wxz(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.x(), z: self.z() } }
    fn wxw(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.x(), z: self.w() } }
    fn wyx(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.y(), z: self.x() } }
    fn wyy(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.y(), z: self.y() } }
    fn wyz(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.y(), z: self.z() } }
    fn wyw(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.y(), z: self.w() } }
    fn wzx(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.z(), z: self.x() } }
    fn wzy(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.z(), z: self.y() } }
    fn wzz(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.z(), z: self.z() } }
    fn wzw(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.z(), z: self.w() } }
    fn wwx(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.w(), z: self.x() } }
    fn wwy(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.w(), z: self.y() } }
    fn wwz(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.w(), z: self.z() } }
    fn www(&self) -> TVec3<T> { TVec3 { x: self.w(), y: self.w(), z: self.w() } }

    fn xxxw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.x(), w: self.w() } }
    fn xxyw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.y(), w: self.w() } }
    fn xxzw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.z(), w: self.w() } }
    fn xxww(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.x(), z: self.w(), w: self.w() } }
    fn xyxw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.x(), w: self.w() } }
    fn xyyw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.y(), w: self.w() } }
    fn xyzw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.z(), w: self.w() } }
    fn xyww(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.y(), z: self.w(), w: self.w() } }
    fn xzxw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.x(), w: self.w() } }
    fn xzyw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.y(), w: self.w() } }
    fn xzzw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.z(), w: self.w() } }
    fn xzww(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.z(), z: self.w(), w: self.w() } }
    fn xwxx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.x(), w: self.x() } }
    fn xwxy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.x(), w: self.y() } }
    fn xwxz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.x(), w: self.z() } }
    fn xwxw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.x(), w: self.w() } }
    fn xwyx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.y(), w: self.x() } }
    fn xwyy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.y(), w: self.y() } }
    fn xwyz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.y(), w: self.z() } }
    fn xwyw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.y(), w: self.w() } }
    fn xwzx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.z(), w: self.x() } }
    fn xwzy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.z(), w: self.y() } }
    fn xwzz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.z(), w: self.z() } }
    fn xwzw(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.z(), w: self.w() } }
    fn xwwx(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.w(), w: self.x() } }
    fn xwwy(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.w(), w: self.y() } }
    fn xwwz(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.w(), w: self.z() } }
    fn xwww(&self) -> TVec4<T> { TVec4 { x: self.x(), y: self.w(), z: self.w(), w: self.w() } }

    fn yxxw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.x(), w: self.w() } }
    fn yxyw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.y(), w: self.w() } }
    fn yxzw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.z(), w: self.w() } }
    fn yxww(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.x(), z: self.w(), w: self.w() } }
    fn yyxw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.x(), w: self.w() } }
    fn yyyw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.y(), w: self.w() } }
    fn yyzw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.z(), w: self.w() } }
    fn yyww(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.y(), z: self.w(), w: self.w() } }
    fn yzxw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.x(), w: self.w() } }
    fn yzyw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.y(), w: self.w() } }
    fn yzzw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.z(), w: self.w() } }
    fn yzww(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.z(), z: self.w(), w: self.w() } }
    fn ywxx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.x(), w: self.x() } }
    fn ywxy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.x(), w: self.y() } }
    fn ywxz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.x(), w: self.z() } }
    fn ywxw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.x(), w: self.w() } }
    fn ywyx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.y(), w: self.x() } }
    fn ywyy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.y(), w: self.y() } }
    fn ywyz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.y(), w: self.z() } }
    fn ywyw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.y(), w: self.w() } }
    fn ywzx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.z(), w: self.x() } }
    fn ywzy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.z(), w: self.y() } }
    fn ywzz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.z(), w: self.z() } }
    fn ywzw(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.z(), w: self.w() } }
    fn ywwx(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.w(), w: self.x() } }
    fn ywwy(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.w(), w: self.y() } }
    fn ywwz(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.w(), w: self.z() } }
    fn ywww(&self) -> TVec4<T> { TVec4 { x: self.y(), y: self.w(), z: self.w(), w: self.w() } }

    fn zxxw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.x(), w: self.w() } }
    fn zxyw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.y(), w: self.w() } }
    fn zxzw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.z(), w: self.w() } }
    fn zxww(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.x(), z: self.w(), w: self.w() } }
    fn zyxw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.x(), w: self.w() } }
    fn zyyw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.y(), w: self.w() } }
    fn zyzw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.z(), w: self.w() } }
    fn zyww(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.y(), z: self.w(), w: self.w() } }
    fn zzxw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.x(), w: self.w() } }
    fn zzyw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.y(), w: self.w() } }
    fn zzzw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.z(), w: self.w() } }
    fn zzww(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.z(), z: self.w(), w: self.w() } }
    fn zwxx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.x(), w: self.x() } }
    fn zwxy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.x(), w: self.y() } }
    fn zwxz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.x(), w: self.z() } }
    fn zwxw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.x(), w: self.w() } }
    fn zwyx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.y(), w: self.x() } }
    fn zwyy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.y(), w: self.y() } }
    fn zwyz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.y(), w: self.z() } }
    fn zwyw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.y(), w: self.w() } }
    fn zwzx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.z(), w: self.x() } }
    fn zwzy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.z(), w: self.y() } }
    fn zwzz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.z(), w: self.z() } }
    fn zwzw(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.z(), w: self.w() } }
    fn zwwx(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.w(), w: self.x() } }
    fn zwwy(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.w(), w: self.y() } }
    fn zwwz(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.w(), w: self.z() } }
    fn zwww(&self) -> TVec4<T> { TVec4 { x: self.z(), y: self.w(), z: self.w(), w: self.w() } }

    fn wxxx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.x(), w: self.x() } }
    fn wxxy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.x(), w: self.y() } }
    fn wxxz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.x(), w: self.z() } }
    fn wxxw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.x(), w: self.w() } }
    fn wxyx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.y(), w: self.x() } }
    fn wxyy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.y(), w: self.y() } }
    fn wxyz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.y(), w: self.z() } }
    fn wxyw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.y(), w: self.w() } }
    fn wxzx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.z(), w: self.x() } }
    fn wxzy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.z(), w: self.y() } }
    fn wxzz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.z(), w: self.z() } }
    fn wxzw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.z(), w: self.w() } }
    fn wxwx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.w(), w: self.x() } }
    fn wxwy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.w(), w: self.y() } }
    fn wxwz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.w(), w: self.z() } }
    fn wxww(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.x(), z: self.w(), w: self.w() } }

    fn wyxx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.x(), w: self.x() } }
    fn wyxy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.x(), w: self.y() } }
    fn wyxz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.x(), w: self.z() } }
    fn wyxw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.x(), w: self.w() } }
    fn wyyx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.y(), w: self.x() } }
    fn wyyy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.y(), w: self.y() } }
    fn wyyz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.y(), w: self.z() } }
    fn wyyw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.y(), w: self.w() } }
    fn wyzx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.z(), w: self.x() } }
    fn wyzy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.z(), w: self.y() } }
    fn wyzz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.z(), w: self.z() } }
    fn wyzw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.z(), w: self.w() } }
    fn wywx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.w(), w: self.x() } }
    fn wywy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.w(), w: self.y() } }
    fn wywz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.w(), w: self.z() } }
    fn wyww(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.y(), z: self.w(), w: self.w() } }

    fn wzxx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.x(), w: self.x() } }
    fn wzxy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.x(), w: self.y() } }
    fn wzxz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.x(), w: self.z() } }
    fn wzxw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.x(), w: self.w() } }
    fn wzyx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.y(), w: self.x() } }
    fn wzyy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.y(), w: self.y() } }
    fn wzyz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.y(), w: self.z() } }
    fn wzyw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.y(), w: self.w() } }
    fn wzzx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.z(), w: self.x() } }
    fn wzzy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.z(), w: self.y() } }
    fn wzzz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.z(), w: self.z() } }
    fn wzzw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.z(), w: self.w() } }
    fn wzwx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.w(), w: self.x() } }
    fn wzwy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.w(), w: self.y() } }
    fn wzwz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.w(), w: self.z() } }
    fn wzww(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.z(), z: self.w(), w: self.w() } }

    fn wwxx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.x(), w: self.x() } }
    fn wwxy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.x(), w: self.y() } }
    fn wwxz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.x(), w: self.z() } }
    fn wwxw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.x(), w: self.w() } }
    fn wwyx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.y(), w: self.x() } }
    fn wwyy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.y(), w: self.y() } }
    fn wwyz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.y(), w: self.z() } }
    fn wwyw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.y(), w: self.w() } }
    fn wwzx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.z(), w: self.x() } }
    fn wwzy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.z(), w: self.y() } }
    fn wwzz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.z(), w: self.z() } }
    fn wwzw(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.z(), w: self.w() } }
    fn wwwx(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.w(), w: self.x() } }
    fn wwwy(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.w(), w: self.y() } }
    fn wwwz(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.w(), w: self.z() } }
    fn wwww(&self) -> TVec4<T> { TVec4 { x: self.w(), y: self.w(), z: self.w(), w: self.w() } }
}

#[deriving(Eq, Show)]
pub struct TVec1<T> { x: T, }
#[deriving(Eq, Show)]
pub struct TVec2<T> { x: T, y: T, }
#[deriving(Eq, Show)]
pub struct TVec3<T> { x: T, y: T, z: T, }
#[deriving(Eq, Show)]
pub struct TVec4<T> { x: T, y: T, z: T, w: T, }

impl<T:Clone> Swizzle2<T> for TVec2<T> {
    fn x(&self) -> T { self.x.clone() }
    fn y(&self) -> T { self.y.clone() }
}
impl<T:Clone> Swizzle2<T> for TVec3<T> {
    fn x(&self) -> T { self.x.clone() }
    fn y(&self) -> T { self.y.clone() }
}
impl<T:Clone> Swizzle2<T> for TVec4<T> {
    fn x(&self) -> T { self.x.clone() }
    fn y(&self) -> T { self.y.clone() }
}
impl<T:Clone> Swizzle3<T> for TVec3<T> {
    fn z(&self) -> T { self.z.clone() }
}
impl<T:Clone> Swizzle3<T> for TVec4<T> {
    fn z(&self) -> T { self.z.clone() }
}
impl<T:Clone> Swizzle4<T> for TVec4<T> {
    fn w(&self) -> T { self.w.clone() }
}

pub trait Vec1Args { fn make(self) -> vec1; }
pub fn vec1<Args:Vec1Args>(args: Args) -> vec1 { args.make() }

impl Vec1Args for f32 { fn make(self) -> vec1 { TVec1 { x: self } } }

pub trait IVec1Args { fn make(self) -> ivec1; }
pub fn ivec1<Args:IVec1Args>(args: Args) -> ivec1 { args.make() }

impl IVec1Args for i32 { fn make(self) -> ivec1 { TVec1 { x: self } } }
impl IVec1Args for TVec1<i32> { fn make(self) -> ivec1 { TVec1 { x: self.x } } }

pub trait IVec2Args { fn make(self) -> ivec2; }
pub fn ivec2<Args:IVec2Args>(args: Args) -> ivec2 { args.make() }

impl IVec2Args for i32 { fn make(self) -> ivec2 { TVec2 { x: self, y: self } } }

pub trait IVec3Args { fn make(self) -> ivec3; }
pub fn ivec3<Args:IVec3Args>(args: Args) -> ivec3 { args.make() }

impl IVec3Args for i32 { fn make(self) -> ivec3 { TVec3 { x: self, y: self, z: self } } }
impl IVec3Args for TVec3<i32> { fn make(self) -> ivec3 { TVec3 { x: self.x, y: self.y, z: self.z } } }

impl<T:Num + Clone> Decrement for TVec1<T> {
    fn postdecrement(&mut self) -> TVec1<T> {
        use std::num::One;
        let ret = TVec1{ x: self.x.clone() };
        self.x = self.x - One::one();
        ret
    }

    fn predecrement<'a>(&'a mut self) -> &'a mut TVec1<T> {
        use std::num::One;
        self.x = self.x - One::one();
        self
    }
}

impl<T:Num + Clone> Increment for TVec1<T> {
    fn postincrement(&mut self) -> TVec1<T> {
        use std::num::One;
        let ret = TVec1{ x: self.x.clone() };
        self.x = self.x + One::one();
        ret
    }

    fn preincrement<'a>(&'a mut self) -> &'a mut TVec1<T> {
        use std::num::One;
        self.x = self.x + One::one();
        self
    }
}

pub trait Vec2Args { fn make(self) -> vec2; }

pub fn vec2<Args:Vec2Args>(args: Args) -> vec2 { args.make() }

impl Vec2Args for f32 { fn make(self) -> vec2 { TVec2 { x: self, y: self } } }
impl Vec2Args for (f32,f32) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x, y: y } } }
impl Vec2Args for (int,int) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x as f32, y: y as f32 } } }
impl Vec2Args for (int,f32) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x as f32, y: y } } }
impl Vec2Args for (f32,int) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x, y: y as f32 } } }
impl Vec2Args for TVec2<f32> { fn make(self) -> vec2 { self } }

impl<T:Neg<T>> Neg<TVec2<T>> for TVec2<T> {
    fn neg(&self) -> TVec2<T> {
        TVec2 { x: -self.x, y: -self.y }
    }
}

impl<T:Num> DotProduct<T> for TVec2<T> {
    fn dot(&self, rhs: &TVec2<T>) -> T { self.x * rhs.x + self.y * rhs.y }
}

macro_rules! double_dispatch_T {
    ( $trait_:ident for $LHS_type:ident $method:ident via $RHS_trait:ident $rev_method:ident )
        => {
            pub trait $RHS_trait<T> {
                fn $rev_method(&self, lhs: & $LHS_type<T>) -> $LHS_type<T>;
            }

            impl<T,RHS:$RHS_trait<T>> $trait_<RHS,$LHS_type<T>> for $LHS_type<T> {
                fn $method(&self, rhs: &RHS) -> $LHS_type<T> { rhs.$rev_method(self) }
            }
        }
    ;
    ( $trait_:ident for mut $LHS_type:ident $method:ident via $RHS_trait:ident $rev_method:ident )
        => {
            pub trait $RHS_trait<T> {
                fn $rev_method(&self, lhs: &mut $LHS_type<T>);
            }

            impl<T,RHS:$RHS_trait<T>> $trait_<RHS> for $LHS_type<T> {
                fn $method(&mut self, rhs: &RHS) { rhs.$rev_method(self) }
            }
        }

}

double_dispatch_T!{Add for TVec2 add via TVec2AddRHS rev_add}
double_dispatch_T!{AddAssign for mut TVec2 add_assign via TVec2AddAssignRHS add_into }
double_dispatch_T!{Sub for TVec2 sub via TVec2SubRHS rev_sub}
double_dispatch_T!{SubAssign for mut TVec2 sub_assign via TVec2SubAssignRHS rsb_into }
double_dispatch_T!{Mul for TVec2 mul via TVec2MulRHS rev_mul}
double_dispatch_T!{MulAssign for mut TVec2 mul_assign via TVec2MulAssignRHS mul_into }
double_dispatch_T!{Div for TVec2 div via TVec2DivRHS rev_div}
double_dispatch_T!{DivAssign for mut TVec2 div_assign via TVec2DivAssignRHS div_into }

double_dispatch_T!{Add for TVec3 add via TVec3AddRHS rev_add}
double_dispatch_T!{Sub for TVec3 sub via TVec3SubRHS rev_sub}
double_dispatch_T!{Mul for TVec3 mul via TVec3MulRHS rev_mul}
double_dispatch_T!{Div for TVec3 div via TVec3DivRHS rev_div}
double_dispatch_T!{AddAssign for mut TVec3 add_assign via TVec3AddAssignRHS add_into }
double_dispatch_T!{SubAssign for mut TVec3 sub_assign via TVec3SubAssignRHS rsb_into }
double_dispatch_T!{MulAssign for mut TVec3 mul_assign via TVec3MulAssignRHS mul_into }
double_dispatch_T!{DivAssign for mut TVec3 div_assign via TVec3DivAssignRHS div_into }

double_dispatch_T!{Add for TVec4 add via TVec4AddRHS rev_add}
double_dispatch_T!{Sub for TVec4 sub via TVec4SubRHS rev_sub}
double_dispatch_T!{Mul for TVec4 mul via TVec4MulRHS rev_mul}
double_dispatch_T!{Div for TVec4 div via TVec4DivRHS rev_div}
double_dispatch_T!{AddAssign for mut TVec4 add_assign via TVec4AddAssignRHS add_into }
double_dispatch_T!{SubAssign for mut TVec4 sub_assign via TVec4SubAssignRHS rsb_into }
double_dispatch_T!{MulAssign for mut TVec4 mul_assign via TVec4MulAssignRHS mul_into }
double_dispatch_T!{DivAssign for mut TVec4 div_assign via TVec4DivAssignRHS div_into }

macro_rules! double_dispatch_usual_impls_2 {
    ( $ft:ty $op:ident
      $V_RHS_trait:ident { $v_rev_method:ident }
      $S_RHS_trait:ident { $s_rev_method:ident }
      $ASSIGN_RHS_trait:ident { $op_into_method:ident }
      ) => {
        impl $V_RHS_trait<$ft> for $ft {
            fn $v_rev_method(&self, lhs: &TVec2<$ft>) -> TVec2<$ft> {
                TVec2 { x: lhs.x.$op(self), y: lhs.y.$op(self) }
            }
        }

        impl $S_RHS_trait<$ft,TVec2<$ft>> for TVec2<$ft> {
            fn $s_rev_method(&self, lhs: &S<$ft>) -> TVec2<$ft> {
                let &S(lhs) = lhs;
                TVec2 { x: lhs.$op(&self.x), y: lhs.$op(&self.y) }
            }
        }

        impl $V_RHS_trait<$ft> for TVec2<$ft> {
            fn $v_rev_method(&self, lhs: &TVec2<$ft>) -> TVec2<$ft> {
                TVec2 { x: lhs.x.$op(&self.x), y: lhs.y.$op(&self.y) }
            }
        }

        impl $ASSIGN_RHS_trait<$ft> for $ft {
            fn $op_into_method(&self, recv: &mut TVec2<$ft>) {
                recv.x = recv.x.$op(self);
                recv.y = recv.y.$op(self);
            }
        }

        impl $ASSIGN_RHS_trait<$ft> for TVec2<$ft> {
            fn $op_into_method(&self, recv: &mut TVec2<$ft>) {
                recv.x = recv.x.$op(&self.x);
                recv.y = recv.y.$op(&self.y);
            }
        }
    }
}

double_dispatch_usual_impls_2! { f32 add
                                 TVec2AddRHS { rev_add } SAddRHS { rev_add }
                                 TVec2AddAssignRHS { add_into } }
double_dispatch_usual_impls_2! { f32 sub
                                 TVec2SubRHS { rev_sub } SSubRHS { rev_sub }
                                 TVec2SubAssignRHS { rsb_into } }
double_dispatch_usual_impls_2! { f32 mul
                                 TVec2MulRHS { rev_mul } SMulRHS { rev_mul }
                                 TVec2MulAssignRHS { mul_into } }
double_dispatch_usual_impls_2! { f32 div
                                 TVec2DivRHS { rev_div } SDivRHS { rev_div }
                                 TVec2DivAssignRHS { div_into } }

macro_rules! double_dispatch_usual_impls_3 {
    ( $ft:ty $op:ident
      $V_RHS_trait:ident { $v_rev_method:ident }
      $S_RHS_trait:ident { $s_rev_method:ident }
      $ASSIGN_RHS_trait:ident { $op_into_method:ident }
      ) => {
        impl $V_RHS_trait<$ft> for $ft {
            fn $v_rev_method(&self, lhs: &TVec3<$ft>) -> TVec3<$ft> {
                TVec3 { x: lhs.x.$op(self), y: lhs.y.$op(self), z: lhs.z.$op(self) }
            }
        }

        impl $S_RHS_trait<$ft,TVec3<$ft>> for TVec3<$ft> {
            fn $s_rev_method(&self, lhs: &S<$ft>) -> TVec3<$ft> {
                let &S(lhs) = lhs;
                TVec3 { x: lhs.$op(&self.x), y: lhs.$op(&self.y), z: lhs.$op(&self.z) }
            }
        }

        impl $V_RHS_trait<$ft> for TVec3<$ft> {
            fn $v_rev_method(&self, lhs: &TVec3<$ft>) -> TVec3<$ft> {
                TVec3 { x: lhs.x.$op(&self.x), y: lhs.y.$op(&self.y), z: lhs.z.$op(&self.z) }
            }
        }

        impl $ASSIGN_RHS_trait<$ft> for $ft {
            fn $op_into_method(&self, recv: &mut TVec3<$ft>) {
                recv.x = recv.x.$op(self);
                recv.y = recv.y.$op(self);
                recv.z = recv.z.$op(self);
            }
        }

        impl $ASSIGN_RHS_trait<$ft> for TVec3<$ft> {
            fn $op_into_method(&self, recv: &mut TVec3<$ft>) {
                recv.x = recv.x.$op(&self.x);
                recv.y = recv.y.$op(&self.y);
                recv.z = recv.z.$op(&self.z);
            }
        }
    }
}

double_dispatch_usual_impls_3! { f32 add
                                 TVec3AddRHS { rev_add } SAddRHS { rev_add }
                                 TVec3AddAssignRHS { add_into } }
double_dispatch_usual_impls_3! { f32 sub
                                 TVec3SubRHS { rev_sub } SSubRHS { rev_sub }
                                 TVec3SubAssignRHS { rsb_into } }
double_dispatch_usual_impls_3! { f32 mul
                                 TVec3MulRHS { rev_mul } SMulRHS { rev_mul }
                                 TVec3MulAssignRHS { mul_into } }
double_dispatch_usual_impls_3! { f32 div
                                 TVec3DivRHS { rev_div } SDivRHS { rev_div }
                                 TVec3DivAssignRHS { div_into } }

impl<T:Num + Clone> Decrement for TVec2<T> {
    fn postdecrement(&mut self) -> TVec2<T> {
        use std::num::One;
        let ret = TVec2{ x: self.x.clone(), y: self.y.clone() };
        self.x = self.x - One::one();
        self.y = self.y - One::one();
        ret
    }

    fn predecrement<'a>(&'a mut self) -> &'a mut TVec2<T> {
        use std::num::One;
        self.x = self.x - One::one();
        self.y = self.y - One::one();
        self
    }
}

impl<T:Num + Clone> Increment for TVec2<T> {
    fn postincrement(&mut self) -> TVec2<T> {
        use std::num::One;
        let ret = TVec2{ x: self.x.clone(), y: self.y.clone() };
        self.x = self.x + One::one();
        self.y = self.y + One::one();
        ret
    }

    fn preincrement<'a>(&'a mut self) -> &'a mut TVec2<T> {
        use std::num::One;
        self.x = self.x + One::one();
        self.y = self.y + One::one();
        self
    }
}

pub trait Vec3Args { fn make(self) -> vec3; }

pub fn vec3<Args:Vec3Args>(args: Args) -> vec3 { args.make() }

impl Vec3Args for f32 { fn make(self) -> vec3 { TVec3 { x: self, y: self, z: self } } }
impl Vec3Args for int { fn make(self) -> vec3 { TVec3 { x: self as f32, y: self as f32, z: self as f32 } } }
impl Vec3Args for (f32,f32,f32) { fn make(self) -> vec3 { let (x,y,z) = self; TVec3 { x: x, y: y, z: z } } }
impl Vec3Args for (int,int,int) { fn make(self) -> vec3 { let (x,y,z) = self; TVec3 { x: x as f32, y: y as f32, z: z as f32 } } }
impl Vec3Args for (int,f32,int) { fn make(self) -> vec3 { let (x,y,z) = self; TVec3 { x: x as f32, y: y, z: z as f32 } } }
impl Vec3Args for [int, ..3] { fn make(self) -> vec3 { let v = self; TVec3 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 } } }
impl Vec3Args for (vec2, f32) { fn make(self) -> vec3 { let (v,z) = self; TVec3 { x: v.x, y: v.y, z: z } } }
impl Vec3Args for (vec2, int) { fn make(self) -> vec3 { let (v,z) = self; TVec3 { x: v.x, y: v.y, z: z as f32 } } }
impl Vec3Args for (f32, vec2) { fn make(self) -> vec3 { let (x,v) = self; TVec3 { x: x, y: v.x, z: v.y } } }
impl Vec3Args for (int, vec2) { fn make(self) -> vec3 { let (x,v) = self; TVec3 { x: x as f32, y: v.x, z: v.y } } }
impl Vec3Args for vec4 { fn make(self) -> vec3 { let v = self; TVec3 { x: v.x, y: v.y, z: v.z } } }

impl<T:Neg<T>> Neg<TVec3<T>> for TVec3<T> {
    fn neg(&self) -> TVec3<T> {
        TVec3 { x: -self.x, y: -self.y, z: -self.z }
    }
}

impl<T:Num> DotProduct<T> for TVec3<T> {
    fn dot(&self, rhs: &TVec3<T>) -> T { self.x * rhs.x + self.y * rhs.y + self.z * rhs.z }
}

impl<T:Num + Clone> TVec3<T> {
    pub fn postdecrement(&mut self) -> TVec3<T> {
        use std::num::One;
        let ret = TVec3{ x: self.x.clone(), y: self.y.clone(), z: self.z.clone() };
        self.x = self.x - One::one();
        self.y = self.y - One::one();
        self.z = self.z - One::one();
        ret
    }

    pub fn predecrement<'a>(&'a mut self) -> &'a mut TVec3<T> {
        use std::num::One;
        self.x = self.x - One::one();
        self.y = self.y - One::one();
        self.z = self.z - One::one();
        self
    }

    pub fn postincrement(&mut self) -> TVec3<T> {
        use std::num::One;
        let ret = TVec3{ x: self.x.clone(), y: self.y.clone(), z: self.z.clone() };
        self.x = self.x + One::one();
        self.y = self.y + One::one();
        self.z = self.z + One::one();
        ret
    }

    pub fn preincrement<'a>(&'a mut self) -> &'a mut TVec3<T> {
        use std::num::One;
        self.x = self.x + One::one();
        self.y = self.y + One::one();
        self.z = self.z + One::one();
        self
    }
}

impl<T,RHS:TVec3AddAssignRHS<T>> TVec3<T> {
    /// Placeholder for an assumed future `+=` operator.
    pub fn add_assign(&mut self, rhs: &RHS) {
        rhs.add_into(self)
    }
}

pub trait Vec4Args { fn make(self) -> vec4; }
pub fn vec4<Args:Vec4Args>(args: Args) -> vec4 { args.make() }

impl Vec4Args for f32 { fn make(self) -> vec4 { TVec4 { x: self, y: self, z: self, w: self } } }
impl Vec4Args for int { fn make(self) -> vec4 { TVec4 { x: self as f32, y: self as f32, z: self as f32, w: self as f32 } } }
impl Vec4Args for (f32,f32,f32,f32) { fn make(self) -> vec4 { let (x,y,z,w) = self; TVec4 { x: x, y: y, z: z, w: w } } }
impl Vec4Args for (int,int,int,int) { fn make(self) -> vec4 { let (x,y,z,w) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
impl Vec4Args for [int, ..4] { fn make(self) -> vec4 { let v = self; TVec4 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32, w: v[3] as f32 } } }

impl<T:Num> DotProduct<T> for TVec4<T> {
    fn dot(&self, rhs: &TVec4<T>) -> T {
        self.x * rhs.x + self.y * rhs.y + self.z * rhs.z + self.w * rhs.w
    }
}

#[cfg(test)]
mod vec1_tests {
    #![allow(uppercase_variables)]
    use super::vec1;
    use super::ivec1;
    use super::{Increment};

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
    use super::{AddAssign,SubAssign,MulAssign,DivAssign};
    use super::{Increment, Decrement};

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

#[cfg(test)]
mod vec3_tests {
    #![allow(uppercase_variables)]
    use super::{vec2, vec3, vec4};
    use super::{ivec3};
    use super::dot;
    use super::S;
    use super::{SubAssign,MulAssign,DivAssign};
    use super::{Swizzle2,Swizzle3,Swizzle4};
    use super::{Increment,Decrement};

    #[test]
    fn test_ctor() {
        let A = vec3(1);
        let B = vec3(( 1, 1, 1 ));
        assert_eq!(A, B);

        let mut Tests = vec![];
        Tests.push(vec3((vec2((1,2)), 3)));
        Tests.push(vec3((1, vec2((2,3)))));
        Tests.push(vec3((1, 2, 3)));
        Tests.push(vec3(vec4((1, 2, 3, 4))));

        for v in Tests.iter() {
            assert_eq!(*v, vec3((1, 2, 3)));
        }
    }

    #[test]
    fn test_operators() {
        let A = vec3(1.0f32);
        let B = vec3(1.0f32);
        assert!(A == B);

        let A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = vec3((4.0f32, 5.0f32, 6.0f32));

        let C = A + B;
        assert_eq!(C, vec3((5, 7, 9)));

        let D = B - A;
        assert_eq!(D, vec3((3, 3, 3)));

        let E = A * B;
        assert_eq!(E, vec3((4, 10, 18)));

        let F = B / A;
        assert_eq!(F, vec3((4, 2.5f32, 2)));

        let G = A + 1.0f32;
        assert_eq!(G, vec3((2, 3, 4)));

        let H = B - 1.0f32;
        assert_eq!(H, vec3((3, 4, 5)));

        let I = A * 2.0f32;
        assert_eq!(I, vec3((2, 4, 6)));

        let J = B / 2.0f32;
        assert_eq!(J, vec3((2, 2.5f32, 3)));

        let K = S(1.0f32) + A;
        assert_eq!(K, vec3((2, 3, 4)));

        let L = S(1.0f32) - B;
        assert_eq!(L, vec3((-3, -4, -5)));

        let M = S(2.0f32) * A;
        assert_eq!(M, vec3((2, 4, 6)));

        let N = S(2.0f32) / B;
        assert_eq!(N, vec3((0.5f32, 2.0f32 / 5.0f32, 2.0f32 / 6.0f32)));

        let mut A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = vec3((4.0f32, 5.0f32, 6.0f32));

        A.add_assign(&B);
        assert_eq!(A, vec3((5, 7, 9)));

        A.add_assign(&1.0f32);
        assert_eq!(A, vec3((6, 8, 10)));

        let A = vec3((1.0f32, 2.0f32, 3.0f32));
        let mut B = vec3((4.0f32, 5.0f32, 6.0f32));

        B.sub_assign(&A);
        assert_eq!(B, vec3((3, 3, 3)));

        B.sub_assign(&1.0f32);
        assert_eq!(B, vec3((2, 2, 2)));

        let mut A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = vec3((4.0f32, 5.0f32, 6.0f32));

        A.mul_assign(&B);
        assert_eq!(A, vec3((4, 10, 18)));

        A.mul_assign(&2.0f32);
        assert_eq!(A, vec3((8, 20, 36)));

        let A = vec3((1.0f32, 2.0f32, 3.0f32));
        let mut B = vec3((4.0f32, 5.0f32, 6.0f32));

        B.div_assign(&A);
        assert_eq!(B, vec3((4, 2.5f32, 2)));

        B.div_assign(&2.0f32);
        assert_eq!(B, vec3((2, 1.25f32, 1)));

        let B = vec3(2.0f32);
        let mut B = B;
        let B_y = B.y;
        B.div_assign(&B_y);
        assert_eq!(B, vec3(1.0f32));

        let A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = -A;
        assert_eq!(B, vec3((-1.0f32, -2.0f32, -3.0f32)));

        let mut A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = A.predecrement();
        assert_eq!(*B, vec3((0.0f32, 1.0f32, 2.0f32)));

        let mut A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = A.postdecrement();
        assert_eq!(B, vec3((1.0f32, 2.0f32, 3.0f32)));
        assert_eq!(A, vec3((0.0f32, 1.0f32, 2.0f32)));

        let mut A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = A.preincrement();
        assert_eq!(*B, vec3((2.0f32, 3.0f32, 4.0f32)));

        let mut A = vec3((1.0f32, 2.0f32, 3.0f32));
        let B = A.postincrement();
        assert_eq!(B, vec3((1.0f32, 2.0f32, 3.0f32)));
        assert_eq!(A, vec3((2.0f32, 3.0f32, 4.0f32)));
    }

    #[test]
    fn test_swizzle_functions() {
        // vec2
        let a = vec2((1, 2));
        let b = vec2((10, 20));
        let r = dot(a, b);                       assert_eq!(r as int, 50);
        let r = dot(vec2(a.xy()), vec2(b.xy())); assert_eq!(r as int, 50);
        let r = dot(vec2(a.xy()), vec2(b.yy())); assert_eq!(r as int, 60);

        // vec3
        let u = vec3((1, 2, 3));
        let v = vec3((10, 20, 30));
        let r = dot(u, v);                       assert_eq!(r as int, 140);
        let r = dot(u.xyz(), v.zyz());           assert_eq!(r as int, 160);
        let r = dot(u, v.zyx());                 assert_eq!(r as int, 100);
        let r = dot(u.xyz(), v);                 assert_eq!(r as int, 140);
        let r = dot(u.xy(), v.xy());             assert_eq!(r as int, 50);

        // vec4
        let s = vec4((1, 2, 3, 4));
        let t = vec4((10, 20, 30, 40));
        let r = dot(s, t);                       assert_eq!(r as int, 300);
        let r = dot(s.xyzw(), t.xyzw());         assert_eq!(r as int, 300);
        let r = dot(s.xyz(), t.xyz());           assert_eq!(r as int, 140);
    }

    #[test]
    fn test_operator_increment() {
        let v0 = ivec3(1i32);
        let mut v1 = ivec3(v0);
        let mut v2 = ivec3(v0);
        let v3 = v1.preincrement();
        let v4 = v2.postincrement();
        let v1 = v1;
        let v2 = v2;

        assert_eq!(v0, v4);
        assert_eq!(v1, v2);
        assert_eq!(v1, *v3);

        let i0 = 1i32;
        let mut i1 = i0;
        let mut i2 = i0;
        let i3 = i1.preincrement();
        let i4 = i2.postincrement();
        let i1 = i1;
        let i2 = i2;

        assert_eq!(i0, i4);
        assert_eq!(i1, i2);
        assert_eq!(i1, *i3);
    }
}

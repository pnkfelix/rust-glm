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

macro_rules! swizzle_def {
    ( $Name:ident : $SuperTrait:ident
      { $(fn $core_name:ident)* }
      { $(($xy:ident $x2:ident $y2:ident))* }
      { $(($xyz:ident $x3:ident $y3:ident $z3:ident))* }
      { $(($xyzw:ident $x4:ident $y4:ident $z4:ident $w4:ident))* } )
        =>
    {
        pub trait $Name<T> : $SuperTrait<T>
        {
            $(fn $core_name(&self) -> T;)*
            $(fn $xy(&self) -> TVec2<T> {
                TVec2 { x: self.$x2(), y: self.$y2() } })*
            $(fn $xyz(&self) -> TVec3<T> {
                TVec3 { x: self.$x3(), y: self.$y3(), z: self.$z3() } })*
            $(fn $xyzw(&self) -> TVec4<T> {
                TVec4 { x: self.$x4(), y: self.$y4(), z: self.$z4(), w: self.$w4() } })*
        }
    }
}

// (trivial base case to ease macro definition.)
trait Swizzle0<T> { }
impl<T,X> Swizzle0<T> for X { }

swizzle_def!{
    Swizzle1 : Swizzle0 { fn x }
    { (xx x x) }
    { (xxx x x x) }
    { (xxxx x x x x) }
}

swizzle_def!{
    Swizzle2 : Swizzle1 { fn y }
    {          (xy x y)
      (yx y x) (yy y y)
    }
    {             (xxy x x y)
      (xyx x y x) (xyy x y y)

      (yxx y x x) (yxy y x y)
      (yyx y y x) (yyy y y y)
    }
    {                (xxxy x x x y)
      (xxyx x x y x) (xxyy x x y y)

      (xyxx x y x x) (xyxy x y x y)
      (xyyx x y y x) (xyyy x y y y)

                     (yxxy y x x y)
      (yxyx y x y x) (yxyy y x y y)

      (yyxx y y x x) (yyxy y y x y)
      (yyyx y y y x) (yyyy y y y y)
    }
}

swizzle_def!{
    Swizzle3 : Swizzle2 { fn z }
    {                   (xz x z)
                        (yz y z)
      (zx z x) (zy z y) (zz z z)
    }
    {                          (xxz x x z)
                               (xyz x y z)
       (xzx x z x) (xzy x z y) (xzz x z z)

                               (yxz y x z)
                               (yyz y y z)
       (yzx y z x) (yzy y z y) (yzz y z z)

       (zxx z x x) (zxy z x y) (zxz z x z)
       (zyx z y x) (zyy z y y) (zyz z y z)
       (zzx z z x) (zzy z z y) (zzz z z z)
    }
    {
                                     (xxxz x x x z)
                                     (xxyz x x y z)
       (xxzx x x z x) (xxzy x x z y) (xxzz x x z z)

                                     (xyxz x y x z)
                                     (xyyz x y y z)
       (xyzx x y z x) (xyzy x y z y) (xyzz x y z z)

       (xzxx x z x x) (xzxy x z x y) (xzxz x z x z)
       (xzyx x z y x) (xzyy x z y y) (xzyz x z y z)
       (xzzx x z z x) (xzzy x z z y) (xzzz x z z z)


                                     (yxxz y x x z)
                                     (yxyz y x y z)
       (yxzx y x z x) (yxzy y x z y) (yxzz y x z z)

                                     (yyxz y y x z)
                                     (yyyz y y y z)
       (yyzx y y z x) (yyzy y y z y) (yyzz y y z z)

       (yzxx y z x x) (yzxy y z x y) (yzxz y z x z)
       (yzyx y z y x) (yzyy y z y y) (yzyz y z y z)
       (yzzx y z z x) (yzzy y z z y) (yzzz y z z z)


       (zxxx z x x x) (zxxy z x x y) (zxxz z x x z)
       (zxyx z x y x) (zxyy z x y y) (zxyz z x y z)
       (zxzx z x z x) (zxzy z x z y) (zxzz z x z z)

       (zyxx z y x x) (zyxy z y x y) (zyxz z y x z)
       (zyyx z y y x) (zyyy z y y y) (zyyz z y y z)
       (zyzx z y z x) (zyzy z y z y) (zyzz z y z z)

       (zzxx z z x x) (zzxy z z x y) (zzxz z z x z)
       (zzyx z z y x) (zzyy z z y y) (zzyz z z y z)
       (zzzx z z z x) (zzzy z z z y) (zzzz z z z z)
    }
}

swizzle_def!{
    Swizzle4 : Swizzle3 { fn w }
    {                            (xw x w)
                                 (yw y w)
                                 (zw z w)
      (wx w x) (wy w y) (wz w z) (ww w w)
    }
    {                                     (xxw x x w)
                                          (xyw x y w)
                                          (xzw x z w)
      (xwx x w x) (xwy x w y) (xwz x w z) (xww x w w)

                                          (yxw y x w)
                                          (yyw y y w)
                                          (yzw y z w)
      (ywx y w x) (ywy y w y) (ywz y w z) (yww y w w)

                                          (zxw z x w)
                                          (zyw z y w)
                                          (zzw z z w)
      (zwx z w x) (zwy z w y) (zwz z w z) (zww z w w)

      (wxx w x x) (wxy w x y) (wxz w x z) (wxw w x w)
      (wyx w y x) (wyy w y y) (wyz w y z) (wyw w y w)
      (wzx w z x) (wzy w z y) (wzz w z z) (wzw w z w)
      (wwx w w x) (wwy w w y) (wwz w w z) (www w w w)
    }
    {                                              (xxxw x x x w)
                                                   (xxyw x x y w)
                                                   (xxzw x x z w)
      (xxwx x x w x) (xxwy x x w y) (xxwz x x w z) (xxww x x w w)

                                                   (xyxw x y x w)
                                                   (xyyw x y y w)
                                                   (xyzw x y z w)
      (xywx x y w x) (xywy x y w y) (xywz x y w z) (xyww x y w w)

                                                   (xzxw x z x w)
                                                   (xzyw x z y w)
                                                   (xzzw x z z w)
      (xzwx x z w x) (xzwy x z w y) (xzwz x z w z) (xzww x z w w)

      (xwxx x w x x) (xwxy x w x y) (xwxz x w x z) (xwxw x w x w)
      (xwyx x w y x) (xwyy x w y y) (xwyz x w y z) (xwyw x w y w)
      (xwzx x w z x) (xwzy x w z y) (xwzz x w z z) (xwzw x w z w)
      (xwwx x w w x) (xwwy x w w y) (xwwz x w w z) (xwww x w w w)


                                                   (yxxw y x x w)
                                                   (yxyw y x y w)
                                                   (yxzw y x z w)
      (yxwx y x w x) (yxwy y x w y) (yxwz y x w z) (yxww y x w w)

                                                   (yyxw y y x w)
                                                   (yyyw y y y w)
                                                   (yyzw y y z w)
      (yywx y y w x) (yywy y y w y) (yywz y y w z) (yyww y y w w)

                                                   (yzxw y z x w)
                                                   (yzyw y z y w)
                                                   (yzzw y z z w)
      (yzwx y z w x) (yzwy y z w y) (yzwz y z w z) (yzww y z w w)

      (ywxx y w x x) (ywxy y w x y) (ywxz y w x z) (ywxw y w x w)
      (ywyx y w y x) (ywyy y w y y) (ywyz y w y z) (ywyw y w y w)
      (ywzx y w z x) (ywzy y w z y) (ywzz y w z z) (ywzw y w z w)
      (ywwx y w w x) (ywwy y w w y) (ywwz y w w z) (ywww y w w w)


                                                   (zxxw z x x w)
                                                   (zxyw z x y w)
                                                   (zxzw z x z w)
      (zxwx z x w x) (zxwy z x w y) (zxwz z x w z) (zxww z x w w)

                                                   (zyxw z y x w)
                                                   (zyyw z y y w)
                                                   (zyzw z y z w)
      (zywx z y w x) (zywy z y w y) (zywz z y w z) (zyww z y w w)

                                                   (zzxw z z x w)
                                                   (zzyw z z y w)
                                                   (zzzw z z z w)
      (zzwx z z w x) (zzwy z z w y) (zzwz z z w z) (zzww z z w w)

      (zwxx z w x x) (zwxy z w x y) (zwxz z w x z) (zwxw z w x w)
      (zwyx z w y x) (zwyy z w y y) (zwyz z w y z) (zwyw z w y w)
      (zwzx z w z x) (zwzy z w z y) (zwzz z w z z) (zwzw z w z w)
      (zwwx z w w x) (zwwy z w w y) (zwwz z w w z) (zwww z w w w)


      (wxxx w x x x) (wxxy w x x y) (wxxz w x x z) (wxxw w x x w)
      (wxyx w x y x) (wxyy w x y y) (wxyz w x y z) (wxyw w x y w)
      (wxzx w x z x) (wxzy w x z y) (wxzz w x z z) (wxzw w x z w)
      (wxwx w x w x) (wxwy w x w y) (wxwz w x w z) (wxww w x w w)

      (wyxx w y x x) (wyxy w y x y) (wyxz w y x z) (wyxw w y x w)
      (wyyx w y y x) (wyyy w y y y) (wyyz w y y z) (wyyw w y y w)
      (wyzx w y z x) (wyzy w y z y) (wyzz w y z z) (wyzw w y z w)
      (wywx w y w x) (wywy w y w y) (wywz w y w z) (wyww w y w w)

      (wzxx w z x x) (wzxy w z x y) (wzxz w z x z) (wzxw w z x w)
      (wzyx w z y x) (wzyy w z y y) (wzyz w z y z) (wzyw w z y w)
      (wzzx w z z x) (wzzy w z z y) (wzzz w z z z) (wzzw w z z w)
      (wzwx w z w x) (wzwy w z w y) (wzwz w z w z) (wzww w z w w)

      (wwxx w w x x) (wwxy w w x y) (wwxz w w x z) (wwxw w w x w)
      (wwyx w w y x) (wwyy w w y y) (wwyz w w y z) (wwyw w w y w)
      (wwzx w w z x) (wwzy w w z y) (wwzz w w z z) (wwzw w w z w)
      (wwwx w w w x) (wwwy w w w y) (wwwz w w w z) (wwww w w w w)
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

impl<T:Clone> Swizzle1<T> for TVec2<T> { fn x(&self) -> T { self.x.clone() } }
impl<T:Clone> Swizzle2<T> for TVec2<T> { fn y(&self) -> T { self.y.clone() } }

impl<T:Clone> Swizzle1<T> for TVec3<T> { fn x(&self) -> T { self.x.clone() } }
impl<T:Clone> Swizzle2<T> for TVec3<T> { fn y(&self) -> T { self.y.clone() } }
impl<T:Clone> Swizzle3<T> for TVec3<T> { fn z(&self) -> T { self.z.clone() } }

impl<T:Clone> Swizzle1<T> for TVec4<T> { fn x(&self) -> T { self.x.clone() } }
impl<T:Clone> Swizzle2<T> for TVec4<T> { fn y(&self) -> T { self.y.clone() } }
impl<T:Clone> Swizzle3<T> for TVec4<T> { fn z(&self) -> T { self.z.clone() } }
impl<T:Clone> Swizzle4<T> for TVec4<T> { fn w(&self) -> T { self.w.clone() } }

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

macro_rules! impl_Vec2Args_for {
    ($a:ident,$b:ident) => {
        impl Vec2Args for ($a,$b) { fn make(self) -> vec2 { let (x,y) = self; TVec2 { x: x as f32, y: y as f32 } } }
    }
    ;
    ($a:ident 2 $S:ident) => {
        impl Vec2Args for $a { fn make(self) -> vec2 { let $S{x:x,y:y,..} = self; TVec2 { x: x as f32, y: y as f32 } } }
    }
    ;
    ($a:ident copy) => {
        impl Vec2Args for $a { fn make(self) -> vec2 { TVec2 { x: self as f32, y: self as f32 } } }
    }
}

impl_Vec2Args_for!(f32 copy)
impl_Vec2Args_for!(int,int)
impl_Vec2Args_for!(int,f32)
impl_Vec2Args_for!(f32,int)
impl_Vec2Args_for!(f32,f32)
impl_Vec2Args_for!(vec2 2 TVec2)
impl_Vec2Args_for!(vec3 2 TVec3)
impl_Vec2Args_for!(vec4 2 TVec4)

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

macro_rules! double_dispatch_usual_impls_4 {
    ( $ft:ty $op:ident
      $V_RHS_trait:ident { $v_rev_method:ident }
      $S_RHS_trait:ident { $s_rev_method:ident }
      $ASSIGN_RHS_trait:ident { $op_into_method:ident }
      ) => {
        impl $V_RHS_trait<$ft> for $ft {
            fn $v_rev_method(&self, lhs: &TVec4<$ft>) -> TVec4<$ft> {
                TVec4 { x: lhs.x.$op(self), y: lhs.y.$op(self), z: lhs.z.$op(self), w: lhs.w.$op(self) }
            }
        }

        impl $S_RHS_trait<$ft,TVec4<$ft>> for TVec4<$ft> {
            fn $s_rev_method(&self, lhs: &S<$ft>) -> TVec4<$ft> {
                let &S(lhs) = lhs;
                TVec4 { x: lhs.$op(&self.x), y: lhs.$op(&self.y), z: lhs.$op(&self.z), w: lhs.$op(&self.w) }
            }
        }

        impl $V_RHS_trait<$ft> for TVec4<$ft> {
            fn $v_rev_method(&self, lhs: &TVec4<$ft>) -> TVec4<$ft> {
                TVec4 { x: lhs.x.$op(&self.x), y: lhs.y.$op(&self.y), z: lhs.z.$op(&self.z), w: lhs.w.$op(&self.w) }
            }
        }

        impl $ASSIGN_RHS_trait<$ft> for $ft {
            fn $op_into_method(&self, recv: &mut TVec4<$ft>) {
                recv.x = recv.x.$op(self);
                recv.y = recv.y.$op(self);
                recv.z = recv.z.$op(self);
                recv.w = recv.w.$op(self);
            }
        }

        impl $ASSIGN_RHS_trait<$ft> for TVec4<$ft> {
            fn $op_into_method(&self, recv: &mut TVec4<$ft>) {
                recv.x = recv.x.$op(&self.x);
                recv.y = recv.y.$op(&self.y);
                recv.z = recv.z.$op(&self.z);
                recv.w = recv.w.$op(&self.w);
            }
        }
    }
}

double_dispatch_usual_impls_4! { f32 add
                                 TVec4AddRHS { rev_add } SAddRHS { rev_add }
                                 TVec4AddAssignRHS { add_into } }
double_dispatch_usual_impls_4! { f32 sub
                                 TVec4SubRHS { rev_sub } SSubRHS { rev_sub }
                                 TVec4SubAssignRHS { rsb_into } }
double_dispatch_usual_impls_4! { f32 mul
                                 TVec4MulRHS { rev_mul } SMulRHS { rev_mul }
                                 TVec4MulAssignRHS { mul_into } }
double_dispatch_usual_impls_4! { f32 div
                                 TVec4DivRHS { rev_div } SDivRHS { rev_div }
                                 TVec4DivAssignRHS { div_into } }

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

macro_rules! impl_Vec3Args_for {
    ($a:ident,$b:ident,$c:ident) => {
        impl Vec3Args for ($a,$b,$c) { fn make(self) -> vec3 { let (x,y,z) = self; TVec3 { x: x as f32, y: y as f32, z: z as f32 } } }
    }
    ;
    ($a:ident 2,$b:ident) => {
        impl Vec3Args for ($a,$b) { fn make(self) -> vec3 { let (TVec2{x:x,y:y},z) = self; TVec3 { x: x as f32, y: y as f32, z: z as f32 } } }
    }
    ;
    ($a:ident,$b:ident 2) => {
        impl Vec3Args for ($a,$b) { fn make(self) -> vec3 { let (x,TVec2{x:y,y:z}) = self; TVec3 { x: x as f32, y: y as f32, z: z as f32 } } }
    }
    ;
    ($a:ident 3 $S:ident) => {
        impl Vec3Args for $a { fn make(self) -> vec3 { let $S{x:x,y:y,z:z,..} = self; TVec3 { x: x as f32, y: y as f32, z: z as f32 } } }
    }
    ;
    ($a:ident copy) => {
        impl Vec3Args for $a { fn make(self) -> vec3 { TVec3 { x: self as f32, y: self as f32, z: self as f32 } } }
    }
}

impl_Vec3Args_for!(f32 copy)
impl_Vec3Args_for!(int copy)
impl_Vec3Args_for!(int,int,int)
impl_Vec3Args_for!(int,int,f32)
impl_Vec3Args_for!(int,f32,int)
impl_Vec3Args_for!(int,f32,f32)
impl_Vec3Args_for!(f32,int,f32)
impl_Vec3Args_for!(f32,f32,int)
impl_Vec3Args_for!(f32,f32,f32)
impl_Vec3Args_for!(vec2 2,f32)
impl_Vec3Args_for!(vec2 2,int)
impl_Vec3Args_for!(f32,vec2 2)
impl_Vec3Args_for!(int,vec2 2)
impl_Vec3Args_for!(vec3 3 TVec3)
impl_Vec3Args_for!(vec4 3 TVec4)
impl Vec3Args for [int, ..3] { fn make(self) -> vec3 { let v = self; TVec3 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 } } }

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

pub trait Vec4Args { fn make(self) -> vec4; }
pub fn vec4<Args:Vec4Args>(args: Args) -> vec4 { args.make() }

macro_rules! impl_Vec4Args_for {
    ($a:ident,$b:ident,$c:ident,$d:ident) => {
        impl Vec4Args for ($a,$b,$c,$d) { fn make(self) -> vec4 { let (x,y,z,w) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident 2,$b:ident,$c:ident) => {
        impl Vec4Args for ($a,$b,$c) { fn make(self) -> vec4 { let (TVec2{x:x,y:y},z,w) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident,$b:ident 2,$c:ident) => {
        impl Vec4Args for ($a,$b,$c) { fn make(self) -> vec4 { let (x,TVec2{x:y,y:z},w) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident,$b:ident,$c:ident 2) => {
        impl Vec4Args for ($a,$b,$c) { fn make(self) -> vec4 { let (x,y,TVec2{x:z,y:w}) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident 2,$b:ident 2) => {
        impl Vec4Args for ($a,$b) { fn make(self) -> vec4 { let (TVec2{x:x,y:y},TVec2{x:z,y:w}) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident 3,$b:ident) => {
        impl Vec4Args for ($a,$b) { fn make(self) -> vec4 { let (TVec3{x:x,y:y,z:z},w) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident,$b:ident 3) => {
        impl Vec4Args for ($a,$b) { fn make(self) -> vec4 { let (x,TVec3{x:y,y:z,z:w}) = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident 4 $S:ident) => {
        impl Vec4Args for $a { fn make(self) -> vec4 { let $S{x:x,y:y,z:z,w:w,..} = self; TVec4 { x: x as f32, y: y as f32, z: z as f32, w: w as f32 } } }
    }
    ;
    ($a:ident copy) => {
        impl Vec4Args for $a { fn make(self) -> vec4 { TVec4 { x: self as f32, y: self as f32, z: self as f32, w: self as f32 } } }
    }
}


impl Vec4Args for [int, ..4] { fn make(self) -> vec4 { let v = self; TVec4 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32, w: v[3] as f32 } } }

impl_Vec4Args_for!(f32 copy)
impl_Vec4Args_for!(int copy)
impl_Vec4Args_for!(int,int,int,int)
impl_Vec4Args_for!(int,int,int,f32)
impl_Vec4Args_for!(int,int,f32,int)
impl_Vec4Args_for!(int,int,f32,f32)
impl_Vec4Args_for!(int,f32,int,int)
impl_Vec4Args_for!(int,f32,int,f32)
impl_Vec4Args_for!(int,f32,f32,int)
impl_Vec4Args_for!(int,f32,f32,f32)
impl_Vec4Args_for!(f32,int,int,int)
impl_Vec4Args_for!(f32,int,int,f32)
impl_Vec4Args_for!(f32,int,f32,int)
impl_Vec4Args_for!(f32,int,f32,f32)
impl_Vec4Args_for!(f32,f32,int,int)
impl_Vec4Args_for!(f32,f32,int,f32)
impl_Vec4Args_for!(f32,f32,f32,int)
impl_Vec4Args_for!(f32,f32,f32,f32)
impl_Vec4Args_for!(vec2 2,int,int)
impl_Vec4Args_for!(int, vec2 2,int)
impl_Vec4Args_for!(int, int, vec2 2)
impl_Vec4Args_for!(vec2 2,vec2 2)
impl_Vec4Args_for!(int, vec3 3)
impl_Vec4Args_for!(vec3 3, int)
impl_Vec4Args_for!(vec4 4 TVec4)

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
    use super::{AddAssign,SubAssign,MulAssign,DivAssign};
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

#[cfg(test)]
mod vec4_tests {
    #![allow(uppercase_variables)]

    use super::{vec2,vec3,vec4};
    use super::S;

    #[test]
    fn test_ctor() {
        let A = vec4(1);
        let B = vec4((1, 1, 1, 1));
        assert_eq!(A, B);

        let mut Tests = vec![];
        Tests.push(vec4((vec2((1, 2)), 3, 4)));
        Tests.push(vec4((1, vec2((2, 3)), 4)));
        Tests.push(vec4((1, 2, vec2((3, 4)))));
        Tests.push(vec4((vec3((1, 2, 3)), 4)));
        Tests.push(vec4((1, vec3((2, 3, 4)))));
        Tests.push(vec4((vec2((1, 2)), vec2((3, 4)))));
        Tests.push(vec4((1, 2, 3, 4)));
        Tests.push(vec4(vec4((1, 2, 3, 4))));

        for v in Tests.iter() {
            assert_eq!(*v, vec4((1, 2, 3, 4)));
        }
    }

    #[test]
    fn test_operators() {
        let A = vec4(1.0f32);
        let B = vec4(1.0f32);
        assert!(A == B);

        let A = vec4((1f32, 2f32, 3f32, 4f32));
        let B = vec4((4f32, 5f32, 6f32, 7f32));
        let C = A + B;
        assert_eq!(C, vec4((5, 7, 9, 11)));
        let D = B - A;
        assert_eq!(D, vec4((3, 3, 3, 3)));
        let E = A * B;
        assert_eq!(E, vec4((4, 10, 18, 28)));
        let F = B / A;
        assert_eq!(F, vec4((4, 2.5f32, 2, 7f32 / 4f32)));
        let G = A + 1f32;
        assert_eq!(G, vec4((2, 3, 4, 5)));
        let H = B - 1f32;
        assert_eq!(H, vec4((3, 4, 5, 6)));
        let I = A * 2f32;
        assert_eq!(I, vec4((2, 4, 6, 8)));
        let J = B / 2f32;
        assert_eq!(J, vec4((2, 2.5f32, 3, 3.5f32)));
        let K = S(1f32) + A;
        assert_eq!(K, vec4((2, 3, 4, 5)));
        let L = S(1f32) - B;
        assert_eq!(L, vec4((-3, -4, -5, -6)));
        let M = S(2f32) * A;
        assert_eq!(M, vec4((2, 4, 6, 8)));
        let N = S(2f32) / B;
        assert_eq!(N, vec4((0.5f32, 2f32 / 5f32, 2f32 / 6f32, 2f32 / 7f32)));
    }
}

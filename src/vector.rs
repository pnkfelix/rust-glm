// Copyright 2014 Felix S. Klock II. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::typedefs::{vec1,vec2,vec3,vec4};
use super::typedefs::{ivec1,ivec2,ivec3,ivec4};
use src::scalar::{S,SAddRHS,SSubRHS,SMulRHS,SDivRHS};
use src::operators::{EpsilonEq};
use src::operators::{Increment,Decrement};
use src::operators::{AddAssign,SubAssign,MulAssign,DivAssign};

pub trait DotProduct<T> {
    fn dot(&self, &Self) -> T;
}

pub fn dot<T,V:DotProduct<T>>(x: V, y: V) -> T { x.dot(&y) }

pub trait Summable<T> {
    fn sum(&self) -> T;
}

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

#[deriving(PartialEq, Eq, Show, Default, Clone)]
pub struct TVec1<T> { pub x: T, }
#[deriving(PartialEq, Eq, Show, Default, Clone)]
pub struct TVec2<T> { pub x: T, pub y: T, }
#[deriving(PartialEq, Eq, Show, Default, Clone)]
pub struct TVec3<T> { pub x: T, pub y: T, pub z: T, }
#[deriving(PartialEq, Eq, Show, Default, Clone)]
pub struct TVec4<T> { pub x: T, pub y: T, pub z: T, pub w: T, }

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

pub trait IVec4Args { fn make(self) -> ivec4; }
pub fn ivec4<Args:IVec4Args>(args: Args) -> ivec4 { args.make() }

impl IVec4Args for i32 { fn make(self) -> ivec4 { TVec4 { x: self, y: self, z: self, w: self } } }
impl IVec4Args for TVec4<i32> { fn make(self) -> ivec4 { TVec4 { x: self.x, y: self.y, z: self.z, w: self.w } } }


macro_rules! impl_IncrementDecrement_for {
    ($TVec:ident $($x:ident),*) => {
        impl<T:Num + Clone> Increment for $TVec<T> {
            fn postincrement(&mut self) -> $TVec<T> {
                use std::num::One;
                let ret = $TVec{ $( $x: self.$x.clone() ),* };
                $( self.$x = self.$x + One::one(); )*
                ret
            }

            fn preincrement<'a>(&'a mut self) -> &'a mut $TVec<T> {
                use std::num::One;
                $( self.$x = self.$x + One::one(); )*
                self
            }
        }

        impl<T:Num + Clone> Decrement for $TVec<T> {
            fn postdecrement(&mut self) -> $TVec<T> {
                use std::num::One;
                let ret = $TVec{ $( $x: self.$x.clone() ),* };
                $( self.$x = self.$x - One::one(); )*
                ret
            }

            fn predecrement<'a>(&'a mut self) -> &'a mut $TVec<T> {
                use std::num::One;
                $( self.$x = self.$x - One::one(); )*
                self
            }
        }
    }
}

impl_IncrementDecrement_for!{ TVec1 x }
impl_IncrementDecrement_for!{ TVec2 x, y }
impl_IncrementDecrement_for!{ TVec3 x, y, z }
impl_IncrementDecrement_for!{ TVec4 x, y, z, w }

macro_rules! impl_Neg_for {
    ($TVec:ident $($x:ident),*) => {
        impl<T:Neg<T>> Neg<$TVec<T>> for $TVec<T> {
            fn neg(&self) -> $TVec<T> { $TVec{ $( $x: -self.$x ),* } }
        }
    }
}

impl_Neg_for!{ TVec1 x }
impl_Neg_for!{ TVec2 x, y }
impl_Neg_for!{ TVec3 x, y, z }
impl_Neg_for!{ TVec4 x, y, z, w }

impl<T:Num+Clone> Summable<T> for TVec1<T> {
    fn sum(&self) -> T { self.x.clone() }
}
impl<T:Num+Clone> Summable<T> for TVec2<T> {
    fn sum(&self) -> T { self.x + self.y }
}
impl<T:Num+Clone> Summable<T> for TVec3<T> {
    fn sum(&self) -> T { self.x + self.y + self.z } 
}
impl<T:Num+Clone> Summable<T> for TVec4<T> {
    fn sum(&self) -> T { self.x + self.y + self.z + self.w }
}

macro_rules! impl_DotProduct_for {
    ($TVec:ident $($x:ident),*) => {
        impl<T:Num> DotProduct<T> for $TVec<T> {
            fn dot(&self, rhs: &$TVec<T>) -> T {
                use std::num::Zero;
                let mut sum : T = Zero::zero();
                { $( sum = sum + self.$x * rhs.$x; )* }
                sum
            }
        }
    }
}

impl_DotProduct_for!{ TVec1 x }
impl_DotProduct_for!{ TVec2 x, y }
impl_DotProduct_for!{ TVec3 x, y, z }
impl_DotProduct_for!{ TVec4 x, y, z, w }

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
        impl $V_RHS_trait<$ft,TVec2<$ft>> for $ft {
            fn $v_rev_method(&self, lhs: &TVec2<$ft>) -> TVec2<$ft> {
                TVec2 { x: lhs.x.$op(self), y: lhs.y.$op(self) }
            }
        }

        impl<FT:Num> $S_RHS_trait<FT,TVec2<FT>> for TVec2<FT> {
            fn $s_rev_method(&self, lhs: &S<FT>) -> TVec2<FT> {
                let &S(ref lhs) = lhs;
                TVec2 { x: lhs.$op(&self.x), y: lhs.$op(&self.y) }
            }
        }

        impl<FT:Num> $V_RHS_trait<FT,TVec2<FT>> for TVec2<FT> {
            fn $v_rev_method(&self, lhs: &TVec2<FT>) -> TVec2<FT> {
                TVec2 { x: lhs.x.$op(&self.x), y: lhs.y.$op(&self.y) }
            }
        }

        impl $ASSIGN_RHS_trait<$ft> for $ft {
            fn $op_into_method(&self, recv: &mut TVec2<$ft>) {
                recv.x = recv.x.$op(self);
                recv.y = recv.y.$op(self);
            }
        }

        impl<FT:Num> $ASSIGN_RHS_trait<FT> for TVec2<FT> {
            fn $op_into_method(&self, recv: &mut TVec2<FT>) {
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
        impl $V_RHS_trait<$ft,TVec3<$ft>> for $ft {
            fn $v_rev_method(&self, lhs: &TVec3<$ft>) -> TVec3<$ft> {
                TVec3 { x: lhs.x.$op(self), y: lhs.y.$op(self), z: lhs.z.$op(self) }
            }
        }

        impl<FT:Num> $S_RHS_trait<FT,TVec3<FT>> for TVec3<FT> {
            fn $s_rev_method(&self, lhs: &S<FT>) -> TVec3<FT> {
                let &S(ref lhs) = lhs;
                TVec3 { x: lhs.$op(&self.x), y: lhs.$op(&self.y), z: lhs.$op(&self.z) }
            }
        }

        impl<FT:Num> $V_RHS_trait<FT,TVec3<FT>> for TVec3<FT> {
            fn $v_rev_method(&self, lhs: &TVec3<FT>) -> TVec3<FT> {
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
        impl $V_RHS_trait<$ft,TVec4<$ft>> for $ft {
            fn $v_rev_method(&self, lhs: &TVec4<$ft>) -> TVec4<$ft> {
                TVec4 { x: lhs.x.$op(self), y: lhs.y.$op(self), z: lhs.z.$op(self), w: lhs.w.$op(self) }
            }
        }

        impl<FT:Num> $S_RHS_trait<FT,TVec4<FT>> for TVec4<FT> {
            fn $s_rev_method(&self, lhs: &S<FT>) -> TVec4<FT> {
                let &S(ref lhs) = lhs;
                TVec4 { x: lhs.$op(&self.x), y: lhs.$op(&self.y), z: lhs.$op(&self.z), w: lhs.$op(&self.w) }
            }
        }

        impl<FT:Num> $V_RHS_trait<FT,TVec4<FT>> for TVec4<FT> {
            fn $v_rev_method(&self, lhs: &TVec4<FT>) -> TVec4<FT> {
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

        impl<FT:Num> $ASSIGN_RHS_trait<FT> for TVec4<FT> {
            fn $op_into_method(&self, recv: &mut TVec4<FT>) {
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

macro_rules! impl_Vec3Args_for_choice {
    ( $a:ident, $b:ident, $c:ident, $($ignore:ident),*) => {
        impl_Vec3Args_for!($a, $b, $c)
    }
}

impl_Vec3Args_for!(f32 copy)
impl_Vec3Args_for!(int copy)

all_choices!( impl_Vec3Args_for_choice :
              todo: { (int | f32) (int | f32) (int | f32) }
              done: { (ignored) })

impl_Vec3Args_for!(vec2 2,f32)
impl_Vec3Args_for!(vec2 2,int)
impl_Vec3Args_for!(f32,vec2 2)
impl_Vec3Args_for!(int,vec2 2)
impl_Vec3Args_for!(vec3 3 TVec3)
impl_Vec3Args_for!(vec4 3 TVec4)
impl Vec3Args for [int, ..3] { fn make(self) -> vec3 { let v = self; TVec3 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 } } }

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

macro_rules! impl_Vec4Args_for_choice {
    ( $a:ident, $b:ident, $c:ident, $d:ident, $($ignore:ident),*) => {
        impl_Vec4Args_for!($a, $b, $c, $d)
    }
}

all_choices!( impl_Vec4Args_for_choice :
              todo: { (int | f32) (int | f32) (int | f32) (int | f32) }
              done: { (ignored) } )

impl_Vec4Args_for!(vec2 2,int,int)
impl_Vec4Args_for!(vec2 2,int,f32)
impl_Vec4Args_for!(vec2 2,f32,int)
impl_Vec4Args_for!(vec2 2,f32,f32)
impl_Vec4Args_for!(int, vec2 2,int)
impl_Vec4Args_for!(int, vec2 2,f32)
impl_Vec4Args_for!(f32, vec2 2,int)
impl_Vec4Args_for!(f32, vec2 2,f32)
impl_Vec4Args_for!(int, int, vec2 2)
impl_Vec4Args_for!(int, f32, vec2 2)
impl_Vec4Args_for!(f32, int, vec2 2)
impl_Vec4Args_for!(f32, f32, vec2 2)
impl_Vec4Args_for!(vec2 2,vec2 2)
impl_Vec4Args_for!(int, vec3 3)
impl_Vec4Args_for!(f32, vec3 3)
impl_Vec4Args_for!(vec3 3, int)
impl_Vec4Args_for!(vec3 3, f32)
impl_Vec4Args_for!(vec4 4 TVec4)

macro_rules! vec1 { ($($arg:expr),*) => { vec1(($($arg),*)) } }
macro_rules! vec2 { ($($arg:expr),*) => { vec2(($($arg),*)) } }
macro_rules! vec3 { ($($arg:expr),*) => { vec3(($($arg),*)) } }
macro_rules! vec4 { ($($arg:expr),*) => { vec4(($($arg),*)) } }

macro_rules! impl_EpsilonEq_for {
    ($TVec:ident $($x:ident),*) => {
        impl<T:Num+PartialOrd> EpsilonEq<$TVec<T>> for $TVec<T> {
            fn epsilon_eq(&self, rhs: &$TVec<T>, epsilons: &$TVec<T>) -> bool {
                let mut result = true;
                { $( result = result && ((self.$x - rhs.$x) < epsilons.$x); )* }
                return result;
            }
        }
    }
}

impl_EpsilonEq_for!(TVec1 x)
impl_EpsilonEq_for!(TVec2 x, y)
impl_EpsilonEq_for!(TVec3 x, y, z)
impl_EpsilonEq_for!(TVec4 x, y, z, w)


#[cfg(test)]
mod vec1_tests {
    #![allow(non_snake_case)] // #![allow(uppercase_variables)]
    use super::vec1;
    use super::ivec1;

    use src::operators::{Increment};

    #[test]
    fn test_operators() {
        let A = vec1!(1.0f32);
        let B = vec1!(2.0f32);
        assert!(A != B);
    }

    #[test]
    fn test_operator_increment() {
        let v0 = ivec1(1i32);
        let mut v1 = ivec1(v0);
        let mut v2 = ivec1(v0);
        let v3 = *v1.preincrement();
        let v4 = v2.postincrement();
        let v1 = v1;
        let v2 = v2;

        assert_eq!(v0, v4);
        assert_eq!(v1, v2);
        assert_eq!(v1, v3);
    }
}

#[cfg(test)]
mod vec2_tests {
    #![allow(non_snake_case)] // #![allow(uppercase_variables)]
    use super::vec2;

    use src::operators::{AddAssign,SubAssign,MulAssign,DivAssign};
    use src::operators::{Increment, Decrement};
    use src::scalar::S;

    #[test]
    fn test_operators() {
        let A = vec2!(1.0f32);
        let B = vec2!(1.0f32);
        assert!(A == B);

        let A = vec2!(1.0f32);
        let C = A + 1.0f32;
        let mut A = A;
        A.add_assign(&1.0f32);
        assert!(A.x == 2.0 && A.y == 2.0);
        assert!(A == C);

        let A = vec2!(1.0f32);
        let B = vec2!(2.0f32, -1.0f32);
        let C = A + B;
        let mut A = A;
        A.add_assign(&B);
        assert!(A.x == 3.0 && A.y == 0.0);
        assert!(A == C);

        let A = vec2!(1.0f32);
        let C = A - 1.0f32;
        let mut A = A;
        A.sub_assign(&1.0f32);
        assert!(A.x == 0.0 && A.y == 0.0);
        assert!(A == C);

        let A = vec2!(1.0f32);
        let C = A * 2.0f32;
        let mut A = A;
        A.mul_assign(&2.0f32);
        assert!(A.x == 2.0 && A.y == 2.0);
        assert!(A == C);

        let A = vec2!(2.0f32);
        let B = vec2!(2.0f32);
        let C = A / B;
        let mut A = A;
        A.div_assign(&B);
        assert!(A.x == 1.0 && A.y == 1.0);
        assert!(A == C);

        let A = vec2!(1.0f32, 2.0f32);
        let B = vec2!(4.0f32, 5.0f32);
        let C = A + B;
        assert_eq!(C, vec2!( 5i,  7i ));

        let D = B - A;
        assert_eq!(D, vec2!( 3i,  3i ));

        let E = A * B;
        assert_eq!(E, vec2!( 4i, 10i ));

        let F = B / A;
        assert_eq!(F, vec2!( 4i, 2.5f32 ));

        let G = A + 1.0f32;
        assert_eq!(G, vec2!( 2i,  3i ));

        let H = B - 1.0f32;
        assert_eq!(H, vec2!( 3i,  4i ));

        let I = A * 2.0f32;
        assert_eq!(I, vec2!( 2i,  4i ));

        let J = B / 2.0f32;
        assert_eq!(J, vec2!( 2i, 2.5f32 ));

        let K = S(1.0f32) + A;
        assert_eq!(K, vec2!( 2i,  3i ));

        let L = S(1.0f32) - B;
        assert_eq!(L, vec2!(-3i, -4i ));

        let M = S(2.0f32) * A;
        assert_eq!(M, vec2!( 2i,  4i ));

        let N = S(2.0f32) / B;
        assert_eq!(N, vec2!( 0.5f32,  2.0f32/5.0f32 ));

        let A = vec2!( 1i,  2i );
        let B = vec2!( 4i,  5i );
        let mut A = A;
        A.add_assign(&B);
        assert_eq!(A, vec2!( 5i, 7i ));

        A.add_assign(&1.0f32);
        assert_eq!(A, vec2!( 6i, 8i ));

        let A = vec2!( 1i,  2i );
        let B = vec2!( 4i,  5i );
        let mut B = B;
        B.sub_assign(&A);
        assert_eq!(B, vec2!( 3i, 3i ));

        B.sub_assign(&1.0f32);
        assert_eq!(B, vec2!( 2i, 2i ));

        let A = vec2!( 1i,  2i );
        let B = vec2!( 4i,  5i );
        let mut A = A;
        A.mul_assign(&B);
        assert_eq!(A, vec2!( 4i, 10i ));

        A.mul_assign(&2.0f32);
        assert_eq!(A, vec2!( 8i, 20i ));

        let A = vec2!( 1i,  2i );
        let B = vec2!( 4i,  5i );
        let mut B = B;
        B.div_assign(&A);
        assert_eq!(B, vec2!( 4i, 2.5f32 ));

        B.div_assign(&2.0f32);
        assert_eq!(B, vec2!( 2i, 1.25f32 ));

        let B = vec2!(2.0f32);
        let mut B = B;
        let B_y = B.y;
        B.div_assign(&B_y);
        assert_eq!(B, vec2!( 1.0f32 ));

        let A = vec2!( 1.0f32, 2.0f32 );
        let B = -A;
        assert_eq!(B, vec2!( -1.0f32, -2.0f32 ));
    }

    #[test]
    fn test_incr_decr_operators() {
        let A = vec2!( 1.0f32, 2.0f32 );
        let mut A = A;
        {
            let B = A.predecrement(); // Rust does not have operator--(int).
            assert_eq!(*B, vec2!( 0.0f32, 1.0f32 ));
        }
        assert_eq!(A, vec2!( 0.0f32, 1.0f32 ));

        let A = vec2!( 1.0f32, 2.0f32 );
        let mut A = A;
        let B = A.postdecrement(); // Rust does not have operator--().
        assert_eq!(B, vec2!( 1.0f32, 2.0f32 ));
        assert_eq!(A, vec2!( 0.0f32, 1.0f32 ));

        let A = vec2!( 1.0f32, 2.0f32 );
        let mut A = A;
        {
            let B = A.preincrement(); // Rust does not have operator++(int).
            assert_eq!(*B, vec2!( 2.0f32, 3.0f32 ));
        }
        assert_eq!(A, vec2!( 2.0f32, 3.0f32 ));

        let A = vec2!( 1.0f32, 2.0f32 );
        let mut A = A;
        let B = A.postincrement(); // Rust does not have operator++().
        assert_eq!(B, vec2!( 1.0f32, 2.0f32 ));
        assert_eq!(A, vec2!( 2.0f32, 3.0f32 ));

    }
}

#[cfg(test)]
mod vec3_tests {
    #![allow(non_snake_case)] // #![allow(uppercase_variables)]
    use super::{vec2, vec3, vec4};
    use super::{ivec3};
    use super::dot;
    use super::{Swizzle2,Swizzle3,Swizzle4};

    use src::operators::{Increment,Decrement};
    use src::operators::{AddAssign,SubAssign,MulAssign,DivAssign};
    use src::scalar::S;

    #[test]
    fn test_ctor() {
        let A = vec3!(1i);
        let B = vec3!( 1i, 1i, 1i );
        assert_eq!(A, B);

        let mut Tests = vec![];
        Tests.push(vec3!(vec2!(1i,2i), 3i));
        Tests.push(vec3!(1i, vec2!(2i,3i)));
        Tests.push(vec3!(1i, 2i, 3i));
        Tests.push(vec3!(vec4!(1i, 2i, 3i, 4i)));

        for v in Tests.iter() {
            assert_eq!(*v, vec3!(1i, 2i, 3i));
        }
    }

    #[test]
    fn test_operators() {
        let A = vec3!(1.0f32);
        let B = vec3!(1.0f32);
        assert!(A == B);

        let A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = vec3!(4.0f32, 5.0f32, 6.0f32);

        let C = A + B;
        assert_eq!(C, vec3!(5i, 7i, 9i));

        let D = B - A;
        assert_eq!(D, vec3!(3i, 3i, 3i));

        let E = A * B;
        assert_eq!(E, vec3!(4i, 10i, 18i));

        let F = B / A;
        assert_eq!(F, vec3!(4i, 2.5f32, 2i));

        let G = A + 1.0f32;
        assert_eq!(G, vec3!(2i, 3i, 4i));

        let H = B - 1.0f32;
        assert_eq!(H, vec3!(3i, 4i, 5i));

        let I = A * 2.0f32;
        assert_eq!(I, vec3!(2i, 4i, 6i));

        let J = B / 2.0f32;
        assert_eq!(J, vec3!(2i, 2.5f32, 3i));

        let K = S(1.0f32) + A;
        assert_eq!(K, vec3!(2i, 3i, 4i));

        let L = S(1.0f32) - B;
        assert_eq!(L, vec3!(-3i, -4i, -5i));

        let M = S(2.0f32) * A;
        assert_eq!(M, vec3!(2i, 4i, 6i));

        let N = S(2.0f32) / B;
        assert_eq!(N, vec3!(0.5f32, 2.0f32 / 5.0f32, 2.0f32 / 6.0f32));

        let mut A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = vec3!(4.0f32, 5.0f32, 6.0f32);

        A.add_assign(&B);
        assert_eq!(A, vec3!(5i, 7i, 9i));

        A.add_assign(&1.0f32);
        assert_eq!(A, vec3!(6i, 8i, 10i));

        let A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let mut B = vec3!(4.0f32, 5.0f32, 6.0f32);

        B.sub_assign(&A);
        assert_eq!(B, vec3!(3i, 3i, 3i));

        B.sub_assign(&1.0f32);
        assert_eq!(B, vec3!(2i, 2i, 2i));

        let mut A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = vec3!(4.0f32, 5.0f32, 6.0f32);

        A.mul_assign(&B);
        assert_eq!(A, vec3!(4i, 10i, 18i));

        A.mul_assign(&2.0f32);
        assert_eq!(A, vec3!(8i, 20i, 36i));

        let A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let mut B = vec3!(4.0f32, 5.0f32, 6.0f32);

        B.div_assign(&A);
        assert_eq!(B, vec3!(4i, 2.5f32, 2i));

        B.div_assign(&2.0f32);
        assert_eq!(B, vec3!(2i, 1.25f32, 1i));

        let B = vec3!(2.0f32);
        let mut B = B;
        let B_y = B.y;
        B.div_assign(&B_y);
        assert_eq!(B, vec3!(1.0f32));

        let A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = -A;
        assert_eq!(B, vec3!(-1.0f32, -2.0f32, -3.0f32));

        let mut A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = A.predecrement();
        assert_eq!(*B, vec3!(0.0f32, 1.0f32, 2.0f32));

        let mut A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = A.postdecrement();
        assert_eq!(B, vec3!(1.0f32, 2.0f32, 3.0f32));
        assert_eq!(A, vec3!(0.0f32, 1.0f32, 2.0f32));

        let mut A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = A.preincrement();
        assert_eq!(*B, vec3!(2.0f32, 3.0f32, 4.0f32));

        let mut A = vec3!(1.0f32, 2.0f32, 3.0f32);
        let B = A.postincrement();
        assert_eq!(B, vec3!(1.0f32, 2.0f32, 3.0f32));
        assert_eq!(A, vec3!(2.0f32, 3.0f32, 4.0f32));
    }

    #[test]
    fn test_swizzle_functions() {
        // vec2
        let a = vec2!(1i, 2i);
        let b = vec2!(10i, 20i);
        let r = dot(a, b);                         assert_eq!(r as int, 50);
        let r = dot(vec2!(a.xy()), vec2!(b.xy())); assert_eq!(r as int, 50);
        let r = dot(vec2!(a.xy()), vec2!(b.yy())); assert_eq!(r as int, 60);

        // vec3
        let u = vec3!(1i, 2i, 3i);
        let v = vec3!(10i, 20i, 30i);
        let r = dot(u, v);                       assert_eq!(r as int, 140);
        let r = dot(u.xyz(), v.zyz());           assert_eq!(r as int, 160);
        let r = dot(u, v.zyx());                 assert_eq!(r as int, 100);
        let r = dot(u.xyz(), v);                 assert_eq!(r as int, 140);
        let r = dot(u.xy(), v.xy());             assert_eq!(r as int, 50);

        // vec4
        let s = vec4!(1i, 2i, 3i, 4i);
        let t = vec4!(10i, 20i, 30i, 40i);
        let r = dot(s, t);                       assert_eq!(r as int, 300);
        let r = dot(s.xyzw(), t.xyzw());         assert_eq!(r as int, 300);
        let r = dot(s.xyz(), t.xyz());           assert_eq!(r as int, 140);
    }

    #[test]
    fn test_operator_increment() {
        let v0 = ivec3(1i32);
        let mut v1 = ivec3(v0);
        let mut v2 = ivec3(v0);
        let v3 = *v1.preincrement();
        let v4 = v2.postincrement();
        let v1 = v1;
        let v2 = v2;

        assert_eq!(v0, v4);
        assert_eq!(v1, v2);
        assert_eq!(v1, v3);

        let i0 = 1i32;
        let mut i1 = i0;
        let mut i2 = i0;
        let i3 = *i1.preincrement();
        let i4 = i2.postincrement();
        let i1 = i1;
        let i2 = i2;

        assert_eq!(i0, i4);
        assert_eq!(i1, i2);
        assert_eq!(i1, i3);
    }
}

#[cfg(test)]
mod vec4_tests {
    #![allow(non_snake_case)]

    use super::{vec2,vec3,vec4};
    use super::ivec4;
    use super::{Swizzle2,Swizzle3,Swizzle4};

    use src::operators::{AddAssign,SubAssign,MulAssign,DivAssign};
    use src::operators::{Increment,Decrement};
    use src::scalar::S;

    use test::Bencher;

    #[test]
    fn test_ctor() {
        let A = vec4!(1i);
        let B = vec4!(1i, 1i, 1i, 1i);
        assert_eq!(A, B);

        let mut Tests = vec![];
        Tests.push(vec4!(vec2!(1i, 2i), 3i, 4i));
        Tests.push(vec4!(1i, vec2!(2i, 3i), 4i));
        Tests.push(vec4!(1i, 2i, vec2!(3i, 4i)));
        Tests.push(vec4!(vec3!(1i, 2i, 3i), 4i));
        Tests.push(vec4!(1i, vec3!(2i, 3i, 4i)));
        Tests.push(vec4!(vec2!(1i, 2i), vec2!(3i, 4i)));
        Tests.push(vec4!(1i, 2i, 3i, 4i));
        Tests.push(vec4!(vec4!(1i, 2i, 3i, 4i)));

        for v in Tests.iter() {
            assert_eq!(*v, vec4!(1i, 2i, 3i, 4i));
        }
    }

    #[test]
    fn test_operators() {
        let A = vec4!(1.0f32);
        let B = vec4!(1.0f32);
        assert!(A == B);

        let A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = vec4!(4f32, 5f32, 6f32, 7f32);
        let C = A + B;
        assert_eq!(C, vec4!(5i, 7i, 9i, 11i));
        let D = B - A;
        assert_eq!(D, vec4!(3i, 3i, 3i, 3i));
        let E = A * B;
        assert_eq!(E, vec4!(4i, 10i, 18i, 28i));
        let F = B / A;
        assert_eq!(F, vec4!(4i, 2.5f32, 2i, 7f32 / 4f32));
        let G = A + 1f32;
        assert_eq!(G, vec4!(2i, 3i, 4i, 5i));
        let H = B - 1f32;
        assert_eq!(H, vec4!(3i, 4i, 5i, 6i));
        let I = A * 2f32;
        assert_eq!(I, vec4!(2i, 4i, 6i, 8i));
        let J = B / 2f32;
        assert_eq!(J, vec4!(2i, 2.5f32, 3i, 3.5f32));
        let K = S(1f32) + A;
        assert_eq!(K, vec4!(2i, 3i, 4i, 5i));
        let L = S(1f32) - B;
        assert_eq!(L, vec4!(-3i, -4i, -5i, -6i));
        let M = S(2f32) * A;
        assert_eq!(M, vec4!(2i, 4i, 6i, 8i));
        let N = S(2f32) / B;
        assert_eq!(N, vec4!(0.5f32, 2f32 / 5f32, 2f32 / 6f32, 2f32 / 7f32));

        let mut A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = vec4!(4f32, 5f32, 6f32, 7f32);
        A.add_assign(&B);
        assert_eq!(A, vec4!(5i, 7i, 9i, 11i));
        A.add_assign(&1f32);
        assert_eq!(A, vec4!(6i, 8i, 10i, 12i));
        let A = vec4!(1f32, 2f32, 3f32, 4f32);
        let mut B = vec4!(4f32, 5f32, 6f32, 7f32);
        B.sub_assign(&A);
        assert_eq!(B, vec4!(3i, 3i, 3i, 3i));
        B.sub_assign(&1f32);
        assert_eq!(B, vec4!(2i, 2i, 2i, 2i));
        let mut A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = vec4!(4f32, 5f32, 6f32, 7f32);
        A.mul_assign(&B);
        assert_eq!(A, vec4!(4i, 10i, 18i, 28i));
        A.mul_assign(&2f32);
        assert_eq!(A, vec4!(8i, 20i, 36i, 56i));
        let A = vec4!(1f32, 2f32, 3f32, 4f32);
        let mut B = vec4!(4f32, 5f32, 6f32, 7f32);
        B.div_assign(&A);
        assert_eq!(B, vec4!(4f32, 2.5f32, 2f32, 7f32 / 4f32));
        B.div_assign(&2f32);
        assert_eq!(B, vec4!(2f32, 1.25f32, 1f32, 7f32 / 4f32 / 2f32));

        let mut B = vec4!(2f32);
        let B_y = B.y;
        B.div_assign(&B_y);
        assert_eq!(B, vec4!(1f32));

        let A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = -A;
        assert_eq!(B, vec4!(-1f32, -2f32, -3f32, -4f32));

        let mut A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = A.predecrement();
        assert_eq!(*B, vec4!(0f32, 1f32, 2f32, 3f32));

        let mut A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = A.postdecrement();
        assert_eq!(B, vec4!(1f32, 2f32, 3f32, 4f32));
        assert_eq!(A, vec4!(0f32, 1f32, 2f32, 3f32));

        let mut A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = A.preincrement();
        assert_eq!(*B, vec4!(2f32, 3f32, 4f32, 5f32));

        let mut A = vec4!(1f32, 2f32, 3f32, 4f32);
        let B = A.postincrement();
        assert_eq!(B, vec4!(1f32, 2f32, 3f32, 4f32));
        assert_eq!(A, vec4!(2f32, 3f32, 4f32, 5f32));
    }

    #[test]
    fn test_swizzle_partial() {
        let A = vec4!(1i, 2i, 3i, 4i);
        let B = vec4!(A.xy(), A.zw());
        assert_eq!(A, B);

        let B = vec4!(A.xy(), 3f32, 4f32);
        assert_eq!(A, B);

        let B = vec4!(1f32, A.yz(), 4f32);
        assert_eq!(A, B);

        let B = vec4!(1f32, 2f32, A.zw());
        assert_eq!(A, B);

        let B = vec4!(A.xyz(), 4f32);
        assert_eq!(A, B);

        let B = vec4!(1f32, A.yzw());
        assert_eq!(A, B);
    }

    #[test]
    fn test_operator_increment() {
        let v0 = ivec4(1i32);
        let mut v1 = ivec4(v0);
        let mut v2 = ivec4(v0);
        let v3 = *v1.preincrement();
        let v4 = v2.postincrement();
        let v1 = v1;

        assert_eq!(v0, v4);
        assert_eq!(v1, v2);
        assert_eq!(v1, v3);
    }


    static Size: uint = 1000;

    #[bench]
    fn bench_perf_AoS(b: &mut Bencher) {
        use std::default::Default;
        use src::typedefs::{vec2,vec3,vec4};
        #[deriving(Default)]
        #[allow(dead_code)]
        struct AoS { A: vec4, B: vec3, C: vec3, D: vec2 }
        let mut In = Vec::<AoS>::new();
        let mut Out = Vec::<AoS>::new();
        for _ in range(0, Size) {
            In.push(Default::default());
            Out.push(Default::default());
        }
        b.iter(|| {
            for i in range(0u, Size) {
                *Out.get_mut(i) = In[i];
            }
        })
    }

    #[bench]
    fn bench_perf_SoA(b: &mut Bencher) {
        use std::default::Default;
        use src::typedefs::{vec2,vec3,vec4};

        let mut InA = Vec::<vec4>::new();
        let mut InB = Vec::<vec3>::new();
        let mut InC = Vec::<vec3>::new();
        let mut InD = Vec::<vec2>::new();
        let mut OutA = Vec::<vec4>::new();
        let mut OutB = Vec::<vec3>::new();
        let mut OutC = Vec::<vec3>::new();
        let mut OutD = Vec::<vec2>::new();

        for _ in range(0, Size) {
            InA.push(Default::default());
            InB.push(Default::default());
            InC.push(Default::default());
            InD.push(Default::default());
            OutA.push(Default::default());
            OutB.push(Default::default());
            OutC.push(Default::default());
            OutD.push(Default::default());
        }
        b.iter(|| {
            for i in range(0u, Size) {
                *OutA.get_mut(i) = InA[i];
                *OutB.get_mut(i) = InB[i];
                *OutC.get_mut(i) = InC[i];
                *OutD.get_mut(i) = InD[i];
            }
        })
    }
}

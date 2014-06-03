// Copyright 2014 Felix S. Klock II. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

pub trait EpsilonEq<Epsilons> {
    fn epsilon_eq(&self, rhs: &Self, epsilon: &Epsilons) -> bool;
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

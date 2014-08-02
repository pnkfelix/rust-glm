// Copyright 2014 Felix S. Klock II. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

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

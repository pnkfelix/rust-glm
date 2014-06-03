rust-glm
========

Port of (subset of) GLM to Rust

This is a port of the OpenGL GLM library (http://glm.g-truc.net/) to Rust.

The primary intent is to be as close as possible to the same API that GLM
provides: the same set of functions/methods, with same or similar type
signatures, and the same numeric results from their computations.

A secondary goal is to explore how to encode C++ language features such
as operator overloading in Rust as it stands today.  In that particular
example, right now one *can* statically dispatch on the types of both `A`
and `B` on both sides of an expression `A * B`, but it requires a bit of
finesse.

A third goal is to explore how to use Rust macro's system to ease writing
code to accomplish the above two goals.

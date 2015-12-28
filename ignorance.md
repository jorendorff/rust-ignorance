## License

I'm afraid this one is copyright me and all rights reserved for now.
Not my usual thing. I'm under contract for a book on this stuff.


## Random questions

> Does `assert!(foo)` print a message containing the expression source
> code (`"foo"`) if the assertion fails?

Yes - but the expression may have different whitespace, and comments are
stripped, as though it's reconstructed from the string of tokens.

    assert!(a==b);

    thread '<main>' panicked at 'assertion failed: a == b', src/main.rs:4

> What kind of code does `assert!(x)` expand to?
> How can I tell what a macro expands to, generally?

@@@

> How does the "automatic block return value if the semicolon is
> missing" thing work grammatically?  Does rust have redundant statement
> and expression syntax?
>
> What if you do `let x = if cond { V1 };` with no `else` clause?
> What is the type? What if the type has a Default?

@@@

> Does the Rust standard library contain anything for distributing work
> across multiple threads, e.g. a spmc sort of channel analagous to
> mpsc?

@@@

> Is there a 0MQ client in Rust?

Definitely, but I haven't looked into it.

@@@

> Is there a generic trait implemented by channels that can also be
> implemented by a 0MQ socket?

@@@

> Do you have to `use std;` in order to write fully qualified names like
> `std::env::args()`?

@@@

> What's the prelude like? How does it get included?

@@@

> What do error messages look like when you `result.expect("something went wrong")` and
> something goes wrong?

@@@

> Does `cargo build --release` automatically produce stripped binaries?

@@@

> What's the GDB experience like?

@@@



## Traits

> Can I add a method to all instances of a trait, without defining a new
> trait, by saying something like this?
>
>     impl<It: Iterator> It {
>         fn smooth(self) -> SmoothIterator<It> {
>             SmoothIterator::new(self)
>         }
>     }

No. The error message is:

    error: no base type found for inherent implementation; implement a trait or new type instead



## Terminology

> Is there a better term than "object" for these things that have lifetimes?

@@@

> What are the branches of an `enum` called? Variants?

@@@

> What are the member fields of enums called? Fields?

@@@



## Lifetimes

> What invariants exactly does the borrow checker enforce?

I'm not sure, but here are things I know it does enforce:

*   An object can't be moved or dropped while any reference to it, or to
    any enclosing object or subobject of it, exists.

*   As long as any non-`mut` reference to an object exists,
    the object can't be assigned to (with an exception for cells?).

*   As long as a reference points to data in an object of `enum` type,
    the object can't change to another variant of that `enum`.

*   As long as a `mut` reference to an object exists,
    no other reference to it, or to any enclosing object or subobject of it, exists.

*   There's some set of things you can't do to an object that's been moved
    or dropped.

@@@

> Do we always know statically whether or not a binding has been dropped? What about code like:
>
>     {
>         let x = Thing::new();
>         if condition { use(&x); drop(x); }
>         ...
>     }
>
> Is it allowed? Is it allowed *only* if `x` is not mentioned thereafter?
>
> What if `condition` is false? `x` gets dropped implicitly at the end of the block.
> How does the compiled code know whether or not it has already been dropped?

@@@

> When you declare `let (a, b) = ...`, do the two bindings have the same lifetime?

If neither `a` nor `b` implements `Drop`, then yes!

    let (mut a, mut b) = (Thing::new(0), Thing::new(1));
    b.p = Some(&a.v);   // ok
    a.p = Some(&b.v);   // also ok

Here `Thing` is a struct with a lifetime parameter:

    struct Thing<'a> {
        v: usize,
        p: Option<&'a usize>
    }

Since this code compiles fine, it must be that `a` and `b` are given the
same lifetime (call it `'t`) and the same type, `Thing<'t>`.
Furthermore, the fields `a.v` and `b.v` must have lifetime `'t` as well.

However, if you add `impl<'a> Drop for Thing<'a>`,
then it breaks.

(Proof sketch: Doing `b.p = Some(&a.v)` establishes a lifetime constraint:
the lifetime of `a.v` must be >= the referent lifetime
specified in the declaration of `b.p`,
which must be >= the lifetime of `b.p` itself.
And the fields of a struct must live as long as the struct.
So we have:

    lifetime_of a.v
    >= referent_lifetime_of b.p  // === lifetime_parameter_of type_of b
    >= lifetime_of b.p
    >= lifetime_of b

    lifetime_of b.v
    >= referent_lifetime_of a.p  // === lifetime_parameter_of type_of a
    >= lifetime_of a.p
    >= lifetime_of a

If we add a few more sensible constraints about Rust not being allowed
to create `a.v` and `b.v` before creating either of `a` or `b`, then
the only solution would be that all these lifetimes are equal.)

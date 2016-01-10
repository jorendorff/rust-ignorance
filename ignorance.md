## License

I'm afraid this one is copyright me and all rights reserved for now.
Not my usual thing.


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

> Why is "hello world" 500KB?

@@@

> What's the mechanism behind thread panic cleanup? The same as LLVM's
> support for C++ exceptions?

@@@


## Grammar

> In a path, can a module name be separated from the following `::` by whitespace?

Yes.

    println!("{}", abc ::def());

I don't know why the parser appears to have a special case for
identifiers followed immediately by `::`.

> Can a macro name be separated from its `!` by whitespace?

Yes. This works:

    println /* comment */
        !("hello world");

> Can a macro name be a path?

No. They're not scoped quite like other items, I guess because macros
have to be expanded at a point during compilation when scopes don't
exist yet.

http://doc.rust-lang.org/book/macros.html#scoping-and-macro-importexport

Examples of things not working:

    mod foo {
        macro_rules! bar {
            ($e:expr) => (println!("bar: {}", $e));
        }
    }

    use foo::bar;  // error: unresolved import `foo::bar`. There is no `bar` in `foo`

    fn main() {
        foo::bar!(3);  // error: expected macro name without module separators
    }

You also can't declare a macro to be `pub`. Instead, you have to declare
`mod foo` to have the attribute `#[macro_use]` or `#[macro_use(bar)]`.

To export a macro to other crates, you have to give the macro the
`#[macro_export]` attribute. This works even if the module in which
the macro is declared doesn't have `#[macro_use]`.


## Statements and expressions

> How does the "automatic block return value if the semicolon is
> missing" thing work grammatically?  Does rust have redundant statement
> and expression syntax?

Rust does *not* have redundant statement and expression syntax.

I've sort of reverse-engineered how this works from the parser, but I
don't know how to express it yet. It may be horrible.

@@@

> What if you do `let x = if cond { V1 };` with no `else` clause?
> What is the type? What if the type has a Default?

@@@

> How is overflow handled with unsigned integer types?

The same as for signed integer types.

> Is there subslice syntax `&x[y..z]`? If so, how is it handled in the grammar?

@@@

> How does this semicolon thing work anyway? Is `if x { f(); }` an expression?

@@@

> Is there a nice error message if you type `genfn<u32>()` instead of
> `genfn::<u32>()`?

@@@

> Is `b'\xff'` a legal byte literal?

Yes.

Weirdly, `'\xff'` is not a legal `char` literal, I guess to avoid giving
the impression that the character U+00FF is represented as the byte FF
in strings.

> Does Rust have a `sizeof` operator or equivalent?

@@@

> Can both sides of `@` be patterns, or is the lhs required to be a single identifier?

@@@

> Does Rust have labeled `break` and `continue`?

Yes.

    'foo: loop {
        ...
        break 'foo;
    }

> Interesting. Is that `'foo` also a lifetime? Can it be used as a
> lifetime parameter? (If so, is that useful?)

@@@

> In the parser, in the `ast` module, we have:
>
>     /// An `if` block, with an optional else block
>     ///
>     /// `if expr { block } else { expr }`
>     ExprIf(P<Expr>, P<Block>, Option<P<Expr>>),

Why is the "then" part a block, when the "else" part is an expr? What's the difference?


## Types and type inference

### Weird types

> How do you create a value of `Box<T>` where `T` is an unsized type?
> In particular, it seems useful for trait objects.

The reference gives an example for trait objects:
you can just `Box::new(10) as Box<MyTrait>` if the right `impl` exists.

@@@

> Possibly the same question: Why doesn't this program work?
>
>     use std::ops::*;
>     static arr: [i32; 4] = [1, 2, 3, 4];
>     fn main() {
>         let slice: &'static [i32] = &arr;
>         let a = slice as &Index<Range<usize>, Output=&'static [i32]>;
>         println!("{:?}", a[2..4]);
>     }

@@@


### Diverging expressions (`!`)

> Can a diverging expression be used as a function parameter? What is its type?

Yes, it can, and I suppose its type is inferred.

This program:

    fn g(a: String) -> usize { a.len() }
    fn f() -> usize { g(return 13) }
    fn main() { println!("f(): {}", f()); }

prints `f(): 13`.

> Can a diverging expression be used as one side of an if-expression?
> Can I write: `g(if x { return 13; } else { 117 })`?

Yes. The type of the if-expression is, I suppose, the type of
the other branch.  This program:

    fn g(a: usize) -> usize { a & !3 }

    fn f(a: bool) -> usize {
        g(if a { return 13; } else { 117 })
    }

    fn main() {
        println!("f(true): {}", f(true));
        println!("f(false): {}", f(false));
    }

prints:

    f(true): 13
    f(false): 116

> OK. If the type of a diverging expression is "inferred", I guess that
> would mean that an expression like `return 13` generates no "outward"
> type equations for the purpose of type inference. It would seem to
> follow that if there are no contextual ("inward") constraints on its
> type either, then there should be a type error. Inference can't figure
> out the type. Right?

@@@

> Can you do `panic!() + 2`?

@@@


## User-defined types (structs and enums)

> Tuples automatically derive `Clone` and `Copy` if appropriate, right?

Yes.

> OK. What about tuple-style structs?

They don't derive `Clone` or `Copy` unless you ask for them.

> Do patterns work in struct declarations?
>
>     struct WrappingRange {
>         (start, span): (i32, u32)
>     }

No. `error: expected ident`.


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

> What is the term for these things that have lifetimes?

They are called values.

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

If neither `a` nor `b` implements `Drop`, then yes. (??!?)

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

> The reference says that in `let x = foo(&temp())`, the temporary that
> stores the result of `temp()` will be freed after the `let` declaration.
> Is that true even if the signature of `foo` is `fn foo(&T) -> &T`?
> In that case, it seems like the temporary should live as long as `x`.

The temporary really only lives as long as the declaration. Lifetime
inference does not come into play; the lifetime of a temporary is
determined syntactically.

This program:

    fn seven() -> u32 { 7 }
    fn f(x: &u32) -> &u32 { x }

    fn main() {
        let x = f(&seven());
        println!("{}", *x);
    }

produces these errors:

    error: borrowed value does not live long enough
    :5     let x = f(&seven());
                      ^~~~~~~
    note: reference must be valid for the block suffix following statement 0 at 5:24...
    :5     let x = f(&seven());
    :6     println!("{}", *x);
    :7 }
    note: ...but borrowed value is only valid for the statement at 5:4
    help: consider using a `let` binding to increase its lifetime

> If lifetimes can be different depending on whether or not a type
> implements `Drop`, then what happens with type parameters? Is this
> something weird about `Drop`?

This works OK whether you call it with a type that implements `Drop` or
not:

    struct GenThing<'a, T: 'a> {
        v: T,
        p: Option<&'a T>
    }

    fn ok_generic_downref<T>(av: T, bv: T) {
        let (mut a, mut b) = (GenThing::new(av), GenThing::new(bv));
        b.p = Some(&a.v);
        a.p = Some(&b.v);
    }

(Given the obvious implementation of `GenThing::new()`.)

This doesn't cause any problems, because the destructor in question
(`T::drop(&mut self)`) does not have access to the reference field
`GenThing::p`.

@@@

> I want to write a struct for parsing some text.
> Can I do it without a lifetime parameter?
>
>     struct Parser {
>         chars: Chars<???>
>         remaining_input: &'??? str
>     }

It's probably best for the parser to own the string
for the duration of parsing.


## Statics

> The reference says that "Statics may contain interior mutability
> through the `UnsafeCell` language item." What is a language item?
> Why won't the normal things work?

No idea what a "language item" is.

I think now that it's some pluggable but super fundamental part of Rust,
like the default allocator (`malloc`). But I don't remember where I read
that, and there was a warning not to actually use this feature.

Possibly because `Mutex` and `RwLock` have destructors, while `Cell` and
`RefCell` are not `Sync`. Statics must be `Sync` and must not have
destructors.

@@@


## Modules, paths, names, namespaces

> A path can start with `<` *type* `>::`. What does that do?

@@@

> Can you have both a type and a `fn` with the same name in the same module?

Yes.

    struct X { a: u32 }

    #[allow(non_snake_case)]
    fn X(v: u32) -> X {
        X { a: v + 1 }
    }

It seems there are two namespaces: one for types and one for values.

Note that a declaration `struct X;` or `struct X(u32);` declares both a
type and a value.

> Suppose I do `pub use other_mod::MyFancyEnum;` where
> `other_mod::MyFancyEnum` is an enum with some public constructors. Are
> the constructors automatically exposed by my module too? Is there a
> way to make them not be?

@@@


## Crates

> What exactly is a crate?

@@@

> Can an arbitrary crate (in source form or in binary form) be turned
> into a .so/.dylib/.dll shared library?

@@@

> Can crates be linked together into bigger crates?

@@@

> Can two crates depend on each other (a cyclic dependency)?

@@@

> How does syntax extension sharing across crates work, exactly? (Not
> smart enough to have a precise question yet.)

@@@

> But in particular, how is the `#[derive(Serialize)]` thing in
> `serde_json` implemented, within `serde_json`, and how is it handled
> by the compiler when `serde_json` is being consumed as an `extern
> crate`?

@@@

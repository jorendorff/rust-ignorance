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

The `assert!` macro uses the `stringify!` macro to construct this string.

> What kind of code does `assert!(x)` expand to?
> How can I tell what a macro expands to, generally?

Macros are documented in the most straightforward way possible:
the documentation literally includes complete source code for the macro,
albeit with the whitespace scrambled.

The definition of `assert!` is:

    macro_rules! assert {
        ($cond: expr) => (
            if !$cond {
                panic!(concat!("assertion failed: ", stringify!($cond)));
            }
        );
        ($cond: expr, $($arg: tt)+) => (
            if !$cond {
                panic!($($arg)+);
            }
        );
    }

Or something to that effect.

> Macros are hygienic, right? If a macro takes an `expr` argument,
> and the actual argument contains a `break`, but the macro also
> inserts a loop, then which loop does the `break` break out of?
> (I guess hygiene would suggest it breaks out of the innermost loop
> lexically evident in the context in which the macro is invoked?
> Not actually totally sure of myself there.)

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

> What causes Rust to emit and not emit this message?
>
>     error: not all control paths return a value
>
> I think there are cases where this message *should* be emitted, but it
> isn't.

I think it is definitely emitted when you write something like:

    fn foo() -> Object {
        f();
    }

but in this case, it is not:

    fn cat<F: BufRead>(infile: F) -> std::io::Result<()> {
        for line in infile.lines() {
            let line = try!(line);
            println!("    {:?}", line);
        }
    }

In the latter case, due to an accident of grammar,
the `for`-expression is parsed as a final expression
rather than as a statement. I bet that's it.

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

The syntax of a block is `{ stmt* expr? }`.

(In addition, inner attributes are permitted if this is the main block of a `fn`.)

A statement is either a `let` declaration,
an item declaration (a nested `fn`, `struct`, or `enum`),
or an *expression statement* (just an expression followed by a semicolon).

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

> Does a `for` loop consume the collection even if it's a collection of a type that's `Copy`?

Sure does.

    let v: Vec<i32> = vec![0, 1, 2, 3, 4];
    for _ in v {}
    println!("after: {:?}", v);  // error: use of moved value: `v`

If the collection itself is `Copy`, then we're ok:

    let v: Vec<i32> = vec![0, 1, 2, 3, 4];
    let slice: &[i32] = &v;
    for _ in slice {}
    for _ in slice {}
    println!("after: {:?}", slice);

> OK, can you prevent a `for` loop from consuming its collection by using a `ref` pattern?

Nope:

    let v: Vec<i32> = vec![0, 1, 2, 3, 4];
    for ref p in v {}
    println!("after: {:?}", v);  // error: use of moved value: `v`

> Is it generally true that you can make a `for` loop not consume the collection
> by adding a `&` to the right of `in`? If so, how does that work?

It's not guaranteed by the language, but the builtin collections all seem to support this pattern.

> If you add `&mut` to the right of `for x in`, does that cause `x` to be bound
> to a `mut` reference?

Again, not guaranteed by the language, and while this works for most of
the standard collection types, it does not work for others.

Consider `BTreeSet`. Modifying a value that's stored in an ordered collection
would be trouble! So `IntoIterator` is not implemented for `&'a mut BTreeSet<V>`.

Similarly, `IntoIterator` is implemented for `&'a mut BTreeMap<K, V>`, but
the item type for that implementation is `(&'a K, &'a mut V)`;
the keys are not mutable. Same for `HashMap`.

> OK, so much for standard collection types. What about arrays of fixed size? Slices?

Arrays of fixed size are not directly iterable.

Slices are iterable and `Copy`. If you iterate over a slice of type `&[T]`,
the type of your binding is `&T`.

`mut` slices are also iterable, but not `Copy`.

    let slice: &mut [i32] = &mut [0, 1, 2];
    for p in slice { println!("{:?}", *p); *p += 1; }
    println!("after: {:?}", slice);  // error: use of moved value

> How can an integer be converted to a C-like `enum` type?
>
>     #![derive(Debug, Clone, Copy)]
>     enum Color { Red = 1, Green, Blue, Purple }
>     let i = 1;
>     let c = ???;  // convert i to type Color
>     println!("{:?}", c);  // should say "Red"
>
> (The reverse is possible using a cast: `Red as i32` is `1`.)

I don't think this is possible short of `std::mem::transmute()`.

@@@


## Declarations

> What are the scoping rules for `let` vs. `fn` (and other item declarations) in a block?

The scope of a `let` binding begins *after* the `let` declaration and continues to the end of the block.

The scope of an item is the whole containing block.

This means that two `fn`s declared in a block can call each other:

    #[test]
    fn fns() {
        fn odd(x: usize) -> bool { x != 0 && even(x - 1) }
        fn even(x: usize) -> bool { x == 0 || odd(x - 1) }
        assert_eq!(even(33), false);  // passes
    }

but two closures can't:

    let odd = |x| x != 0 && even(x - 1);  // error: unresolved name `even`
    let even = |x| x == 0 || odd(x - 1);

Constructing the closures more carefully can get rid of the name error,
but there is still a type error:

    let (odd, even);
    odd = |x| x != 0 && even(x - 1);  // type error: `even`
    even = |x| x == 0 || odd(x - 1);

> What happens if the type of a block isn't in scope outside the block?
>
>     println!("{:?}", {
>         #[derive(Debug)] struct Cow;
>         Cow
>     });

This works, printing `Cow`!

The type doesn't have to be in scope in order for generics to operate on it.


## Types and type inference

> Why can't I write `(3.14).floor()`?

I think this is kind of unfortunate.
The type of a constant like `3.14` is either `f32` or `f64`;
the exact type is inferred from context.
But type inference does not suss out the type equation

    typeof 3.14 = typeof (3.14).floor()

because Rust does not consider that `f32` and `f64` have a bunch of methods in common.

If you tell Rust the type, instead of relying on type inference, it works:

    println!("{}", (3.14f64).floor());  // 3
    println!("{}", f64::floor(3.14));   // 3

The former works because, knowing the type of `3.14f64`, Rust can then look up the `floor` method.
The latter works because we tell Rust to look up the `floor` method of the `f64` type.

When the method involved is an operator:

    println!("{}", 1.0 / 3.0);  // ok!
    println!("{}", std::ops::Div::div(1.0, 3.0)); // ok
    println!("{}", (1.0).div(3.0)); // error: no method named `div` found for type `_`

The `/` operator (the first example)
must be syntactic sugar for the second example here,
not the third.

I think `(3.14).floor()` could be made to work:
give `f32` and `f64` a common trait and put all their common methods on it.
Then tell the type checker that a float literal's type has that trait (as a bound).

> What coercions are permitted without a cast?

`&mut T` to `&T` is allowed.

`&T` to `&U` is allowed if `<T as Deref>::Target` is `U`.

`&mut T` to `&mut U` is allowed if `T: DerefMut` and `<T as Deref>::Target` is `U`.

@@@


### `if` and `match`

> What are the types involved in an `if` expression?

For a while I had a theory that the two blocks of an `if` expression
only had to agree in type if the `if` expression's own type was actually
used. But that predicts this program is OK, and it's not:

    fn main() {
        if true { 1 } else { "ok" };  // error: if and else have incompatible types
        println!("ok");
    }

So I now believe that the two blocks of an `if` expression must always agree.
Furthermore, if an `if` expression appears as a statement, I suppose its type must be `()`.

    fn f() -> bool { true }

    fn main() {
        if true { f() }  // error: mismatched types: expected (), found bool
        else { }
        println!("ok");
    }

I believe that `if EXPR BLOCK` without an `else` block is always
equivalent to `if EXPR BLOCK else {}` with an empty `else` block.

> If a type has no values, is it legal to use it in a `match` expression with no arms?

Yes:

    enum Nonesuch {}

    fn what(ns: Nonesuch) -> ! {
        match ns {}   // ok
    }

Of course such an expression can never be evaluated,
because you would have to have already produced a value of the empty `Nonesuch` type,
a logical impossibility.


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

> Do you have to `use std;` in order to write fully qualified names like
> `std::env::args()`?

Yes, except in the toplevel of a crate. Weird. :(

The reason is that every crate behaves as though it has an implicit `extern crate std;`
and that form only adds the crate to the toplevel. It must be explicitly imported
into each submodule.

The names exported by the prelude are implicitly imported into *every* module,
but `std` is not among them.

You can write `::std` anywhere.

> What's the prelude like? How does it get included?

This is documented under [std::prelude](http://doc.rust-lang.org/std/prelude/index.html).

> Can a block contain a module?

Yes!

    println!("{}", {
        mod x { pub const Q: i32 = 3; }
        x::Q
    });

Why not, right? Macros will find a use for every scrap of orthogonality
the language gives us.

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

> When is a non-`pub` member of a module visible from another module?

I don't know, but certainly:

    mod a {
        pub struct X { f: i32 }
        mod b { /* X::f is accessible here */ }
    }
    mod c { /* X::f is not accessible here */

@@@

> Can you make the fields of a tuple-like struct public?
> What about individual arms of an `enum`?

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

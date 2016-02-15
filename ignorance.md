## License

Hi!

Feel free to use these questions for whatever you like!
That's the important part, right?

I'm afraid the answers are copyright me and all rights reserved for now.
I know, not my usual thing.


## Random questions

> Is it just my system that can't build 32-bit executables? Or is it a thing?

I was ultimately able to build 32-bit executables by downloading the whole
32-bit version of Rust. Not by cross-compiling. It's possible that a
cross-compiler for i686 is built into x86_64 rust, but it seems no 32-bit
libstd is included.

@@@

> How can I set a breakpoint in rust-gdb and have it hit? In particular,
> the program I want to debug is a debug build of rustc.

After building `--enable-debug`, I was able to `break main` and do a few more
simple things. I don't remember if I ever got a breakpoint to hit.

@@@

> How can I turn on logging for rustc?

@@@

> Is it possible to dump a value using the `Debug` trait from rust-gdb?

@@@

> In general, what is rust-gdb? Is there any documentation for it?

rust-gdb is a script that force-loads some Python pretty-printers.

@@@

> Why isn't stock GDB good enough for what Rust is trying to do?

GDB doesn't load arbitrary Python extensions unless the user opts in,
for security reasons.

@@@

> Does Rust have static assertions?

@@@

> Can you use `impl` to add methods to a type defined in another module?
> Another crate?

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

> Are there separate type and value namespaces in Rust?

Sure seems like it:

    type X = i64;
    const X: bool = true;   // totally OK
    fn main() {
        println!("{}", X);  // true
    }

Also, the error messages when you use a type where a value was expected, or
vice versa, suggest that when Rust expects one, it forgets the other ever
existed.

    println!("{}", i64);  // error: unresolved name `i64`

    let x = 13; x::new();  // error: use of undeclared type or module `x`

> Which type names also name values?

Structs. (Type aliases for structs don't count!)

> Are modules in the value namespace, the type namespace, or both?

Only the type namespace.

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

Two rules explain all the weirdness:

1.  **The semicolon after an expression statement is optional
    if the expression is "complete"**—that is,
    it's an unparenthesized `if`, `match`, `loop`, `while`, `for`,
    `unsafe`, or block expression, or a macro use with curly braces.
    ("Complete" expressions always end with `}`,
    but `match x {_=>1} + match y {_=>2}` is not complete,
    even though it starts with a keyword and ends with `}`.
    *Wait, that's nonsensical because the `+` isn't allowed
    after the first match-expression. Need a better example here.*)

    **If the semicolon is omitted,
    the type of the expression is required to be `()`.**
    (This is a nice rule because it means you can drop the semicolon after
    an `if` expression as long as you put semicolons at the end of each
    of the `if`-expression's blocks.)

    If you actually write a semicolon, the type of the expression
    can be whatever you want; it'll be ignored.

2.  Because of rule 1, there is an ambiguity in block syntax
    whenever a "complete" expression is immdiately followed by an operator
    that's both a unary operator and a binary operator.

        fn f() -> i32 {
            if a {
                b();
            } else {
                c();
            }
            -1
        }

    We don't want this to parse as `(if ...) - 1` which is nonsense
    (and wouldn't type-check). It has to parse as an `if` *statement*
    followed by the expression `-1`.

    (Other confusable operators:
    address-of `&` vs. bitwise `&`,
    dereferencing `*` vs. multiplying `*`,
    grouping parentheses vs. function-call parentheses.)

    Therefore we have rule 2: **In a block, an expression
    or expression-statement that begins with a "complete" expression
    consists only of that expression.** Any subsequent tokens
    in the block must be parsed as part of the next expression or statement.

    (But the way this is implemented in the parser seems to be weirder
    and more work than I can justify in my head.)

    The consequences of this rule can be weird.

        { 1 + if a { 1 } else { 0 } + 1 }  // ok
        {     if a { 1 } else { 0 } + 1 }  // error: unexpected token: `+`
        {    (if a { 1 } else { 0 })+ 1 }  // ok

> OK. Is a macro invocation with curly braces a "complete" expression?

Yes. Or, if the macro happens to be `macro_rules!`, it's not an expression at all.

> How does this semicolon thing work anyway? Is `if x { f(); }` an expression?

It is; see above.

> What if you do `let x = if cond { V1 };` with no `else` clause?
> What is the type? What if the type of V1 has a `Default`?

An omitted `else` clause is like `else {}`.
The type of the `else` branch is `()`.
It doesn't matter what type V1 is.

> How is overflow handled with unsigned integer types?

The same as for signed integer types.

> Is there subslice syntax `&x[y..z]`? If so, how is it handled in the grammar?

Yes, slicing works.

As far as the grammar is concerned, three separate productions here:
`& _` composed with `_ [ _ ]` composed with `_ .. _`.

But I'm pretty sure the compiler desugars all this to *two* method calls, not three?
I'll have to experiment to figure out exactly what happens here.

@@@

> Is there a nice error message if you type `genfn<u32>()` instead of
> `genfn::<u32>()`?

It's not bad.

    rusteval.rs:2:39: 2:45 error: chained comparison operators require parentheses
    rusteval.rs:2 fn main() { let v = {std::mem::size_of<i32>()}; println!("{:?}", v); }
                                                        ^~~~~~
    rusteval.rs:2:39: 2:45 help: use `::<...>` instead of `<...>` if you meant to specify type arguments
    rusteval.rs:2:40: 2:43 error: unresolved name `i32` [E0425]
    rusteval.rs:2 fn main() { let v = {std::mem::size_of<i32>()}; println!("{:?}", v); }
                                                         ^~~
    rusteval.rs:2:40: 2:43 help: run `rustc --explain E0425` to see a detailed explanation
    rusteval.rs:2:22: 2:39 error: binary operation `<` cannot be applied to type `fn() -> usize {core::mem::size_of}` [E0369]
    rusteval.rs:2 fn main() { let v = {std::mem::size_of<i32>()}; println!("{:?}", v); }
                                       ^~~~~~~~~~~~~~~~~
    rusteval.rs:2:22: 2:39 help: run `rustc --explain E0369` to see a detailed explanation
    rusteval.rs:2:22: 2:39 note: an implementation of `std::cmp::PartialOrd` might be missing for `fn() -> usize {core::mem::size_of}`
    rusteval.rs:2 fn main() { let v = {std::mem::size_of<i32>()}; println!("{:?}", v); }
                                       ^~~~~~~~~~~~~~~~~
    error: aborting due to previous error

The first "help" message is on point. It can get buried by the surrounding
evidence of rustc's utter confusion.

> Is `b'\xff'` a legal byte literal?

Yes.

Weirdly, `'\xff'` is not a legal `char` literal, I guess to avoid giving
the impression that the character U+00FF is represented as the byte FF
in strings.

> Does Rust have a `sizeof` operator or equivalent?

There's a builtin function: `std::mem::size_of::<T>()`.
Weirdly, it is not a `const fn` currently.

Also `align_of`.

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

## Patterns, `match`, `if let`, pattern matching

> Is an `@` pattern refutable?

Well, hang on.
I used to think there was a hard syntactic distinction
between refutable and irrefutable patterns.
I now think Rust is smarter than that; it can statically compute
a conservative approximation `(<=)` of the relation

    cover :: [Pattern] -> Pattern -> Bool

such that ``priorPatterns `cover` p`` iff for all v matched by p,
some q in priorPatterns also matches v.

This is the relation Rust uses to flag two opposing errors:

*   A match arm is covered by previous unguarded match arms
    ("error: unreachable pattern")

*   The unguarded arms of a `match` fail to cover all possible values.
    That is, they do not cover the wildcard pattern `_`.
    ("error: non-exhaustive patterns")

For the purpose of computing this relation, I'm sure Rust just ignores the `@`
and uses the pattern on the rhs.

> Can `@` be used with `ref`, like:  `ref point @ (ref x, ref y)`?

Even with `ref`, `@` can't be used with a right-hand pattern
that has any pattern bindings in it at all.

But `@` can be used with `ref`:

    match (2, 3) {
        ref point @ (2, _) => format!("ok! {:?}", point),
        _ => panic!("FAIL")
    }

> Can `@` be used on non-copyable types at all?

Sure. This is presumably why the right-hand pattern can't have bindings:
the matched value is moved into the new variable named on the lhs of `@`.

> `|` cannot normally be used in patterns outside of `match`.  But what
> if the last alternative is irrefutable, as in `let (3 | _) = 4;`?
> (That is: is the restriction syntactic or semantic?)

It's syntactic. `let (3 | _) = 4;` is a syntax error.

> Is `&(0...100)` a valid pattern?

No. Extra parentheses are not allowed in patterns, and here `...` binds tighter than `&`.

There's no operator precedence in patterns.
The grammar has one big `pat` nonterminal with a ton of productions, including:

    pat: '&' pat
    pat: pat-range-end '...' pat-range-end

> `ref` can't be used in function parameters, right?

Well, it can:

    fn g(ref mut x: i32) { *x += 1; }  // compiles fine

But parameters are still always passed by move/copy.

    let k = 1;   // note: not mut
    g(k);        // works fine (copied in)
    assert_eq(k, 1);  // the original is unchanged


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


### Method calls

> Can a method that takes its `self` argument by value be called via `Deref`?
> For example, `Box::new(Pickle).chomp()`?

Yes, if you own the `Box`, you can consume it in this way.

    struct Pickle;
    impl Pickle {
        fn chomp(self) { println!("chomp!"); }
    }

    fn main() {
        let b = Box::new(Pickle);
        b.chomp();  // chomp!
    }

Chomping `b` consumes the `Box` as well as the `Pickle`.

But it only works for `Box`!

    struct Carton<T>(T);
    impl<T> std::ops::Deref for Carton<T> {
        type Target = T;
        fn deref(&self) -> &T { &self.0 }
    }

    fn main() {
        let b = Carton(Pickle);
        b.chomp();  // error: cannot move out of borrowed content
    }

What's needed to make this work would be a third `Deref` trait, `DerefInto` or `DerefOnce`.

> Suppose I define a new struct `Vec3`. Is it possible to overload
> multiplication by a primitive type, such that `x * v` works when `x: f64`,
> `v: Vec3`?

Yep. `impl Mul<Vec3> for f32` just works!


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

> Given `struct X { a: i64, b: bool}`, can you call `X` like a function,
> `X(32, true)`?

No.



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

> Can the syntax `type X = Y;` appear in an `impl` that isn't a trait `impl`?

No:

    struct K;
    impl K {
        type J = u32;  // error: associated types are not allowed in inherent impls
    }

According to `rustc --explain E0202`, inherent associated types were
part of [RFC 195](https://github.com/rust-lang/rfcs/pull/195) but have
never been implemented.

> In this issue <https://github.com/rust-lang/rust/issues/31299>,
> what is going on?

@@@


## Terminology

> What is the term for these things that have lifetimes?

They are called values.

> What are the branches of an `enum` called? Variants?

Yes.

> What are the member fields of enums called? Fields?

@@@



## Moves, borrowing, ownership, lifetimes

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

> Do we always know statically whether or not a value has been dropped? What about code like:
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

> Can values be moved out of structs, tuples, arrays, and enums?

A value can definitely be moved out of an aggregate via pattern matching.
It consumes the aggregate:

    // tuples
    let v = (a, z)
    let (aa, zz) = v;  // moves v.0 -> aa, v.1 -> zz, uses up v

    // structs
    let v = a .. z;
    let Range { start: aa, stop: zz } = v;  // moves v.start -> aa, v.stop -> zz, uses up v

    // enums - a little different because matching an enum is refutable.
    // We show `if let`, but `match` would also work.
    let v = Some(a);
    if let Some(aa) = v { ... }   // moves a -> aa, uses up v

(Arrays would work too, except that slice patterns are not stable yet.)

In any of these cases, using `ref` in the pattern
will cause the original value *not* to be consumed;
nothing will be moved out of it after all.
Instead all the bindings are references to parts of the original.

Using `ref` in a pattern is called "binding by-ref".
Not using it is "binding by-move".
It is an error to bind by-move and by-ref in the same pattern.
(But oddly enough, it's OK to bind by-ref in some arms of a `match`
and by-value in others.
Guessing:
in by-value arms, the value is used up *before* the arm-consequent is evaluated;
in by-ref arms, it is used up *after*, when the references expire.

In addition, for structs and tuples only, you can move individual fields
out of a value you own, like an argument or local variable (or
a value that has been moved into a `move` closure, I think).

    let v = (a, z);
    let aa = v.0;   // moves v.0 -> aa, uses up v.0 only
    let bb = v.1;   // moves v.1 -> bb, now v is completely moved

> If a variable is declared without an initializer, can it be initialized twice?
>
>     {
>         struct Cake;
>         let x;
>         x = Cake;
>         let y = x;  // move
>         x = Cake;   // re-initialize
>     }

Nope.  The last line gets ``error: re-assignment of immutable variable `x`.``

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
>     struct Parser<'x> {   // <-- eliminate this somehow?
>         chars: Chars<'x>
>         remaining_input: &'x str
>     }

It's probably best for the parser to own the string
for the duration of parsing.

> Does (non-initializing) assignment cause the previous value to be dropped?
>
>     let mut a = vec![1];
>     a = vec![2];

@@@

> Can lifetimes be used to enforce a rule like "both arguments to this method
> must be owned by the same parent object"?
>
>     fn are_friends(thing1: ThingRef<'a>, thing2: ThingRef<'a>) -> bool { ... }
>
> (We want to ensure that every `ThingRef<'a>` is created from a
> `ThingZone` with lifetime `'a`; that no two `ThingZone` objects have
> the same lifetime; and that no `ThingRef<'a>` can pass as a
> `ThingRef<'b>` if `'b` is a different lifetime.)

Yes: https://gist.github.com/jorendorff/45116c5aa132d5fe58e9

Discussion: https://users.rust-lang.org/t/can-rust-lifetimes-be-used-to-enforce-this-api-rule/4287

> OK, how exactly does the scheme above work?

Good question.

@@@

> Do lifetimes really have "no influence on codegen", as Mutabah claims
> in this fascinating conversation?
>
>     <Raticide> I'm not sure I get lifetimes. Is it basically: anything that uses lifetime 'a will stay in memory until all things using that lifetime descope?
>     <Mutabah> Raticide: No, lifetimes are a compile-time annotation used to check that things aren't dropped while still in use
>     <Raticide> oh, so they don't really change any behaviour?
>     <Mutabah> Nope.
>     <Mutabah> No influence on codegen
>     <Raticide> ah cool. thanks
>     <kmc> Raticide: what you said is basically true though, just the causality is backwards from what you implied
>     <kmc> a value is freed simply when it reaches the end of a function without being moved somewhere else
>     <Raticide> yeah, so it ensures the human keeps them in memory
>     <kmc> right
>     <Havvy> Raticide:  Yep. Just like shape types ensure the human passes values with the right size and shape.
>     <Raticide> that's pretty cool
>     <Raticide> better than a segfault
>     <kmc> yep
>     <kmc> better than a silent, exploitable use after free :)
>
> I think it's basically right, but type systems are weird, so I would not
> be surprised to hear of exceptions to the rule.
>
> It would be interesting to know the erasure algorithm that turns a
> Rust program into a Rust-minus-lifetimes program. Remove all lifetime
> parameters; remove all lifetime annotations from reference types. Is
> anything left?

@@@


## Closures

> Can you move a closure into a `Box<Fn()>` and then call it?

Sure can.

    let b: Box<Fn()> = Box::new(|| println!("woof"));
    b();  // woof!

More elaborately:

    fn make_adder(n: i32) -> Box<Fn(i32) -> i32> {
        Box::new(move |i| i + n)
    }

    let adder = make_adder(3);
    assert_eq!(adder(100), 103);
    assert_eq!(adder(adder(100)), 106);

> Hmm. Integers are copyable... so can you drop the `move` keyword from that
> closure?

No, the `move` is necessary. If you drop it, `n` is borrowed by default (even
though it's small and copyable) so the closure would not be allowed to outlive
`n` the way it does here.


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

> Is it a big deal that Rust doesn't have mutable global variables?

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

> Are names from enclosing modules automatically visible in nested modules?

No.

    mod a {
        pub fn g() -> i32 { 123 }
        pub mod b {
            pub fn f() -> i32 { a::g() }  // error: Use of undeclared type or module `a`
        }
    }

    fn main() {
        println!("{}", a::b::f());
    }

You have to import them.

> In a `use` declaration, what's in scope?

They aren't like other paths.

Setting aside the relative import forms `super::foo` and `self::bar`,
the paths that appear in `use` declarations are "absolute":
the first step in the path must be `std`
or a crate explicitly opened with `extern crate foo;` at toplevel.

> Can you `let`-declare a name that, *later* in the same block,
> is declared as an item? Does the `let` declaration shadow the item?

Yes and yes.

    fn main() {
        x();  // fn
        let x = || println!("let");
        x();  // let
        fn x() { println!("fn"); }
        x();  // let
    }

> Is it allowed to use a leading `::` in a `use` declaration?

Yes:

    use ::std::mem::swap;

But it doesn't do anything special.
Paths in `use` declarations are automatically absolute paths.

> Is `::<` one token or two?

Two:

    Vec::/**/<i32>::new()    // ok

> Can a block contain a module?

Yes!

    println!("{}", {
        mod x { pub const Q: i32 = 3; }
        x::Q
    });

Why not, right? Macros will find a use for every scrap of orthogonality
the language gives us.

> Can a module declared in a block be the target of `use` declarations?

Not from outside it:

    {
        mod a { pub fn f() {} }
        use self::a::f;  // error: unresolved import
        println!("{:?}", f());
    }

Within the local module, a relative import (`use super::m` or `self::m`)
will do the trick:

    {
        mod a {
            fn f() {}
            pub mod b {
                use super::f;  // works, refers to a::f above
                pub fn g() { f() }
            }
        }
        println!("{:?}", a::b::g());
    }


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

> When is a non-`pub` member of a module visible from another module?

I don't know, but certainly:

    mod a {
        pub struct X { f: i32 }
        mod b { /* X::f is accessible here */ }
    }
    mod c { /* X::f is not accessible here */

@@@

> Can the fields of a tuple struct be public?

Yes:

    pub struct A(pub i32, pub i32);

If any field of a tuple struct isn't visible to you,
then you can't invoke it as a constructor.

> Can individual variants of an `enum` be `pub`?

No, `pub` isn't grammatically allowed inside an `enum` declaration.

> Can a module export a type that's implemented as an `enum`
> (and some methods)
> without also exporting all the constructors?

I don't think so!

@@@


## Crates

> What exactly is a crate? What's an rlib and what is in it?

@@@

> Can an arbitrary crate (in source form or in binary form) be turned
> into a .so/.dylib/.dll shared library?

@@@

> Can crates be linked together into bigger crates?

@@@

> Can two crates depend on each other (a cyclic dependency)?

@@@

> How are macros "linked" across crates? Does the answer mean that dependencies
> must be compiled all the way down to an rlib before compilation on downstream
> crates begins?

@@@

> How are other syntactic extensions "linked" across crates? (It's unstable, I
> know.)

@@@

> Suppose a dependency of my project updates from 1.4.7 to 1.4.8. How do I tell
> Cargo to update? Do I have to recompile my project?

@@@

> What I'm getting at with all of the above is, what does it mean for a point
> release of a Rust crate to be "compatible" with the previous point release?
> Is there a "source compatible" vs. "binary compatible" distinction in Rust?

@@@

> But in particular, how is the `#[derive(Serialize)]` thing in
> `serde_json` implemented, within `serde_json`, and how is it handled
> by the compiler when `serde_json` is being consumed as an `extern
> crate`?

@@@


## Macros

> Does `assert!(foo)` print a message containing the expression source
> code (`"foo"`) if the assertion fails?

Yes - but the expression may have different whitespace, and comments are
stripped, because the message is reconstituted from the parsed expression AST.

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

> Macros are hygienic, right?

Only very slightly!

I'm still figuring out the rules, but it seems like `let` variables are the
only names that get any kind of hygiene marker at all.

You can't generate code, for example, that refers to names that are private to
the module where the macro was defined. You'll have to make them public.
Then you can refer to them using `$crate`.

>  Are closure and fn arguments protected by hygiene markers? Local items?

@@@

> If a macro takes an `expr` argument,
> and the actual argument contains a `break`, but the macro also
> inserts a loop, then which loop does the `break` break out of?
> (I guess hygiene would suggest it breaks out of the innermost loop
> lexically evident in the context in which the macro is invoked?
> Not actually totally sure of myself there.)

Lol, not even close. `macro_rules!` macros are nowhere near that hygienic.

    macro_rules! rep {
        ($i:ident, $v:ident, $r:expr, $s:expr) => {
            for $v in $r {
                println!("{} {}", $i, $v);
                $s
            }
        }
    }

    fn main() {
        for i in 1..6 {
            rep!(i, x, 1..6, { if i == 3 && x == 3 { break; }});
        }
    }

> Is `my_macro! BONK` a possible macro-use syntax?

I don't think that's something the parser would accept at all, no.

There is surprisingly specific macro syntax for specific contexts:
expression macros, as in `(3 + f![])`, can use `()` or `[]` or `{}`
and no semicolon is expected or allowed for any of the three.
Declaration macros are different:
an identifier token is permitted before the mandatory delimited token-tree,
and if the delimited token-tree uses `()` or `[]`,
then a semicolon is required after.

Macros are permitted in a *lot* of different contexts;
I'll have to look to see what patterns there are.

@@@

> What about `my_macro!(X)` where X is required to be a single token tree?

Yes, a `macro_rules` macro can capture a token tree:

    macro_rules! my_macro {
        ($x:tt) => { ... }
    }

I wrote a `json!` macro that works that way and is actually kind of neat.


## Unsafe code

> The documentation for `std::mem::transmute::<T, U>()` says "Both types must
> have the same size." What if they don't?

Compile-time error.

(This can be abused to create static assertions.)

> Does Rust have any strict-aliasing rules that unsafe code must follow?

@@@

> Is it Undefined Behaviour if unsafe code changes a value (via a pointer)
> while a reference exists that points to it?

@@@

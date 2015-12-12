

> Does `assert!(foo)` print a message containing the expression source
> code (`"foo"`) if the assertion fails?

Yes - but the expression may have different whitespace, and comments are
stripped, as though it's reconstructed from the string of tokens.

    assert!(a==b);

    thread '<main>' panicked at 'assertion failed: a == b', src/main.rs:4

> What kind of code does `assert!(x)` expand to?
> How can I tell what a macro expands to, generally?

@

> How does the "automatic block return value if the semicolon is
> missing" thing work grammatically?  Does rust have redundant statement
> and expression syntax?
>
> What if you do `let x = if cond { V1 };` with no `else` clause?
> What is the type? What if the type has a Default?

@

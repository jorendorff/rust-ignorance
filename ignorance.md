

> Does `assert!(foo)` print a message containing the expression source
> code (`"foo"`) if the assertion fails?

Yes - but the expression may have different whitespace, and comments are
stripped, as though it's reconstructed from the string of tokens.

> What kind of code does `assert!(x)` expand to?
> How can I tell what a macro expands to, generally?

@


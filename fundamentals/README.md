Everything in Haskell is a **function**.

# Interactive `ghci`

In ghc interactive shell:

* `:quit`
* `:l <script.hs>`
* `:r` reload script
* `:t <type>` examine the type
* `:info <typeclass>` check the typeclass
* `:m + Data.List Data.Map`, load modules to global bname space
* `:k <type>`, show the kind of a type

In ghci:

* To define a function, must use `let` in the head


# To compile
```bash
ghc --make helloworld
./helloworld
```

# `.` and `$`
`.` is a higher order function for function composition.

```hs
Prelude> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Note also the `f.g.h x` is not equivalent to `(f.g.h) x`, because it is interpreted as `f.g.(h x)` which won't typecheck unless `(h x)` returns a function.

This is where the `$` operator can come in handy: `f.g.h $ x` turns `x` from being a parameter to `h` to being a parameter to the whole expression. And so it becomes equivalent to `f(g(h x))` and the pipe works again.

```hs
Prelude> :t ($)
($) :: (a -> b) -> a -> b
```


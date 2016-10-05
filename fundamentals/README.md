Everything in Haskell is a **function**.

# Interactive `ghci`

In ghc interactive shell:

* `:quit`
* `:l <script.hs>`, load
* `:r` reload scrip
* `:t <type>` examine the type
* `:info <typeclass>` check the typeclass
* `:m + Data.List Data.Map`, load modules to global bname space
* `:k <type>`, show the kind of a type

In ghci:

* To define a function, must use `let` in the when do declarations.
```hs
let pat x y z = x * (y + z)
```

# Comilaton `ghc`
```bash
ghc --make helloworld
./helloworld

# or
runhaskell helloworld.hs
```

# Hoogle
Use the type info of a customized function to search on Hoogle to check whether there are existing built-ins.

# `.` and `$`
`.` is a higher order function for function composition.

```hs
Prelude> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

Note also the `f.g.h x` is not equivalent to `(f.g.h) x`, because it is interpreted as `f.g.(h x)` which won't typecheck unless `(h x)` returns a function.

This is where the `$` operator can come in handy: `f.g.h $ x` turns `x` from being a parameter to `h` to being a parameter to the whole expression. And so it becomes equivalent to `f(g(h x))` and the pipe works again.

```hs
Prelude> :t ($)
($) :: (a -> b) -> a -> b
```

# Typeclass
Typeclass (`class`) is like interface in OOP. Type (`data` for new / `type` for synonym) can behave in that way are made instances (`deriving` / `instance`) of that typeclass.

```hs
-- | implementation of "Eq" typeclass
-- rename funcs, otherwise Ambiguous occurrence ‘==’
class Eq' a where  -- a type variable, equiv equitable
    (.==) :: a -> a -> Bool  -- type declarations
    (./=) :: a -> a -> Bool
    x .== y = not (x ./= y)  -- mutual recursion, thus only need to define == later in instance
    x ./= y = not (x .== y)
```

Type constructor is like Java generics, taking type parameters to produce a concrete type.

# Functor
Functors are things that can be **mapped** over, like lists.
```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

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

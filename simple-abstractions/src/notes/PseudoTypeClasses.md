# Pseudo Type Classes

Type classes are only built-in. Type-class instance rules are built in. Literals
have a similar sort of polymorphism to Haskell, where the type of `1` is
implicitly casted (can fail), but here we don't actually have a type-class that
corresponds to that (e.g. `Num` in Haskell). So, we should be able to write both
`1 : Int` and `1 : -Int` (the reversed-order `Int`)
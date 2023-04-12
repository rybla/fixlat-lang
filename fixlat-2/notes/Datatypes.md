# Datatypes

In Fixlat, each type specifies a lattice over terms of that type. The user can
define their own datatypes, which use some primitive combinators (sum, product,
fixpoint), and also has access to type operators that are relevant to the
lattice derivation, but not to the data structure.

!TODO actually do these need to actually be type operators? Perhaps we can have
a raw "structural" type that just defines the structure, with lattice
annotations that are inferred/defaulted during lattice-checking. And the
would-be-operators then are just casts.

## Datatype Grammar

The usual sum-of-products-with-fixpoints grammar for datatypes.

```
<type> ::=
  | <atomic-type>
  | <ctr> + <type> | <ctr>
  | <fld> * <type> | <fld>
  | fix <type-name> in <type>
  | <type-name>
  | <type> : <lattice> // lattice ascription

<fld> ::=
  | {<fld-name> : <type>}

<ctr> ::=
  | <ctr-name>[<type>]
```

Syntax sugar:
```
{<fld-name> : <term> *  ... * <fld-name> : <term>}
{<fld-name> : <term> * {... * {<fld-name> : <term>}}}
```

## Term Grammar

```
<term> ::=
  | <atomic-term>
  | <term-name>
  | {<fld-name> = <term>, <term>} | {<fld-name> = <term>}
  | <ctr-name>[<term>]
```

Syntax sugar:
```
{<fld-name> = <term>, ..., <fld-name> = <term>}
{<fld-name> = <term>, {... {<fld-name> = <term>}}}
```

## Lattice Grammar

```
<lattice> ::=
  | <atomic-lattice>
  | <lattice-ctr> + <lattice>
  | <lattice-fld> * <lattice>
  | - <lattice> // negation
  | @ // discretize
  | fix <type-name> in <lattice>
  | <type-name>

<lattice-fld> ::=
  | <fld-name> : <lattice>

<lattice-ctr> ::=
  | <ctr-name>[<lattice>]
```

## Datatype Lattices

**Sum.** The lattice `lat1 + lat2` is the sum lattice, where the sum group
labels define if comparisons can only be made among groups of constructrs. As a
shorthand, all consructrs of the same datatype that have no sum group label are
assumed to be of a same default sum group for that sum.

```
type A : A1[Nat] + A2[Nat] + @
type A = A1[Nat] + A2[Nat] + A3[Nat]

query A1[8] ≺ A2[5]
query A3[6] >< A1[8] // since A3[6] : A3[Nat] : @
query A3[6] >< A2[5] // since A3[6] : A3[Nat] : @
```

**Product.** The lattice `lat1 * lat2` is the product lattice with
lexicographical lattice ordering i.e. the first components are compared first,
and upon equality in the first components the second components are compared.

```
type B : {x : Nat * y : Nat}
type B = {x : Nat * y : Nat}

query (x = 1, y = 2)  ≺ {x = 1, y = 3} // eq in x, lt in y
query (x = 1, y = 10) ≺ {x = 2, y = 5} // lt in x, ignore in y
```

**Negation.** The lattice `- lat` is the lattice with the inverted ordering of
`lat`.

```
type C : C[- Nat]
type C = C[Nat]

query   1  ≺   2
query C[1] ≻ C[2]
```

**Discretize.** The lattice `@` is the lattice where all elements are unique.

```
type D : %
type D = D[Nat]

query   1  ≺    2
query D[1] >< D[2]
```

## Latticing

Analogous to how _typing_ is the attempt to judge that a given term has a given
type; _latticing_ is the attempt to judge that a given type has a given lattice.
This pseudo-implementation outlines a bidirectional-latticing algorithm. Note
that some lattice structure, such as negation, `- <lattice>`, is not reflected
exactly in the type structure; this structure is implicitly inferred, or can be
ascribed via `(<type> : <lattice>)`.

**Lattice checking.**
```
// checks that this lattice can be over this type, 
check : Lattice -> Type -> ()
check lat typ =
  lat' <- synth typ
  unify lat lat'
  ()
```

**Lattice synthesis.**
```
// infers the default lattice or checks ascription
synth : Type -> Lattice

// check, then unwrap ascription
synth (typ : lat) = 
  check lat typ
  lat 

synth atm = {{synth atomic type's lattice}}

synth (ctr_name[ctr_typ] + typ) = 
  {{map ctr_typ}}
  {{map typ}}

synth ({fld_name : fld_typ} * typ) = 
  fld_lat <- synth fld_typ
  lat <- synth typ
  {fld_name : fld_lat} * lat

synth (fix typ_name in typ) = 
  {{intro type_name}} 
  lat <- synth typ
  fix typ_name in lat

synth typ_name = {{lookup typ_name}}
```

**Lattice unification.**
- maps over sum, product, and fixpoint lattices
- infers negations 
- if expected lattice is discrete lattice, ignores inferred lattice
```
// unify lattices an expected lattice with an inferred lattice
unify : Lattice -> Lattice -> Lattice

unify atm atm' = 
  assert atm == atm'
  atm

unify (ctr_name1[ctr_lat1] + lat1') (ctr_name2[ctr_lat2] + lat2') = 
  ctr_name <- assert ctr_name1 == ctr_name2; ctr_name1
  ctr_lat <- unify ctr_lat1 ctr_lat2
  lat <- unify lat1' lat2'
  ctr_name[ctr_lat] + lat

unify ({fld_name1 : fld_lat1} * lat1') ({fld_name2 : fld_lat2} * lat2')
  fld_name <- assert fld_name1 == fld_name2; fld_name1
  fld_lat <- unify fld_lat1 fld_lat2
  lat <- unify lat1' lat2'
  {fld_name : fld_lat} * lat

unify (fix typ_name1 in lat1) (fix typ_name2 in lat2) =
  typ_name <- assert typ_name1 == typ_name2; typ_name1
  intro type_name1
  lat <- unify lat1 lat2
  fix typ_name in lat

unify typ_name1 type_name2 = 
  assert typ_name1 == typ_name2;
  typ_name1

// 
unify (- lat1) (- lat2) = lat <- unify lat1 lat2; - lat
unify (- lat1)    lat2  = lat <- unify lat1 lat2; - lat
unify    lat1  (- lat2) = lat <- unify lat1 lat2; - lat

// if expected lattice is discrete, then discretize inferred lattice
// expected discrete lattice unifies with any inferred lattice
unify @ lat2 = @
// inferred discrete lattice only unifies with expected discrete lattice
unify @ @ = @

unify _ _ = error
```
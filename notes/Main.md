# Fix-Lang

A programming language (or DSL, or library, or ...) based on the following
features:
- fixpoint programming style
- relations instead of functions and datatypes
- strong static typing

## Concepts

In functional programming, fundamentally, the user posits _equations_ that give
rise to functions.

In fixpoint programming, fundamentally, the user posits _rules_ that give rise
to relations.

__Inference__ can produce more rules given existing rules in the following ways:
- !TODO decide on a derivation framework (sequent or Hilbert)

A __concrete__ instance of a relation is simply a rule that has no premises and
no free variables.

The __fixpoint__ of a rule system (under inference) is a set of rules (some of
which are concrete instances). I say set of _rules_ rather than set of _concrete
instances_ because the fixpoint also includes all of the partially-instantiated
rules.

It is not feasible to directly compute the fixpoint of a rule system, so instead
the basic interface to the fixpoint is by querying, where a __query__ is an
__expression__ (can have free variables). The query is responded to be a subset
of the rule system's fixpoint that each have as its conclusion an expression
that __matches__ the queried expression. In other words, the query filters the
fixpoint to all the rules that can immediately derive an expression that matches
the queried expression.

Some interesting aspects of this style of programming:
- A single rule system can give rise to multiple functional relations (i.e.
  functions), and in this way serves as more of a specification as opposed to an
  implementation.
  - For example, a rule system of typing rules over `Γ ⊢ a : A` (with functional
    dependency `Γ, a -> A`) gives rise to these functions:
    - type checking via queries `Γ ⊢ a : A`
    - type inference via queries `Γ ⊢ a : ?A`
    - type inhabitancy via queries `Γ ⊢ ?a : A` (!NOTE this could be a kind of
      proof search?)
- A rule system does not commit to a particular evaluation strategy (such as an
  imperative method system) or a particular [!TODO what's the right
  word for this?] (such as a functional equation system). Instead, a rule system
  has a flexible query implementation in the same way that a functional equation
  system has a flexible evaluation implementation.

## Examples

### Logic

#### And

```
syntax _ ∨ _

rule intro-∨-left(P, Q)
  P
  -------
  P ∨ Q

rule intro-∨-right(P, Q)
  Q
  -------
  P ∨ Q

rule elim-∨(P, Q, R)
  P ⊢ R
  Q ⊢ R
  P ∨ Q
  --------------
  R
```

#### Or

```
syntax _ ∧ _

rule intro-∧(P, Q)
  P
  Q
  ----------------
  P ∧ Q

rule elim-∧-right(P, Q)
  P ∧ Q
  ---------------
  P

rule elim-∧-right(P, Q)
  P ∧ Q
  ---------------
  Q
```

#### Forall

!NOTE `x` is a name, so not a parameter, but the rule defines a scheme of rules
for each possible name, so basically the name is an implicit parameter

!NOTE `P[x]` indicates that `x` is a free variable in P

Univeresal quantification basically acts like a `lambda` from functional
programming.

```
syntax ∀ <<x>> . _

syntax intro-∀(P)
  P[x]
  -------------------------
  ∀ x . P[x]

syntax elim-∀(P_, a)
  ∀ x . P[x]
  ----------------
  P[x := a]
```

#### Exists

Existential quantification basically acts like a `let` from functional
programming.

```
syntax ∃ <<x>> := _ . _

syntax intro-∃(P, a)
  P[x := a]
  -------------------------
  ∃ x := a . P[x]

syntax elim-∃(P)
  ∃ x := a . P[x]
  -------------------------
  P[x := a]
```

### Typing

```
syntax _ : _
```

### Equivalence

```
syntax _ ≡ _

rule refl(a)
  --------------
  a ≡ a
```

### Typing Natural Numbers

```
syntax Nat
syntax 0
syntax 1 + _

rule
  -----------------
  0 : Nat

rule(n)
  n : Nat
  ---------------
  (1 + n) : Nat
```

Functional dependency: in `a : A`, `a -> A`. 
- querying `a : ?` corresponds to type inference
- querying `? : A` corresponds to type inhabitants

### Inductive Datatype over Natural Numbers

Once we've defined the type of natural numbers, we can posit that the
inhabitants of that type form a datatype. A type is an __inductive datatype__ if
you can define the recursion principle over the inhabitants of the type. For
natural numbers, the recursion principle has the type:

```
rec-Nat : 
  a ->        // 0
  (a -> a) -> // 1 + _
  Nat -> a
```

#### Datatype property in terms of recursion function

One way to posit that the inhabitants of `Nat` form a datatype is defining a
functional relation that corresponds to `rec-Nat` as described above.

!TODO should I allow higher-order syntax?
```
// the `_1` indicates that any expression that goes in that place must have 
// exactly 1 syntactical parameter
syntax rec-Nat _ _1 _ => _

rule rec-Nat-0(z, s_)
  ----------------------
  rec-Nat z s 0 => z

// here, we see why `s` must have exactly 1 syntactical parameter, which is 
// instantiated by `a`
rule rec-Nat-1+(z, s_, a)
  rec-Nat z s n => a
  ----------------------
  rec-Nat z s (1 + n) => (s a)
```

Without higher-order syntax, defined in terms of specific "application" syntax:
```
syntax rec-Nat _ _ _ => _

// this should probably be introduced in a previous section so that it can be 
// used in many places
syntax [_ _]

rule rec-Nat-0(z, s)
  ----------------------
  rec-Nat z s 0 => z

rule rec-Nat-1+(z, s, a)
  rec-Nat z s n => a
  ----------------------
  rec-Nat z s (1 + n) => [s a]
```

#### Datatype property in terms of equivalence

An alternative way to posit that the inhabitants of `Nat` form a datatype is in
terms of equivalence.

```
// !NOTE sequent calculus style: hypotheses conjoint, conclusions disjoint 
rule destruct-Nat(n, m)
  n : Nat
  -----------------
  n ≡ 0
  n ≡ (1 + m)

rule induct-Nat(P_, n)
  n : Nat
  P 0
  ∀ m . (((n : Nat) ∧ (P n)) ⊢ (P (1 + n)))
  ------------------------
  p n

// !NOTE this rule is implied by `refl(0)`
rule equiv-0
  0 ≡ 0

rule equiv-1+(n, n')
  n ≡ n'
  ------------------
  (1 + n) ≡ (1 + n')
```

#### Datatype property in terms of propositions

```
rule induct-Nat(P, n)
  P 0
  ∀ m . (((P m) ∧ (m : Nat)) ⊢ P (1 + m))
  n : Nat
  ------------
  P n
```

### Functions over Natural Numbers

We define addition over natural numbers as a 3-way relation `_ + _ => _`.

```
syntax _ + _ => _

rule(n)
  -----------------
  0 + n => n

rule(l, m, n)
  l + m => n
  -----------------
  (1 + l) + m => 1 + n

// typing

rule(l, m, n)
  l : Nat
  m : Nat
  n : Nat
  --------------------
  (l + m => n) : Nat

// functional dependencies
//   l <- m, n
//   m <- l, n
//   n <- l, m

// l is determined by m, n
rule(l, l', m, n)
  l  + m => n
  l' + m => n
  ------------------------
  l ≡ l'

// m is determined by l, n
rule(l, m, m', n)
  l + m  => n
  l + m' => n
  ------------------------
  m ≡ m'

// n is determined by m, n
rule(l, m, n, n')
  l + m => n
  l + m => n'
  ------------------------
  n ≡ n'
```

Although we've defined it as a relation, we also know that the relation is
__functional__ i.e. it defines a function since for any `a`, `b`, there exists a
unique `c` such that `a + b => c`.

Evaluation of the function corresponds to making a query to the rule system as
an abstract instance (i.e. an instance of the relation with free variables)
which is responded to with an encoding of all derivable instances that match the
query.

_Example._ Query `l + 0 => n`. Response:

```
rule
  l + 0 = n
  --------------------
  (1 + l) + m => (1 + n)

rule
  --------------------
  0 + 0 => 0
```

_Example._ Query `(1 + 0) + 0 => n`. Response:

!TODO should the second rule override the first?

```
rule
  0 + 0 => n
  ------------------
  (1 + 0) + 0 => 1 + n

rule
  ------------------
  (1 + 0) + 0 => (1 + 0)
```



## Lattice

A  _lattice_ is a partially-ordered set that, for any finite subset, has a
join (least upper bound) and a meet (greatest lower bound). A lattice has
the following properties:

```
commutativity:
  a ∨ b = b ∨ a
  a ∧ b = b ∧ a

associativity:
  a ∨ (b ∨ c) = (a ∨ b) ∨ c
  a ∧ (b ∧ c) = (a ∧ b) ∧ c

absorbtion:
  a ∨ (a ∧ b) = a
  a ∧ (a ∨ b) = a

idempotentcy:
  a ∨ a = a
  a ∧ a = a
```
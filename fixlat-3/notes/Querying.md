# Querying

TODO
- [ ] can make sense of dist_step over power sets?

## Rule Grammar

A rule has the form.

```
<rule> ::=
	<label> : ∀ <param*> .
		<prop*>	// hypotheses
		----------------
		<prop> 	// conclusion

<param> ::= <term-name> : <type>

<prop> ::= <pred-name>(<term>)
```

## Rule Lattice

See [Datatypes](Datatypes.md) for how to derive a lattice for a
type which corresponds to a lattice over terms of that type.

Rules form a lattice that arises from the lattices over typed terms.

**Lattice over propositions.**
```
a < b
---------------------------
pred(a) < pred(b)


a \/ b  = c
------------------------------------------
pred(a) /\ pred(b) = pred(c)


a \/ b = c
-------------------------------------------
pred(a) \/ pred(b) = pred(c)
```

**Lattice over rules.** Note contravariance in hypotheses; a lesser/weaker
hypothesis corresponds to a stronger/greater rule.
```
... p'_i < p_i ...
q  < q'
------------------------------------
(∀ x . ... p_i ... ⊢ q) < (∀ x . ... p'_i ... ⊢ q')
```

!TODO handle re-ordering/subsumption of hypotheses, since one hypothesis in one
rule could imply/be implied by multiple hypotheses in another rule (what about
group as well? or does this not get handled in the immediate inference rule)

!TODO handle unification of rules via substitution of quantified variables,
where the rule that can be substituted to get the other rule is stronger/greater
(since it's more general)

## Querying Algorithm

**State**.
The state is initialized with a lattice-set of rules, `predSets`.

**Learning rule**.
To *learn* a rule, insert it into the state's `predSets`.

**Orderings over rules**. The lattice-set of rules in the state keeps track of
only to _highest_ (strongest) rules in the rule lattice. There is also
a weaker ordering over rules which determined the order of priority to use them
during querying. This order is 

### Querying a rule

The query rule has the form:
```
// Q_i : Quant
// x_i : Var
// hyp_j : CProp
// con : CProp

R_query : ... Q_i x_i ... .
	... hyp_j ...
	----------------
	con
```

- *Generalize* the query rule.
  - Convert the rule's parameters into metavars
  - Locally store quantifications of intro'd metavars
- *Learn* the hypotheses.
- *Query* the conclusion.

### Querying a proposition

The query proposition has the form:
```
// p : Var
// t : MTerm
p(t)
```

- For each rule in the state.
  - Attempt to *apply* the rule to the query prop.
    - *Generalize* the rule.
    - Try to *unify* the conclusion of the rule with the query prop.
  - If the application is valid, then *query* the hypotheses of the rule in
    order 
		- !TODO The hypotheses are queried in order of appearance, but this behavior
		  might not be ideal.
  - If all the hypothesis queries succeed
    - *learn* the conclusion.
    - done with this query, so break iteration
      - !TODO Does this require some advanced notion of backtracking, where if I
        fail later on in querying a different query down the line, then I need
        to come back here and try the next rule to prove the prop even though I
        succeeded in this intermediate query using this rule?

### Example

Module:
```
// Nat
type Nat = fix Nat' in Zero | Suc(Nat')

// Weight
// - The `- Nat` lattice annotation yields an `Nat` type that has the inverse 
//   lattice ordering of the base `Nat` lattice.
type Weight = Nat : - Nat

// add
predicate add(Weight * Weight * Weight)
rule add_Zero : ∀ w.
	⊢ add(w, Zero, w)

rule add_Suc : ∀ w1 w2 ∃ w3.
	add(w1, w2, w3)
	⊢ add(w1, Suc(w2), Suc(w3))

// Node
type Node = Node(Nat)

// Path
type Path = Node * Weight * Node

// step
predicate step(Path)

... step rules that encode graph

// dist
predicate dist(Path)

rule dist_self : ∀ n.
	⊢ dist(n, 0, n)

rule dist_step : ∀ n1 w1 n2 w2 n3 w3.
	step(n2, w2, n3) // first, so is queried first
	dist(n1, w1, n2)
	add(w1, w2, w3)
	⊢ dist(n1, w3, n3)
```

Query:
```
// nStart, nFinish are some specific nodes
query : ∃ w.
	dist(nStart, w, nFinish)
```

Query trace:
- intro `∃ w`
- look for producers of `dist(nStart, w, nFinish)`
  - found rule `dist_step` where
    - `n1 := nStart`
    - `n3 := nFinish`
    - `w3 := w`
    - intro `∀ w1 n2 w2 n3`
      - !TODO is this forall or exists?
  - query `step(n2, w2, nEnd)`
  - query `dist(nStart, w1, n2)`

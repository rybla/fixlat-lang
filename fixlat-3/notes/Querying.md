# Querying

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
The state is initialized with a lattice-set of rules.

**Orderings over rules**. The lattice-set of rules in the state keeps track of
only to _highest_ (strongest) rules in the rule lattice. There is also
a weaker ordering over rules which determined the order of priority to use them
during querying. This order is 

### Querying a rule

The queried rule has the form.

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

First, **generalize** the rule.
- convert the bound rule parameters into bound metavariables
- 

### Example


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

The environment, `env`, statefully keeps track of all the known rules. `env` is
initialized with all the rules postulated in a module.

**Learn a rule**.

Learn a rule `r` by inserting it into `env` such that, for any comparable rule
`r'` that is already in `env`, update `env` to only have `r \/ r'`.

**Query a rule**.

**Query a prop**. 



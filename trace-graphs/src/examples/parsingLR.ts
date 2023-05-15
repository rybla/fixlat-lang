// This is BOTTOM-UP!!

import { Key, makeNode, makeLink, makeLinks, renderRule, makeAxiom } from "./rule_up"

const str = "((a))"

/* ===========================================================================
  Utilities
=========================================================================== */

// indexing maps with tuples of numbers
function index2(i: number, j: number): String {
  return `${i}_${j}`
}
function index3(i: number, j: number, k: number): String {
  return `${i}_${j}_${k}`
}

function has_such_that<k, v>(m: Map<k, v>, k: k, p: (v: v) => Boolean): Boolean {
  const v = m.get(k)
  return v !== undefined && p(v)
}

/* ===========================================================================
  Parses
=========================================================================== */

type Rule1_H3 = {
  case: 'rule1_h3',
  i: number, j: number, k: number, l: number, // indices
  rule1_h2: Key, rpar: Key, // hyps
  key: Key // this
}

type Rule2_H2 = {
  case: 'rule2_h2',
  i: number, j: number, k: number, // indices
  rule2_h1: Key, s2: Key, // hyps
  key: Key // this
}

type Rule3_H1 = {
  case: 'rule3_h1',
  i: number, j: number, // indices
  a: Key, // hyps
  key: Key // this
}

// Parse from index i to index j
type S_Parse = {
  i: number,
  j: number,
  key: Key,
  deriv: Rule1_H3 | Rule2_H2 | Rule3_H1
}

const s_parses: Map<String, S_Parse> = new Map()

function get_s_parse(i: number, j: number): S_Parse | undefined {
  return s_parses.get(index2(i, j))
}

// Overwrites any existing sParses at the parse's index range.
function learn_s_parse(parse: S_Parse): void {
  s_parses.set(index2(parse.i, parse.j), parse)
}

/* ===========================================================================
  Axioms
    rule1: S -> (S)
    rule2: S -> SS
    rule3: S -> a
    char: forall c in str
=========================================================================== */

const rule1 = makeAxiom({
  label: `rule1`,
  vars: [`∀i`, `∀j`, `∀k`, `∀l`],
  hyps: [`i ( j`, `j S k`, `k ) l`],
  con: `i S l`
})

const rule2 = makeAxiom({
  label: `rule2`,
  vars: [`∀i`, `∀j`, `∀k`],
  hyps: [
    `i S j`,
    `j S k`
  ],
  con: `i S k`
})

const rule3 = makeAxiom({
  label: `rule3`,
  vars: [`∀i`, `∀j`],
  hyps: [`i a j`],
  con: `i S j`
})

type Char_Parse = {
  i: number, j: number,
  c: String,
  key: Key
}

const char_parses: Char_Parse[] = []
for (let i = 0; i < str.length; i++) {
  const c = str.charAt(i)
  const key = makeAxiom({ con: `${i} ${c} ${i + 1}` })
  char_parses.push({ i, j: i + 1, c, key })
}

// Application of rule1 with first 1 hyp instantiated
type Rule1_H1 = {
  i: number,
  j: number,
  lpar: Key,
  key: Key
}

// rule1_h1s: i_j => Rule1_H1
const rule1_h1s: Map<String, Rule1_H1> = new Map()

function learn_Rule1_H1(i: number, j: number, lpar: Key): Rule1_H1 | undefined {
  const ix = index2(i, j)

  // ignore if already learned this instance
  if (has_such_that(rule1_h1s, ix, rule1_h1 => rule1_h1.lpar === lpar)) return undefined

  const key = makeNode(renderRule({
    label: `rule1`,
    vars: [`∀k`, `∀l`],
    hyps: [`${j} S k`, `k ) l`],
    con: `${i} S l`
  }))
  const rule1_h1: Rule1_H1 = { i, j, lpar, key }
  rule1_h1s.set(ix, rule1_h1)
  makeLinks([rule1, lpar], "rule1 (hyp1 1)", [key])
  return rule1_h1
}

// Application of rule1 with first 2 hyps instantiated
type Rule1_H2 = {
  i: number,
  j: number,
  k: number,
  rule1_h1: Key,
  s: Key,
  key: Key
}

// rule1_h2s: i => j => k => Rule1_H2
const rule1_h2s: Map<String, Rule1_H2> = new Map()

function learn_Rule1_H2(i: number, j: number, k: number, rule1_h1: Key, s: Key): Rule1_H2 {

  const ix = index3(i, j, k)

  // ignore if already learned this instance
  if (has_such_that(rule1_h2s, ix, rule1_h2 => rule1_h2.rule1_h1 === rule1_h1)) return undefined

  const key = makeNode(renderRule({
    label: `rule1`,
    vars: [`∀l`],
    hyps: [`${k} ) l`],
    con: `${i} S l`
  }))
  const rule1_h2: Rule1_H2 = { i, j, k, rule1_h1, s, key }
  rule1_h2s.set(ix, rule1_h2)
  makeLinks([rule1_h1, s], "rule1 (hyp 2)", [key])
  return rule1_h2
}

function learn_Rule1_H3(rule1_h2: Rule1_H2, rpar: Char_Parse): S_Parse {
  const i = rule1_h2.i
  const j = rule1_h2.j
  const k = rule1_h2.k
  const l = rpar.j

  // !TODO
  // const ix =

  // // ignore if already learned this instance
  // if (hasSuchThat(rule1_h2s, ix, rule1_h2 => rule1_h2.rule1_h1 === rule1_h1)) return undefined

  const key = makeNode(renderRule({
    con: `${i} S ${l}`
  }))
  const parse: S_Parse = {
    i: i, j: l,
    key,
    deriv: {
      case: 'rule1_h3',
      i, j, k, l,
      rule1_h2: rule1_h2.key,
      rpar: rpar.key,
      key
    }
  }
  learn_s_parse(parse)
  makeLinks([rule1_h2.key, rpar.key], "rule1 (hyp 3)", [key])
  return parse
}

// Application of rule2 with first 1 hyp instantiated
type Rule2_H1 = {
  i: number,
  j: number,
  s1: Key,
  key: Key
}

const rule2_h1s: Map<String, Rule2_H1> = new Map()

function learn_Rule2_H1(s1: S_Parse): Rule2_H1 {
  const i = s1.i
  const j = s1.j
  const ix = index2(i, j)

  // !TODO check for duplicate

  const key = makeNode(renderRule({
    label: `rule2`,
    vars: [`∀k`],
    hyps: [`${j} S k`],
    con: `${i} S k`
  }))
  const rule2_h1: Rule2_H1 = { i, j, s1: s1.key, key }
  rule2_h1s.set(ix, rule2_h1)
  makeLinks([rule2, s1.key], "rule2 (hyp 1)", [key])
  return rule2_h1
}

function learn_Rule2_H2(rule2_h1: Rule2_H1, s2: S_Parse): S_Parse {
  const i = rule2_h1.i
  const j = rule2_h1.j
  const k = s2.j
  const ix = index2(i, k)

  // !TODO check for duplicate

  const key = makeNode(renderRule({
    con: `${i} S ${k}`
  }))
  const parse: S_Parse = {
    i: i, j: k, key,
    deriv: {
      case: 'rule2_h2',
      i, j, k,
      rule2_h1: rule2_h1.key, s2: s2.key,
      key
    }
  }
  learn_s_parse(parse)
  makeLinks([rule2_h1.key, s2.key], "rule2 (hyp 2)", [key])
  return parse
}

function learn_Rule3(i: number, j: number, a: Char_Parse): S_Parse {
  const key = makeNode(renderRule({
    con: `${i} S ${j}`
  }))
  const parse: S_Parse = {
    i, j, key,
    deriv: {
      case: 'rule3_h1',
      i, j,
      a: a.key,
      key
    }
  }
  learn_s_parse(parse)
  makeLinks([rule3, a.key], "rule3", [key])
  return parse
}

/* ===========================================================================
  Parsing fixpoint
=========================================================================== */

const lparParses: Char_Parse[] = []
const rparParses: Char_Parse[] = []
const aParses: Char_Parse[] = []
for (let i = 0; i < str.length; i++) {
  const c = str.charAt(i)
  const p = char_parses[i]
  if (c == '(') { lparParses.push(p) }
  else if (c == ')') { rparParses.push(p) }
  else if (c == 'a') { aParses.push(p) }
  else { throw new Error(`Input string should not have character: '${c}'`); }
}

// apply rule 1 (hyp 1)
lparParses.forEach(lpar => {
  learn_Rule1_H1(lpar.i, lpar.j, lpar.key)
})

// apply rule 3 (hyp 1)
aParses.forEach(a => {
  learn_Rule3(a.i, a.j, a)
})

function loop() {
  // !TODO need a check to make sure that i dont use a rule on the same
  // hypothesis twice

  const sParses_old = new Map(s_parses)
  const rule1_h1s_old = new Map(rule1_h1s)
  const rule1_h2s_old = new Map(rule1_h2s)
  const rule2_h1s_old = new Map(rule2_h1s)

  // apply each rule 1 (inst hyp 1)
  rule1_h1s_old.forEach(rule1_h1 => {
    // wants hypothesis: ∃k . j S k
    sParses_old.forEach(s => {
      if (s.i == rule1_h1.j) {
        learn_Rule1_H2(rule1_h1.i, rule1_h1.j, s.j, rule1_h1.key, s.key)
      }
    })
  })

  // apply each rule 1 (inst hyps 1, 2)
  rule1_h2s_old.forEach(rule1_h2 => {
    // wants hypothesis: ∃l . j ) l
    lparParses.forEach(rpar => {
      if (rpar.i == rule1_h2.j) {
        learn_Rule1_H3(rule1_h2, rpar)
      }
    })
  })

  // apply rule 2

  sParses_old.forEach(s1 => {
    learn_Rule2_H1(s1)
  })

  // apply each rule 2 (inst hyp 1)

  rule2_h1s_old.forEach(rule2_h1 => {
    sParses_old.forEach(s2 => {
      if (s2.i == rule2_h1.j) {
        learn_Rule2_H2(rule2_h1, s2)
      }
    })
  })

}

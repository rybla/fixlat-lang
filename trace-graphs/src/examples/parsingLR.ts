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

// // Parse from index i to index j
// type S_Parse = {
//   i: number,
//   j: number,
//   key: Key,
//   deriv: Rule1_H3 | Rule2_H2 | Rule3_H1
// }

type S_Parse = Rule1_H3 | Rule2_H2 | Rule3_H1

const s_parses: Map<String, S_Parse> = new Map()

function get_s_parse(i: number, j: number): S_Parse | undefined {
  return s_parses.get(index2(i, j))
}

// Overwrites any existing sParses at the parse's index range.
function add_s_parse(parse: S_Parse): void {
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
  hyps: [
    `i ( j`,
    `j S k`,
    `k ) l`
  ],
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
  hyps: [
    `i a j`
  ],
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

function learn_Rule1_H1(lpar: Char_Parse): Rule1_H1 | undefined {
  const i = lpar.i
  const j = lpar.j
  const ix = index2(i, j)

  // ignore if already learned this instance
  if (has_such_that(rule1_h1s, ix, rule1_h1 => rule1_h1.lpar === lpar.key)) return undefined

  const key = makeNode(renderRule({
    label: `rule1`,
    vars: [`∀k`, `∀l`],
    hyps: [
      `${j} S k`,
      `k ) l`
    ],
    con: `${i} S l`
  }))
  const rule1_h1: Rule1_H1 = { i, j, lpar: lpar.key, key }
  rule1_h1s.set(ix, rule1_h1)
  makeLinks([rule1, lpar.key], "rule1 (hyp1 1)", [key])
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

function learn_Rule1_H2(i: number, j: number, k: number, rule1_h1: Key, s: Key): Rule1_H2 | undefined {

  const ix = index3(i, j, k)

  // ignore if already learned this instance
  if (has_such_that(rule1_h2s, ix, rule1_h2 => rule1_h2.rule1_h1 === rule1_h1)) return undefined

  const key = makeNode(renderRule({
    label: `rule1`,
    vars: [`∀l`],
    hyps: [
      `${k} ) l`
    ],
    con: `${i} S l`
  }))
  const rule1_h2: Rule1_H2 = { i, j, k, rule1_h1, s, key }
  rule1_h2s.set(ix, rule1_h2)
  makeLinks([rule1_h1, s], "rule1 (hyp 2)", [key])
  return rule1_h2
}

type Rule1_H3 = {
  case: 'rule1_h3',
  i: number, j: number, k: number, l: number, // indices
  rule1_h2: Key, rpar: Key, // hyps
  key: Key // this
}

function learn_Rule1_H3(rule1_h2: Rule1_H2, rpar: Char_Parse): S_Parse | undefined {
  const i = rule1_h2.i
  const j = rule1_h2.j
  const k = rule1_h2.k
  const l = rpar.j
  const ix = index2(i, l)

  // ignore if already learned this instance
  if (has_such_that(s_parses, ix, parse =>
    parse.case === 'rule1_h3' &&
    parse.rule1_h2 === rule1_h2.key &&
    parse.rpar === rpar.key)
  ) return undefined

  const key = makeNode(renderRule({
    con: `${i} S ${l}`
  }))
  const parse: S_Parse = {
    // i: i, j: l,
    // key,
    // deriv: {
    //   case: 'rule1_h3',
    //   i, j, k, l,
    //   rule1_h2: rule1_h2.key,
    //   rpar: rpar.key,
    //   key
    // }
    case: 'rule1_h3',
    i, j, k, l,
    rule1_h2: rule1_h2.key,
    rpar: rpar.key,
    key
  }
  add_s_parse(parse)
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

function learn_Rule2_H1(s1: S_Parse): Rule2_H1 | undefined {
  const i = s1.i
  const j = s1.j
  const ix = index2(i, j)

  // ignore if already learned this instance
  if (has_such_that(rule2_h1s, ix, rule2_h1 => rule2_h1.s1 === s1.key)) return undefined

  const key = makeNode(renderRule({
    label: `rule2`,
    vars: [`∀k`],
    hyps: [
      `${j} S k`
    ],
    con: `${i} S k`
  }))
  const rule2_h1: Rule2_H1 = { i, j, s1: s1.key, key }
  rule2_h1s.set(ix, rule2_h1)
  makeLinks([rule2, s1.key], "rule2 (hyp 1)", [key])
  return rule2_h1
}

type Rule2_H2 = {
  case: 'rule2_h2',
  i: number, j: number, k: number, // indices
  rule2_h1: Key, s2: Key, // hyps
  key: Key // this
}

function learn_Rule2_H2(rule2_h1: Rule2_H1, s2: S_Parse): S_Parse | undefined {
  const i = rule2_h1.i
  const j = rule2_h1.j
  const k = s2.j
  const ix = index2(i, k)

  // ignore if already learned this instance
  if (has_such_that(s_parses, ix, parse =>
    parse.case === 'rule2_h2' &&
    parse.rule2_h1 === rule2_h1.key &&
    parse.s2 === s2.key
  )) return undefined

  const key = makeNode(renderRule({
    con: `${i} S ${k}`
  }))
  const parse: S_Parse = {
    // i: i, j: k, key,
    // deriv: {
    //   case: 'rule2_h2',
    //   i, j, k,
    //   rule2_h1: rule2_h1.key, s2: s2.key,
    //   key
    // }
    case: 'rule2_h2',
    i, j, k,
    rule2_h1: rule2_h1.key, s2: s2.key,
    key
  }
  add_s_parse(parse)
  makeLinks([rule2_h1.key, s2.key], "rule2 (hyp 2)", [key])
  return parse
}

type Rule3_H1 = {
  case: 'rule3_h1',
  i: number, j: number, // indices
  a: Key, // hyps
  key: Key // this
}

function learn_Rule3_H1(a: Char_Parse): S_Parse {
  const i = a.i
  const j = a.j
  const ix = index2(i, j)

  if (has_such_that(s_parses, ix, parse =>
    parse.case === 'rule3_h1' &&
    parse.a === a.key
  )) return undefined

  const key = makeNode(renderRule({
    con: `${i} S ${j}`
  }))
  const parse: S_Parse = {
    // i, j, key,
    // deriv: {
    //   case: 'rule3_h1',
    //   i, j,
    //   a: a.key,
    //   key
    // }
    case: 'rule3_h1',
    i, j,
    a: a.key,
    key
  }
  add_s_parse(parse)
  makeLinks([rule3, a.key], "rule3", [key])
  return parse
}

/* ===========================================================================
  Parsing fixpoint
=========================================================================== */

// Apply rule 1 (hyp 1) and rule 3 (hyp 1). This only involves one pass, since
// more '(' instances will never be found.
char_parses.forEach(char => {
  switch (char.c) {
    case '(': learn_Rule1_H1(char); break
    case 'a': learn_Rule3_H1(char); break
    default: break
  }
})

function loop() {

  const sParses_old = new Map(s_parses)
  const rule1_h1s_old = new Map(rule1_h1s)
  const rule1_h2s_old = new Map(rule1_h2s)
  const rule2_h1s_old = new Map(rule2_h1s)

  // Apply each rule 1 (inst hyp 1).

  rule1_h1s_old.forEach(rule1_h1 => {
    // Wants hypothesis: ∃k . j S k
    sParses_old.forEach(s => {
      if (s.i == rule1_h1.j) {
        learn_Rule1_H2(rule1_h1.i, rule1_h1.j, s.j, rule1_h1.key, s.key)
      }
    })
  })

  // Apply each rule 1 (inst hyps 1, 2).

  rule1_h2s_old.forEach(rule1_h2 => {
    // Wants hypothesis: ∃l . j ) l
    const char = char_parses.at(rule1_h2.j)
    if (char !== undefined && char.c === ')') {
      learn_Rule1_H3(rule1_h2, char)
    }
  })

  // Apply rule 2.

  sParses_old.forEach(s1 => {
    // Wants hypothesis: ∃i ∃j. i S j
    learn_Rule2_H1(s1)
  })

  // Apply each rule 2 (inst hyp 1).

  rule2_h1s_old.forEach(rule2_h1 => {
    // Wants hypothesis: ∃k . j S k
    sParses_old.forEach(s2 => {
      if (s2.i == rule2_h1.j) {
        learn_Rule2_H2(rule2_h1, s2)
      }
    })
  })

}


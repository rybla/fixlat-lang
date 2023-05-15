// This is BOTTOM-UP!!

import { Key, makeNode, makeLink, makeLinks, renderRule, makeAxiom } from "./rule_up"

// const str = "a"
const str = "(a)"
// const str = "aa"
// const str = "((a))"
// const str = "aaa"
// const str = "(a)(a)(a)"
console.log(`[parsingLR] str = \"${str}\"`)

// config

const _links_to_general_rules = true

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

type S_Parse = (Rule1_H3 | Rule2_H2 | Rule3_H1) & { str: String }

const s_parses: Map<String, S_Parse> = new Map()

function get_s_parse(i: number, j: number): S_Parse | undefined {
  return s_parses.get(index2(i, j))
}

function get_s_parse_bounds(s: S_Parse): { i: number, j: number } {
  switch (s.case) {
    case 'rule1_h3': return { i: s.i, j: s.l }
    case 'rule2_h2': return { i: s.i, j: s.k }
    case 'rule3_h1': return { i: s.i, j: s.j }
  }
}

// Overwrites any existing sParses at the parse's index range.
function add_s_parse(s: S_Parse): void {
  const bounds = get_s_parse_bounds(s)
  s_parses.set(index2(bounds.i, bounds.j), s)
}

/* ===========================================================================
  Axioms
    rule1: S -> (S)
    rule2: S -> SS
    rule3: S -> a
    char: forall c in str
=========================================================================== */

const rule1 = makeAxiom({
  label: `rule 1 : |(S)`,
  vars: [`∀i`, `∀j`, `∀k`, `∀l`],
  hyps: [
    `i ( j`,
    `j S k`,
    `k ) l`
  ],
  con: `i S l`
})

const rule2 = makeAxiom({
  label: `rule 2 : |SS`,
  vars: [`∀i`, `∀j`, `∀k`],
  hyps: [
    `i S j`,
    `j S k`
  ],
  con: `i S k`
})

const rule3 = makeAxiom({
  label: `rule3: |a`,
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
  key: Key,
  str: String
}

// rule1_h1s: i_j => Rule1_H1
const rule1_h1s: Map<String, Rule1_H1> = new Map()

function learn_Rule1_H1(lpar: Char_Parse): Rule1_H1 | undefined {
  const i = lpar.i
  const j = lpar.j
  const ix = index2(i, j)

  // ignore if already learned this instance
  if (has_such_that(rule1_h1s, ix, rule1_h1 =>
    // rule1_h1.lpar === lpar.key
    true
  )) return undefined

  const substr = `(`
  const key = makeNode(renderRule({
    label: `rule 1 : ${substr}|S)`,
    vars: [`∀k`, `∀l`],
    hyps: [
      `${j} S k`,
      `k ) l`
    ],
    con: `${i} S l`
  }))
  const rule1_h1: Rule1_H1 = {
    i, j, lpar: lpar.key,
    key,
    str: substr
  }
  rule1_h1s.set(ix, rule1_h1)
  if (_links_to_general_rules) {
    makeLinks([rule1, lpar.key], "rule 1 (hyp 1)", [key])
  } else {
    makeLinks([lpar.key], "rule 1 (hyp 1)", [key])
  }
  return rule1_h1
}

// Application of rule1 with first 2 hyps instantiated
type Rule1_H2 = {
  i: number,
  j: number,
  k: number,
  rule1_h1: Key,
  s: Key,
  key: Key,
  str: String
}

// rule1_h2s: i => j => k => Rule1_H2
const rule1_h2s: Map<String, Rule1_H2> = new Map()

function learn_Rule1_H2(rule1_h1: Rule1_H1, s: S_Parse): Rule1_H2 | undefined {
  const i = rule1_h1.i
  const j = rule1_h1.j
  const { j: k } = get_s_parse_bounds(s)
  const ix = index3(i, j, k)

  // ignore if already learned this instance
  if (has_such_that(rule1_h2s, ix, rule1_h2 =>
    // rule1_h2.rule1_h1 === rule1_h1.key
    true
  )) return undefined

  const substr = `${rule1_h1.str}${s.str}`
  const key = makeNode(renderRule({
    label: `rule 1 : ${substr}|)`,
    vars: [`∀l`],
    hyps: [
      `${k} ) l`
    ],
    con: `${i} S l`
  }))
  const rule1_h2: Rule1_H2 = {
    i, j, k,
    rule1_h1: rule1_h1.key, s: s.key,
    key,
    str: substr
  }
  rule1_h2s.set(ix, rule1_h2)
  makeLinks([rule1_h1.key, s.key], "rule 1 (hyp 2)", [key])
  return rule1_h2
}

type Rule1_H3 = {
  case: 'rule1_h3',
  i: number, j: number, k: number, l: number, // indices
  rule1_h2: Key, rpar: Key, // hyps
  key: Key, // this
  str: String
}

function learn_Rule1_H3(rule1_h2: Rule1_H2, rpar: Char_Parse): S_Parse | undefined {
  // console.log(`[learn_Rule1_H3]\n  - rule1_h2 = ${rule1_h2}\n  - rpar: ${rpar}`)
  const i = rule1_h2.i
  const j = rule1_h2.j
  const k = rule1_h2.k
  const l = rpar.j
  const ix = index2(i, l)

  // ignore if already learned this instance
  if (has_such_that(s_parses, ix, parse =>
    // parse.case === 'rule1_h3' &&
    // parse.rule1_h2 === rule1_h2.key &&
    // parse.rpar === rpar.key
    true
  )) return undefined

  const substr = `${rule1_h2.str})`
  const key = makeNode(renderRule({
    label: `rule 1 : ${substr}`,
    con: `${i} S ${l}`
  }))
  const parse: S_Parse = {
    case: 'rule1_h3',
    i, j, k, l,
    rule1_h2: rule1_h2.key,
    rpar: rpar.key,
    key,
    str: substr
  }

  console.log("[learn_Rule1_H3] parse")
  console.log(parse)

  add_s_parse(parse)
  makeLinks([rule1_h2.key, rpar.key], "rule 1 (hyp 3)", [key])
  return parse
}

// Application of rule2 with first 1 hyp instantiated
type Rule2_H1 = {
  i: number,
  j: number,
  s1: Key,
  key: Key,
  str: String
}

const rule2_h1s: Map<String, Rule2_H1> = new Map()

function learn_Rule2_H1(s1: S_Parse): Rule2_H1 | undefined {
  const { i, j } = get_s_parse_bounds(s1)
  const ix = index2(i, j)

  // ignore if already learned this instance
  if (has_such_that(rule2_h1s, ix, rule2_h1 =>
    // rule2_h1.s1 === s1.key
    true
  )) return undefined

  const substr = `${s1.str}`
  const key = makeNode(renderRule({
    label: `rule 2 : ${substr}|S`,
    vars: [`∀k`],
    hyps: [
      `${j} S k`
    ],
    con: `${i} S k`
  }))
  const rule2_h1: Rule2_H1 = {
    i, j, s1: s1.key,
    key,
    str: substr
  }
  rule2_h1s.set(ix, rule2_h1)
  if (_links_to_general_rules) {
    makeLinks([rule2, s1.key], "rule 2 (hyp 1)", [key])
  } else {
    makeLinks([s1.key], "rule 2 (hyp 1)", [key])
  }
  return rule2_h1
}

type Rule2_H2 = {
  case: 'rule2_h2',
  i: number, j: number, k: number, // indices
  rule2_h1: Key, s2: Key, // hyps
  key: Key, // this
  str: String
}

function learn_Rule2_H2(rule2_h1: Rule2_H1, s2: S_Parse): S_Parse | undefined {
  const i = rule2_h1.i
  const j = rule2_h1.j
  const { j: k } = get_s_parse_bounds(s2)
  const ix = index2(i, k)

  // ignore if already learned this instance
  if (has_such_that(s_parses, ix, parse =>
    // parse.case === 'rule2_h2' &&
    // parse.rule2_h1 === rule2_h1.key &&
    // parse.s2 === s2.key
    true
  )) return undefined

  const substr = `${rule2_h1.str}${s2.str}`
  const key = makeNode(renderRule({
    label: `rule 2 : ${substr}`,
    con: `${i} S ${k}`
  }))
  const parse: S_Parse = {
    case: 'rule2_h2',
    i, j, k,
    rule2_h1: rule2_h1.key, s2: s2.key,
    key,
    str: substr
  }
  add_s_parse(parse)
  makeLinks([rule2_h1.key, s2.key], "rule 2 (hyp 2)", [key])
  return parse
}

type Rule3_H1 = {
  case: 'rule3_h1',
  i: number, j: number, // indices
  a: Key, // hyps
  key: Key, // this
  str: String
}

function learn_Rule3_H1(a: Char_Parse): S_Parse {
  const i = a.i
  const j = a.j
  const ix = index2(i, j)

  if (has_such_that(s_parses, ix, parse =>
    // parse.case === 'rule3_h1' &&
    // parse.a === a.key
    true
  )) return undefined

  const substr = `a`
  const key = makeNode(renderRule({
    label: `rule 3 : ${substr}`,
    con: `${i} S ${j}`
  }))
  const parse: S_Parse = {
    case: 'rule3_h1',
    i, j,
    a: a.key,
    key,
    str: substr
  }
  add_s_parse(parse)
  if (_links_to_general_rules) {
    makeLinks([rule3, a.key], "rule 3", [key])
  } else {
    makeLinks([a.key], "rule 3", [key])
  }
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

function loop(): Boolean {
  let progress: Boolean = false

  function update_progress<a>(a: a | undefined) {
    progress = progress || a !== undefined
  }

  // Make copies of current values of these maps, so that as they are updated it
  // doesn't affect the current iterations.

  const sParses_old = new Map(s_parses)
  const rule1_h1s_old = new Map(rule1_h1s)
  const rule1_h2s_old = new Map(rule1_h2s)
  const rule2_h1s_old = new Map(rule2_h1s)

  // Apply each rule 1 (inst hyp 1).

  rule1_h1s_old.forEach(rule1_h1 => {
    // Wants hypothesis: ∃k . j S k
    sParses_old.forEach(s => {
      if (get_s_parse_bounds(s).i === rule1_h1.j) {
        update_progress(learn_Rule1_H2(rule1_h1, s))
      }
    })
  })

  // Apply each rule 1 (inst hyps 1, 2).

  rule1_h2s_old.forEach(rule1_h2 => {
    // Wants hypothesis: ∃l . k ) l
    const char = char_parses.at(rule1_h2.k)
    if (char !== undefined && char.c === ')') {
      update_progress(learn_Rule1_H3(rule1_h2, char))
    }
  })

  if (true) {
    // Apply rule 2.

    sParses_old.forEach(s1 => {
      // Wants hypothesis: ∃i ∃j. i S j
      update_progress(learn_Rule2_H1(s1))
    })

    // Apply each rule 2 (inst hyp 1).

    rule2_h1s_old.forEach(rule2_h1 => {
      // Wants hypothesis: ∃k . j S k
      sParses_old.forEach(s2 => {
        if (get_s_parse_bounds(s2).i == rule2_h1.j) {
          update_progress(learn_Rule2_H2(rule2_h1, s2))
        }
      })
    })
  }

  return progress
}

// do fixpoint
let loop_i = 0
const loop_i_max = 16
while (loop()) {
  loop_i++
  if (loop_i > loop_i_max) {
    console.log("[loop] too many loops")
    break
  }
}

export default function generateDate() {
  // This module does things at the top level, since it was more convenient to
  // define types next to functions.
}
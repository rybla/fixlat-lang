import { Key, makeApplication, makeAxiom, makeInstance, makeLink, makeStep } from './rule'

type ParseResult = { key: Key, ix: number }

// ambiguous grammar
function traceParse_S_v1(str: String) {
  /* ===========================================================================
    Axioms

    Rules:
      rule1: S -> (S)
      rule2: S -> SS
      rule3: S -> a
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

  const charAxioms: Key[] = []
  for (let i = 0; i < str.length; i++) {
    const c = str.charAt(i)
    charAxioms.push(makeAxiom({ con: `${i} ${c} ${i + 1}` }))
  }

  /* ===========================================================================
    Parsers
  =========================================================================== */

  function parse_char(c: String, wanter: Key, ix: number): ParseResult | undefined {
    if (str.charAt(ix) == c) {
      const key = charAxioms.at(ix)
      makeLink(wanter, "wants", key, 'traced')
      return { key, ix: ix + 1 }
    }
    return undefined
  }

  function parse_S_rule1(wanter: Key, ix: number): ParseResult | undefined {
    const rule1_inst1 = makeStep(rule1, "insts", {
      label: `rule1 [i:=${ix}]`,
      vars: [`∀j`, `∀k`, `∀l`],
      hyps: [`${ix} ( j`, `j S k`, `k ) l`],
      con: `${ix} S l`
    }, 'silent')

    makeLink(wanter, "wants", rule1_inst1, 'traced')

    const lpar = parse_char('(', rule1_inst1, ix)
    if (lpar === undefined) return undefined

    const s = parse_S(rule1_inst1, lpar.ix)
    if (s === undefined) return undefined

    const rpar = parse_char(')', rule1_inst1, s.ix)
    if (rpar === undefined) return undefined

    return { key: rule1_inst1, ix: rpar.ix }
  }

  function parse_S_rule2(wanter: Key, ix: number): ParseResult | undefined {
    const rule2_inst = makeStep(rule2, "insts", {
      label: `rule2 [i:=${ix}]`,
      vars: [`∀j`, `∀k`],
      hyps: [
        `${ix} S j`,
        `j S k`
      ],
      con: `${ix} S k`
    }, 'silent')

    makeLink(wanter, "wants", rule2_inst, 'traced')

    const s1 = parse_S_non2(rule2_inst, ix)
    if (s1 === undefined) return undefined

    const s2 = parse_S(rule2_inst, s1.ix)
    if (s2 === undefined) return undefined

    return { key: rule2_inst, ix: s2.ix }
  }

  function parse_S_rule3(wanter: Key, ix: number): ParseResult | undefined {
    const rule3_inst = makeStep(rule3, "insts", {
      label: `rule3 [i:=${ix}]`,
      vars: [`∀j`],
      hyps: [`${ix} a j`],
      con: `${ix} S j`
    }, 'silent')

    makeLink(wanter, "wants", rule3_inst, 'traced')

    const a = parse_char('a', rule3_inst, ix)
    if (a === undefined) return undefined

    return { key: rule3_inst, ix: a.ix }
  }

  let cache_S_rule1: Map<number, ParseResult[]> = new Map()
  function remember_S_rule1(ix: number, s: ParseResult): ParseResult {
    let ss: ParseResult[] = undefined
    if (ss = cache_S_rule1.get(ix)) ss.push(s)
    else cache_S_rule1.set(ix, [s])
    return s
  }

  let cache_S_rule2: Map<number, ParseResult[]> = new Map()
  function remember_S_rule2(ix: number, s: ParseResult): ParseResult {
    let ss: ParseResult[] = undefined
    if (ss = cache_S_rule2.get(ix)) ss.push(s)
    else cache_S_rule2.set(ix, [s])
    return s
  }

  let cache_S_rule3: Map<number, ParseResult[]> = new Map()
  function remember_S_rule3(ix: number, s: ParseResult): ParseResult {
    let ss: ParseResult[] = undefined
    if (ss = cache_S_rule3.get(ix)) ss.push(s)
    else cache_S_rule3.set(ix, [s])
    return s
  }

  function parse_S_non2(wanter: Key, ix: number): ParseResult | undefined {
    const s_rule1 = parse_S_rule1(wanter, ix)
    if (s_rule1 !== undefined) return remember_S_rule1(ix, s_rule1)

    const s_rule3 = parse_S_rule3(wanter, ix)
    if (s_rule3 !== undefined) return remember_S_rule3(ix, s_rule3)
  }


  function parse_S(wanter: Key, ix: number): ParseResult | undefined {
    const s_rule2 = parse_S_rule2(wanter, ix)
    if (s_rule2 !== undefined) return remember_S_rule2(ix, s_rule2)

    return parse_S_non2(wanter, ix)
  }

  parse_S(makeAxiom({
    vars: ["∀i", "∀j"],
    con: "i S j"
  }), 0)
}

// unambiguous grammar
function traceParse_S_v2(str: String) {
  /* ===========================================================================
    Axioms

    Rules:
      rule1: S -> (S)
      rule2: S -> #SS
      rule3: S -> a
  =========================================================================== */

  const rule1 = makeAxiom({
    label: `rule1`,
    vars: [`∀i`, `∀j`, `∀k`, `∀l`],
    hyps: [
      `i ( j`,
      `j Sp k`,
      `k ) l`
    ],
    con: `i Sp l`
  })

  const rule2 = makeAxiom({
    label: `rule2`,
    vars: [`∀i`, `∀j`, `∀k`, `∀l`],
    hyps: [
      `i # j`,
      `j Sp k`,
      `k Sp l`
    ],
    con: `i Sp l`
  })

  const rule3 = makeAxiom({
    label: `rule3`,
    vars: [`∀i`, `∀j`],
    hyps: [`i a j`],
    con: `i Sp j`
  })

  const charAxioms: Key[] = []
  for (let i = 0; i < str.length; i++) {
    const c = str.charAt(i)
    charAxioms.push(makeAxiom({ con: `${i} ${c} ${i + 1}` }))
  }

  /* ===========================================================================
    Parsers
  =========================================================================== */

  function parse_char(c: String, wanter: Key, ix: number): ParseResult | undefined {
    if (str.charAt(ix) == c) {
      const key = charAxioms.at(ix)
      makeLink(wanter, "wants", key, 'traced')
      return { key, ix: ix + 1 }
    }
    return undefined
  }

  function parse_Sp_rule1(wanter: Key, ix: number): ParseResult | undefined {
    const rule1_inst1 = makeStep(rule1, "insts", {
      label: `rule1 [i:=${ix}]`,
      vars: [`∀j`, `∀k`, `∀l`],
      hyps: [
        `${ix} ( j`,
        `j Sp k`,
        `k ) l`
      ],
      con: `${ix} Sp l`
    }, 'silent')

    makeLink(wanter, "wants", rule1_inst1, 'traced')

    const lpar = parse_char('(', rule1_inst1, ix)
    if (lpar === undefined) return undefined

    const s = parse_Sp(rule1_inst1, lpar.ix)
    if (s === undefined) return undefined

    const rpar = parse_char(')', rule1_inst1, s.ix)
    if (rpar === undefined) return undefined

    return { key: rule1_inst1, ix: rpar.ix }
  }

  function parse_Sp_rule2(wanter: Key, ix: number): ParseResult | undefined {
    const rule2_inst = makeStep(rule2, "insts", {
      label: `rule2 [i:=${ix}]`,
      vars: [`∀i`, `∀j`, `∀k`, `∀l`],
      hyps: [
        `${ix} # j`,
        `j Sp k`,
        `k Sp l`
      ],
      con: `${ix} Sp l`
    }, 'silent')

    makeLink(wanter, "wants", rule2_inst, 'traced')

    const hash = parse_char("#", rule2_inst, ix)
    if (hash === undefined) return undefined

    const s1 = parse_Sp(rule2_inst, hash.ix)
    if (s1 === undefined) return undefined

    const s2 = parse_Sp(rule2_inst, s1.ix)
    if (s2 === undefined) return undefined

    return { key: rule2_inst, ix: s2.ix }
  }

  function parse_Sp_rule3(wanter: Key, ix: number): ParseResult | undefined {
    const rule3_inst = makeStep(rule3, "insts", {
      label: `rule3 [i:=${ix}]`,
      vars: [`∀j`],
      hyps: [`${ix} a j`],
      con: `${ix} Sp j`
    }, 'silent')

    makeLink(wanter, "wants", rule3_inst, 'traced')

    const a = parse_char('a', rule3_inst, ix)
    if (a === undefined) return undefined

    return { key: rule3_inst, ix: a.ix }
  }

  function parse_Sp(wanter: Key, ix: number): ParseResult | undefined {
    const sp_rule1 = parse_Sp_rule1(wanter, ix)
    if (sp_rule1 !== undefined) return sp_rule1

    const sp_rule2 = parse_Sp_rule2(wanter, ix)
    if (sp_rule2 !== undefined) return sp_rule2

    const sp_rule3 = parse_Sp_rule3(wanter, ix)
    if (sp_rule3 !== undefined) return sp_rule3
  }

  parse_Sp(makeAxiom({
    vars: ["∀i", "∀j"],
    con: "i S j"
  }), 0)
}


export default function generateData() {
  // // S
  // // traceParse_S_v1(`()`)
  // traceParse_S_v1(`aaa`)
  // // traceParse_S_v1(`(())(()())`)
  // // traceParse_S_v1(`(a)`)
  // // traceParse_S_v1(`(a)(a)`)

  // Sp
  traceParse_S_v2(`#aa`)
  // traceParse_S_v2(`#(a)(a)`)
}
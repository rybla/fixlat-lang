import { Key, makeApplication, makeAxiom, makeInstance } from './rule'

/*

Rules:

  rule1: S -> (S)
  rule2: S -> SS
  rule3: S -> a

*/

// const epsilon = makeAxiom({
//   label: `epsilon`,
//   vars: [`∀i`],
//   con: `i ϵ i`
// })

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

function makeInputString(gas: number): String {
  let str = ``
  while (gas > 0) {
    str = `(${str})`
  }
  return str
}

type ParseResult = { key: Key, ix: number } | undefined

function traceParse(str: String) {
  // pose axioms for string characters
  const charAxioms: Key[] = []
  for (let i = 0; i < str.length; i++) {
    const c = str.charAt(i)
    charAxioms.push(makeAxiom({ con: `${i} ${c} ${i + 1}` }))
  }


  // function parseEpsilon(i: number): ParseResult {
  //   return {
  //     key: makeApplication({
  //       con: `${i} ϵ ${i}`
  //     }, [epsilon]),
  //     ix: i
  //   }
  // }

  function parse_a(i: number): ParseResult {
    if (str.charAt(i) == `a`) return { key: charAxioms.at(i), ix: i + 1 }
    return undefined
  }

  function parse_lpar(i: number): ParseResult {
    if (str.charAt(i) == `(`) return { key: charAxioms.at(i), ix: i + 1 }
    return undefined
  }

  function parse_rpar(i: number): ParseResult {
    if (str.charAt(i) == `)`) return { key: charAxioms.at(i), ix: i + 1 }
    return undefined
  }

  function parse_S_rule1(ix: number): ParseResult {
    const rule1_inst1 = makeInstance({
      label: `rule1`,
      vars: [`∀j`, `∀k`, `∀l`],
      hyps: [`${ix} ( j`, `j S k`, `k ) l`],
      con: `${ix} S l`
    }, rule1, [])

    const lpar = parse_lpar(ix)
    if (lpar === undefined) return undefined

    const rule1_inst2 = makeInstance({
      label: `rule1`,
      vars: [`∀k`, `∀l`],
      hyps: [`${ix} ( ${lpar.ix}`, `${lpar.ix} S k`, `k ) l`],
      con: `${ix} S l`
    }, rule1_inst1, [lpar.key])

    const s = parse_S(lpar.ix)
    if (s === undefined) return undefined

    const rule1_inst3 = makeInstance({
      label: `rule1`,
      vars: [`∀l`],
      hyps: [`${ix} ( ${lpar.ix}`, `${lpar.ix} S ${s.ix}`, `${s.ix} ) l`],
      con: `${ix} S l`
    }, rule1_inst2, [s.key])

    const rpar = parse_rpar(s.ix)
    if (rpar === undefined) return undefined

    const rule1_inst4 = makeInstance({
      label: `rule1`,
      vars: [],
      hyps: [`${ix} ( ${lpar.ix}`, `${lpar.ix} S ${s.ix}`, `${s.ix} ) ${rpar.ix}`],
      con: `${ix} S ${rpar.ix}`
    }, rule1_inst3, [rpar.key])

    return {
      key: rule1_inst4,
      ix: rpar.ix
    }
  }

  function parse_S_rule2(ix: number): ParseResult {
    const rule2_inst1 = makeInstance({
      label: `rule2`,
      vars: [`∀j`, `∀k`],
      hyps: [
        `${ix} S j`,
        `j S k`
      ],
      con: `${ix} S k`
    }, rule2, [])

    const s1 = parse_S_non2(ix)
    if (s1 === undefined) return undefined

    const rule2_inst2 = makeInstance({
      label: `rule2`,
      vars: [`∀k`],
      hyps: [
        `${ix} S ${s1.ix}`,
        `${s1.ix} S k`
      ],
      con: `${ix} S k`
    }, rule2_inst1, [s1.key])

    const s2 = parse_S_non2(ix)
    if (s2 === undefined) return undefined

    const rule2_inst3 = makeInstance({
      label: `rule2`,
      vars: [],
      hyps: [
        `${s1.ix} S ${s1.ix}`
      ],
      con: `${ix} S ${s2.ix}`
    }, rule2_inst2, [s2.key])

    return {
      key: rule2_inst3,
      ix: s2.ix
    }
  }

  function parse_S_rule3(ix: number): ParseResult {
    // return {
    //   key: makeInstance({
    //     label: `rule3`,
    //     con: `${ix} S ${ix}`
    //   }, rule3, [
    //     makeInstance({
    //       label: `epsilon`,
    //       con: `${ix} ϵ ${ix}`
    //     }, epsilon, [])
    //   ]),
    //   ix: ix
    // }
    const a = parse_a(ix)
    if (a === undefined) return undefined

    const rule3_inst = makeInstance({
      con: `${ix} S ${ix + 1}`
    }, rule3, [a.key])
  }

  function parse_S_non2(ix: number): ParseResult {
    // try rule1
    const s_rule1 = parse_S_rule1(ix)
    if (s_rule1 !== undefined) return s_rule1

    // try rule3
    const s_rule3 = parse_S_rule3(ix)
    if (s_rule3 !== undefined) return s_rule3
  }

  function parse_S(ix: number): ParseResult {
    // try rule2
    const s_rule2 = parse_S_rule2(ix)
    if (s_rule2 !== undefined) return s_rule2

    return parse_S_non2(ix)
  }


  parse_S(0)
}

export default function generateData() {
  // traceParse(`(())(()())`)
  traceParse(`(a)`)
}
import { nodes, links, makeAxiom, makeInstance, makeApplication } from './rule_down'

// compute trace of add
function trace(a0, b0) {
  const zero = makeAxiom({
    vars: ["∀x"],
    con: "x + 0 = x"
  })
  const suc = makeAxiom({
    vars: ["∀x", "∀y", "∀z"],
    hyps: ["x + Sy = Sz"],
    con: "x + y = z"
  })

  // Returns the id of the node `a + b = z` where `∃z`.
  function add(a, b) {
    if (b > 0) {
      let bp = b - 1
      let suc_a_bp = makeInstance(
        {
          vars: [`∃z`],
          hyps: [`${a} + ${bp} = z`],
          con: `${a} + ${b} = Sz`,
        },
        suc,
        [add(a, b - 1)]
      )

      return makeApplication(
        {
          vars: [`∃z`],
          con: `${a} + ${b} = z`
        },
        [suc_a_bp]
      )
    } else {
      return makeInstance(
        {
          con: `${a} + ${b} = ${a}`,
        },
        zero,
        []
      )
    }
  }

  add(a0, b0)
}

export default function generateData() {
  trace(9, 5)

  return { nodes, links }
}


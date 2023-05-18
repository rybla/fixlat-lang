import { makeAxiom, makeLinks, Key, makeNode, renderRule } from './rule_up'

const add_suc: Key = makeAxiom({
  label: "add suc",
  vars: ["∀x", "∀y", "∀z"],
  hyps: ["x + y = z"],
  con: "x + Sy = Sz"
})

function trace_add(x: number, y_: number) {
  const add_zero: Key = makeAxiom({
    label: `add zero`,
    con: `${x} + 0 = ${x}`
  })

  let y = 0
  let z = x

  let prev_key: Key = add_zero

  while (y < y_) {
    const next_key = makeNode(renderRule({
      label: `add suc`,
      con: `${x} + ${y + 1} = ${z + 1}`
    }))
    makeLinks(
      [add_suc, prev_key],
      "apply add suc",
      [next_key]
    )
    y++
    z++
    prev_key = next_key
  }
}

export default function generateData() {
  const x = 3
  const y = 4
  trace_add(x, y)
}
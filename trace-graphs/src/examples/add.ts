import { makeAxiom, makeInstance, makeApplication, makeStep, Key, makeLink } from './rule_down'

const add_zero: Key = makeAxiom({
  label: "add zero",
  vars: ["∀x"],
  con: "x + 0 = x"
})
const add_suc: Key = makeAxiom({
  label: "add suc",
  vars: ["∀x", "∀y", "∀z"],
  hyps: ["x + Sy = Sz"],
  con: "x + y = z"
})

// compute trace_add of add
// Returns the id of the node `a + b = z` where `∃z`.
function trace_add(wanter: Key, a: number, b: number) {

  if (b > 0) {
    const bp = b - 1

    const add_suc_inst = makeStep(add_suc, "insts", {
      label: `add suc [x:=${a}, y:=${b}]`,
      vars: [`∀z`],
      hyps: [`${a} + S${bp} = Sz`],
      con: `${a} + ${bp} = z`
    }, 'silent')

    makeLink(wanter, "wants", add_suc_inst, 'traced')

    trace_add(add_suc_inst, a, bp)
  } else {
    const add_zero_inst = makeStep(add_zero, "insts", {
      label: `add zero [x:=${a}]`,
      con: `${a} + 0 = ${a}`
    }, 'silent')

    makeLink(wanter, "wants", add_zero_inst, 'traced')
  }
}

export default function generateData() {
  const a = 9
  const b = 8
  trace_add(makeAxiom({
    vars: ["∃z"],
    con: `${a} + ${b} = z`
  }), a, b)
}


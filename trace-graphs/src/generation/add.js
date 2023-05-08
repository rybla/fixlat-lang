var freshDerivId = -1

var nodes = []
var links = []

// returns new deriv's id
function makeAxiom(label) {
  freshDerivId++

  let node = {
    label: label,
    id: freshDerivId
  }
  nodes.push(node)

  return node.id
}

function makeDeriv(label, targetIds) {
  freshDerivId++

  let node = {
    label: label,
    id: freshDerivId
  }
  nodes.push(node)

  targetIds.forEach(targetId => {
    links.push({
      source: node.id,
      target: targetId,
      label: "wants"
    })
  })

  return node.id
}

// compute trace of add
function trace(a, b) {
  const add_zero = makeAxiom("∀ x . (0 + x) = x")
  const add_suc = makeAxiom(["∀ x y z . (x + Sy = Sz)", "⊢ (x + y = z)"])

  if (b == 0) {
    makeDeriv(`${a} + 0 = ${a}`, [add_zero])
  } else {
    let prev_id = makeDeriv(`${a} + S${b - 1} = Sz`, [add_suc])
    while (b > 0) {
      b--
      prev_id = makeDeriv(`${a} + ${b} = z`, [add_suc, prev_id])
    }
    makeDeriv(`${a} + 0 = ${a}`, [prev_id, add_zero])
  }

}

export default function gen() {
  trace(3, 5)

  return {
    nodes: nodes,
    links: links
  }
}


var freshDerivId = -1

var nodes = []
var links = []

// returns new deriv's id
function makeAxiom(label) {
  let node = {
    label: label,
    id: ++freshDerivId
  }
  nodes.push(node)

  return node.id
}

function makeInstance(label, generalId) {
  let node = {
    label: label,
    id: ++freshDerivId
  }
  nodes.push(node)

  links.push({
    source: generalId,
    target: node.id,
    label: ["generalizes"]
  })

  return node.id
}

function makeApplication(label, wantIds) {
  let node = {
    label: label,
    id: ++freshDerivId
  }
  nodes.push(node)

  wantIds.forEach(wantId => {
    links.push({
      source: node.id,
      target: wantId,
      label: "wants"
    })
  })

  return node.id
}

// compute trace of add
function trace(a, b) {
  const zero = makeAxiom(["∀ x", "0 + x = x"])
  const suc = makeAxiom(["∀ x y z", "x + Sy = Sz", "⊢ x + y = z"])

  if (b == 0) {
    makeInstance(`${a} + 0 = ${a}`, zero)
    // makeApplication(`${a} + 0 = ${a}`, [zero])
  } else {
    let pred = makeApplication(["∃ z", `${a} + S${b-1} = Sz`], [suc])
    while (b > 0) {
      b--
      pred = makeApplication(["∃ z", `${a} + ${b} = z`], [suc, pred])
    }
    let base = makeInstance(`${a} + 0 = ${a}`, zero)
    makeApplication(`${a} + ${b} = ${a + b}`, [base, pred])
  }

}

export default function gen() {
  trace(3, 5)

  return {
    nodes: nodes,
    links: links
  }
}


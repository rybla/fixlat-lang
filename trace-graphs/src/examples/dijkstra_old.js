import { nodes, links, makeAxiom, makeApplication, makeInstance } from "./rule_down"

const vertices = []
const edges = new Map() // edge => {source, target, weight}
const outEdges = new Map() // vertex ==> edge[]
const inEdges = new Map() // vertex ==> edge[]

function makeVertext(v) {
  vertices.push(v)
  outEdges.set(v, [])
  inEdges.set(v, [])
  console.log(`[new vertext] ${v}`)
  return v
}

function setPush(m, k, v) {
  let vs = m.get(k)
  if (vs === undefined) m.set(k, [v])
  else vs.push(v)
}

function makeEdge(source, target, weight) {
  const key = makeAxiom({
    con: `${source} -> ${target} <= ${weight}`
  })
  edges.set(key, { source, target, weight })

  setPush(outEdges, source, key)
  setPush(inEdges, target, key)

  console.log(`[new edge] ${key}: ${source} -> ${target} <= ${weight}`)

  return key
}

function logGraphviz() {
  let str = ""

  edges.forEach(({ source, target, weight }) => {
    str += `${source} -> ${target} [label=${weight}]\n`
  })

  console.log(`digraph G {\n${str}\n}`)
}

const self = makeAxiom({
  vars: ["∀n"],
  con: "n -->> n <= 0"
})

const step = makeAxiom({
  vars: ["∀n1", "∀n2", "∀n3"],
  hyps: [
    "n1 -> n2 <= w1",
    "n2 -->> n3 <= w2",
  ],
  con: "n1 -->> n3 <= w1 + w2"
})

function makeWeight() {
  return Math.floor(1 + 4 * Math.random())
}

function generateGraph(n) {
  const graph = {
    vertices: [],
    edges: []
  }

  for (let i = 0; i < n; i++) {
    makeVertext(i)
    graph.vertices.push(i)
  }

  const n_outEdges = 3 // for each vertex
  for (let v0 = 0; v0 < vertices.length; v0++) {
    for (let i = 0; i < 3; i++) {
      let vertices_ = vertices.filter(v1 => {
        outEdges.get(v0).forEach(e => {
          const tgt = edges.get(e).target
          return !(tgt == v0 || tgt == v1)
        })
        return true
      })
      let v1 = vertices_[Math.floor(Math.random() * vertices_.length)]
      const w = makeWeight()
      makeEdge(v0, v1, w)
      graph.edges.push({ source: v0, target: v1, weight: w })
    }
  }

  logGraph(graph)
}

function loadGraph(graph) {
  graph.vertices.forEach(v => makeVertext(v))
  graph.edges.forEach(({ source, target, weight }) => {
    makeEdge(source, target, weight)
  })
}

function logGraph(graph) {
  let verticesStr = ""
  graph.vertices.forEach(v => {
    verticesStr += `${v}, `
  })

  let edgesStr = ""
  graph.edges.forEach(({ source, target, weight }) => {
    edgesStr += `{source: ${source}, target: ${target}, weight: ${weight}}, `
  })

  let graphStr = `{vertices: ${verticesStr}, edges: ${edgesStr}}`
  console.log(graphStr)
}

function traceShortestPathsFrom(startVertex) {
  // `nextEdges` is sorted by lowest weight first.
  const nextEdges = [] // edge[]
  const visitedVertices = [] // vertex[]
  const shortestDerivs = new Map() // target ==> deriv
  const derivWeights = new Map() // derive ==> weight

  // initialize `shortestDerivs` and `derivWeights`
  // const startDeriv = makeInstance({
  //   con: `${startVertex} -->> ${startVertex} <= 0`
  // }, self, [])
  const startDeriv = makeApplication({
    con: `${startVertex} -->> ${startVertex} <= 0`
  }, [self])
  edges.set(startDeriv, { source: startVertex, target: startVertex, weight: 0 })
  shortestDerivs.set(startVertex, startDeriv)
  derivWeights.set(startDeriv, 0)

  function visitVertex(vertex) {
    // console.log(`[visitVertex]\n  - visitedVertices: ${visitedVertices}\n  - nextEdges: ${nextEdges.map(e => {
    //   let edge = edges.get(e)
    //   return `${edge.source} -> ${edge.target} <= ${edge.weight}`
    // })}\n  vertex: ${vertex}`)

    // Add all edges that start at `target`, which themselves have an unvisited
    // target, to `nextEdges`.
    outEdges.get(vertex).forEach(nextEdge => {
      nextEdges.push(nextEdge)
    })

    // // Return if no more edges to visit.
    // if (nextEdges.length == 0) return

    // Re-sort `nextEdges`.
    nextEdges.sort((edge1, edge2) => edges.get(edge1).weight - edges.get(edge2).weight)

    // Shift the next edge out of `nextEdges` that doesn't have a visited
    // target.
    var nextEdge = undefined
    while (nextEdges.length > 0) {
      nextEdge = nextEdges.shift()
      if (!visitedVertices.includes(edges.get(nextEdge).target)) {
        // Visit `nextEdge` since it's target hasn't been visited
        visit(nextEdge)
        return
      }
    }

    // There was no next edge to an unvisited target.
    return
  }

  function visit(edge) {
    // console.log(`visit: ${edge}`)
    const { source, target, weight: edgeWeight } = edges.get(edge)
    const deriv = shortestDerivs.get(source)
    const derivWeight = derivWeights.get(deriv)
    visitedVertices.push(target)

    // console.log(`[visit] edge: ${source} -> ${target} <= ${edgeWeight}`)

    // Make new deriv using step rule, using `deriv` and `edge` to satisfy
    // hypotheses.
    // const stepInstance = makeInstance({
    //   hyps: [
    //     `${startVertex} -->> ${source} <= ${derivWeight}`,
    //     `${source} -> ${target} <= ${edgeWeight}`
    //   ],
    //   con: `${startVertex} -->> ${target} <= ${derivWeight + edgeWeight}`
    // }, step, [deriv, edge])
    const stepInstance = makeApplication({
      hyps: [
        `${startVertex} -->> ${source} <= ${derivWeight}`,
        `${source} -> ${target} <= ${edgeWeight}`
      ],
      con: `${startVertex} -->> ${target} <= ${derivWeight + edgeWeight}`
    }, [step, deriv, edge])

    const nextDeriv = makeApplication({
      con: `${startVertex} -->> ${target} <= ${derivWeight + edgeWeight}`
    }, [stepInstance])
    shortestDerivs.set(target, nextDeriv)
    derivWeights.set(nextDeriv, derivWeight + edgeWeight)

    visitVertex(target)
  }

  visitVertex(startVertex)
}

export default function generateData() {
  // const n = 4
  // generateGraph(4)

  // loadGraph({
  //   vertices: [0, 1, 2, 3,],
  //   edges: [{ source: 0, target: 2, weight: 4 }, { source: 0, target: 0, weight: 4 }, { source: 0, target: 0, weight: 4 }, { source: 1, target: 0, weight: 2 }, { source: 1, target: 0, weight: 3 }, { source: 1, target: 0, weight: 4 }, { source: 2, target: 2, weight: 2 }, { source: 2, target: 3, weight: 3 }, { source: 2, target: 0, weight: 1 }, { source: 3, target: 2, weight: 2 }, { source: 3, target: 0, weight: 1 }, { source: 3, target: 3, weight: 2 },]
  // })

  loadGraph({
    vertices: [0, 1, 2, 3],
    edges: [
      { source: 0, target: 1, weight: 2 },
      { source: 1, target: 3, weight: 1 },
      { source: 0, target: 2, weight: 1 },
      { source: 2, target: 3, weight: 4 },
    ]
  })

  logGraphviz()

  traceShortestPathsFrom(0)
}
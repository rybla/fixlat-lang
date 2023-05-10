import { Key, makeAxiom, makeLink, makeStep } from "./rule"

type VertexId = number
type EdgeId = Key
type Weight = number
type Edge = { source: VertexId, target: VertexId, weight: Weight }
type Graph = { vertices: VertexId[], edges: Edge[] }

const vertices: VertexId[] = []
const edges: Map<EdgeId, Edge> = new Map() // edge => {source, target, weight}
const outEdges: Map<VertexId, EdgeId[]> = new Map() // vertex ==> edge[]
const inEdges: Map<VertexId, EdgeId[]> = new Map() // vertex ==> edge[]

function makeVertext(v: VertexId): VertexId {
  vertices.push(v)
  outEdges.set(v, [])
  inEdges.set(v, [])
  console.log(`[new vertext] ${v}`)
  return v
}

function setPush<k, v>(m: Map<k, v[]>, k: k, v: v) {
  let vs = m.get(k)
  if (vs === undefined) m.set(k, [v])
  else vs.push(v)
}

function makeEdge(source: VertexId, target: VertexId, weight: Weight) {
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
  label: "self",
  vars: ["∀n"],
  con: "n -->> n <= 0"
})

const step = makeAxiom({
  label: "step",
  vars: ["∀n1", "∀n2", "∀n3"],
  hyps: [
    "n1 -->> n2 <= w1",
    "n2 -> n3 <= w2",
  ],
  con: "n1 -->> n3 <= w1 + w2"
})

function makeWeight() {
  return Math.floor(1 + 4 * Math.random())
}

function generateGraph(n: number) {
  const graph: Graph = {
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

function loadGraph(graph: Graph) {
  graph.vertices.forEach(v => makeVertext(v))
  graph.edges.forEach(({ source, target, weight }) => {
    makeEdge(source, target, weight)
  })
}

function logGraph(graph: Graph) {
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

function traceShortestPathsFrom(startVertex: VertexId) {

  /* ===========================================================================
  Initialize state
  =========================================================================== */

  // `nextEdges` is sorted by lowest weight first.
  const nextEdges: EdgeId[] = [] // edge[]
  const visitedVertices: VertexId[] = [] // vertex[]
  const shortestDerivs: Map<VertexId, Key> = new Map() // target ==> deriv
  const derivWeights: Map<Key, Weight> = new Map() // derive ==> weight

  /* ===========================================================================
  Starting derivation
  =========================================================================== */

  const startDeriv: Key = makeStep(
    self,
    "insts",
    { con: `${startVertex} -->> ${startVertex} <= 0` },
    'silent'
  )
  edges.set(startDeriv, { source: startVertex, target: startVertex, weight: 0 })
  shortestDerivs.set(startVertex, startDeriv)
  derivWeights.set(startDeriv, 0)

  /* ===========================================================================
  Visiting a vertex
  =========================================================================== */

  function visitVertex(vertex: VertexId) {
    // Add all edges that start at `target`, which themselves have an unvisited
    // target, to `nextEdges`.
    outEdges.get(vertex).forEach(nextEdge => {
      nextEdges.push(nextEdge)
    })

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
  }

  function visit(edge: EdgeId) {
    const { source, target, weight: edgeWeight } = edges.get(edge)
    const deriv = shortestDerivs.get(source)
    const derivWeight = derivWeights.get(deriv)
    visitedVertices.push(target)

    // instantiate the step rule
    const step_inst = makeStep(step, "insts", {
      label: `step [n1:=${startVertex}, n2:=${source}, n3:=${target}]`,
      hyps: [
        `${startVertex} -->> ${source} <= ${derivWeight}`,
        `${source} -> ${target} <= ${edgeWeight}`,
      ],
      con: `${startVertex} -->> ${target} <= ${derivWeight + edgeWeight}`
    }, 'silent')

    shortestDerivs.set(target, step_inst)
    derivWeights.set(step_inst, derivWeight + edgeWeight)

    makeLink(step_inst, "wants", deriv, 'traced')
    makeLink(step_inst, "wants", edge, 'traced')

    // instantiate 
    visitVertex(target)
  }

  visitVertex(0)
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
import { Key, makeNode, makeLink, makeLinks, renderRule } from "./rule_up"

type VertexId = number
type Weight = number
type Edge = { case: 'edge', v1: VertexId, v2: VertexId, w: Weight, key: Key }
type Graph = { vertices: VertexId[], edges: Edge[] }

const vertices: VertexId[] = []
const edges: Map<Key, Edge> = new Map() // edge => {v1, v2, w}
const outEdges: Map<VertexId, Key[]> = new Map() // vertex ==> edge[]
const inEdges: Map<VertexId, Key[]> = new Map() // vertex ==> edge[]

function renderEdge(edge: { v1: any, v2: any, w: any }): string {
  return `${edge.v1} -> ${edge.v2} <= ${edge.w}`
}
function renderPath(path: { v1: any, v2: any, w: any }): string {
  return `${path.v1} -->> ${path.v2} <= ${path.w}`
}

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

function makeEdge(v1: VertexId, v2: VertexId, w: Weight) {
  const key = makeNode(renderRule({
    con: renderEdge({ v1, v2, w })
  }))
  edges.set(key, { case: 'edge', v1, v2, w, key })

  setPush(outEdges, v1, key)
  setPush(inEdges, v2, key)

  console.log(`[new edge] ${key}: ${renderEdge({ v1, v2, w })}`)

  return key
}

function logGraphviz() {
  let str = ""

  edges.forEach(({ v1, v2, w }) => {
    str += `${v1} -> ${v2} [label=${w}]\n`
  })

  console.log(`digraph G {\n${str}\n}`)
}

const makeSelf = (v: VertexId) => makeNode(renderRule({
  label: "self",
  vars: [],
  con: `${v} -->> ${v} <= 0`
}))

const step = makeNode(renderRule({
  label: "step",
  vars: ["∀v1", "∀v2", "∀v3"],
  hyps: [
    "v1 -->> v2 <= w1",
    "v2 -> v3 <= w2",
  ],
  con: "v1 -->> v3 <= w1 + w2"
}))

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
  for (let v1 = 0; v1 < vertices.length; v1++) {
    for (let i = 0; i < n_outEdges; i++) {
      let vertices_ = vertices.filter(v2 => {
        outEdges.get(v1).forEach(e => {
          const tgt = edges.get(e).v2
          return !(tgt == v1 || tgt == v2)
        })
        return true
      })
      let v2 = vertices_[Math.floor(Math.random() * vertices_.length)]
      const w = makeWeight()
      const key = makeEdge(v1, v2, w)
      graph.edges.push({ case: 'edge', v1, v2, w, key })
    }
  }

  logGraph(graph)
}

function loadGraph(
  vertices: VertexId[],
  edges: { v1: VertexId, v2: VertexId, w: Weight }[]
) {
  vertices.forEach(v =>
    makeVertext(v)
  )
  edges.forEach(({ v1, v2, w }) => {
    makeEdge(v1, v2, w)
  })
}

function logGraph(graph: Graph) {
  let verticesStr = ""
  graph.vertices.forEach(v => {
    verticesStr += `${v}, `
  })

  let edgesStr = ""
  graph.edges.forEach(({ v1, v2, w }) => {
    edgesStr += `{v1: ${v1}, v2: ${v2}, w: ${w}}, `
  })

  let graphStr = `{vertices: ${verticesStr}, edges: ${edgesStr}}`
  console.log(graphStr)
}

// step rule with first hypothesis satisfied
type Step1 = {
  case: 'step1',
  v1: VertexId,
  v2: VertexId,
  w1: Weight,
  key: Key
}

type Path = {
  case: 'path',
  v1: VertexId,
  v2: VertexId,
  w: Weight,
  key: Key
}

type Rule = Step1 | Path | Edge

function traceShortestPathsFrom(v0: VertexId) {

  /* ===========================================================================
  Starting rules:
    - rules for edges: v1 -> v2 <= w (already added when loading graph)
    - self rule for v0: v0 -->> v0 <= 0
  =========================================================================== */

  let rules: Rule[] = []
  edges.forEach(edge => rules.push(edge))
  const self_v0_path: Path = { case: 'path', v1: v0, v2: v0, w: 0, key: makeSelf(v0) }
  rules.push(self_v0_path)

  // lightest_paths: v2 => lightest path to v2
  const lightest_paths: Map<VertexId, Path> = new Map()
  lightest_paths.set(v0, self_v0_path)

  /* ===========================================================================
  loop:
    - Pop lightest edge from frontier_edges
      - If no frontier_edges, then done
    - Apply the step rule to lightest path to the edge's source, yielding a
      step1
    - Apply the step1 rule to the edge, yielding a path
    - Set the lightest path to the edge's target as this path
  =========================================================================== */

  function makeStep1(path: Path): Step1 {
    const v1 = path.v1
    const v2 = path.v2
    const w1 = path.w
    const key = makeNode(renderRule({
      vars: [`∀n3`, `∀w2`],
      hyps: [renderEdge({v1: v2, v2: "n3", w: "w2"})],
      con: renderPath({ v1, v2: "n3", w: `${w1} + w2` })
    }))
    const step1: Step1 = { case: 'step1', v1, v2, w1, key }
    rules.push(step1)

    makeLinks([step, path.key], "step", [key])

    return step1
  }

  function makeStepPath(step1: Step1, edge: Edge): Path {
    const v1 = step1.v1
    const v2 = edge.v2
    const w = step1.w1 + edge.w
    const key = makeNode(renderRule({
      con: renderPath({ v1, v2, w })
    }))
    const path: Path = { case: 'path', v1, v2, w, key }
    rules.push(path)

    makeLinks([step1.key, edge.key], "step1", [key])

    return path
  }

  const frontier_edges: Edge[] = []

  // Add all edges starting from v1 to the frontier edges, and sort
  // frontier_edges by weight
  function add_edges_to_frontier_from(v1: VertexId): void {
    edges.forEach(edge => {
      if (edge.v1 === v1 && !lightest_paths.has(edge.v2)) {
        frontier_edges.push(edge)
      }
    })
    frontier_edges.sort((edge1, edge2) => edge1.w - edge2.w)
  }

  // Initialize frontier_edges with edges starting at v0
  add_edges_to_frontier_from(v0)

  function pop_lightest_frontier_edge(): Edge | undefined {
    return frontier_edges.pop()
  }

  // Returns whether or not progress was made this loop
  function loop() {
    console.log(`[loop] frontier_edges = ${frontier_edges.map(edge => "\n  - " + renderEdge(edge))}`)
    // Pop the lightest frontier edge
    const edge = pop_lightest_frontier_edge()
    if (edge === undefined) {
      // If there are none, then reached fixpoint
      return false
    }
    // Find the lighest path to the edge's source
    const path = lightest_paths.get(edge.v1)
    // Apply the step rule to the path
    const step1 = makeStep1(path)
    // Apply the new step1 rule to the path
    const pathNew = makeStepPath(step1, edge)
    // PathNew is the lightest path the the target of the edge
    lightest_paths.set(edge.v2, pathNew)
    // Add all edges that start at edge.v2 to the frontier
    add_edges_to_frontier_from(edge.v2)
    // Made progress
    return true
  }

  // Do fixpoint of loop
  while (loop()) { }
}

export default function generateData() {
  // const n = 20
  // generateGraph(n)

  // loadGraph({
  //   vertices: [0, 1, 2, 3,],
  //   edges: [{ v1: 0, v2: 2, w: 4 }, { v1: 0, v2: 0, w: 4 }, { v1: 0, v2: 0, w: 4 }, { v1: 1, v2: 0, w: 2 }, { v1: 1, v2: 0, w: 3 }, { v1: 1, v2: 0, w: 4 }, { v1: 2, v2: 2, w: 2 }, { v1: 2, v2: 3, w: 3 }, { v1: 2, v2: 0, w: 1 }, { v1: 3, v2: 2, w: 2 }, { v1: 3, v2: 0, w: 1 }, { v1: 3, v2: 3, w: 2 },]
  // })

  loadGraph(
    [0, 1, 2, 3],
    [
      { v1: 0, v2: 1, w: 2 },
      { v1: 1, v2: 3, w: 1 },
      { v1: 0, v2: 2, w: 1 },
      { v1: 2, v2: 3, w: 4 },
    ]
  )

  logGraphviz()

  traceShortestPathsFrom(0)
}
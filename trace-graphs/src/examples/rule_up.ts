export type Key = number
export type Node = { key: Key, text: String }
export type Link = { from: Key, to: Key, text: String }
export type Network = { nodes: Node[], links: Link[] }

export var network: Network = {
  nodes: [],
  links: []
}

export type Rule = {
  label?: String,
  vars?: String[],
  hyps?: String[],
  con: String
}

export function renderRule(rule: Rule): String {
  let l = 0
  if (rule.label !== undefined) {
    l = Math.max(l, rule.label.length)
  }
  if (rule.vars !== undefined) {
    l = Math.max(l, rule.vars.join(" ").length)
  }
  if (rule.hyps != undefined) {
    rule.hyps.forEach(hyp => l = Math.max(l, hyp.length))
  }
  l = Math.max(l, rule.con.length)

  let hbar = ""
  for (let i = 0; i < l; i++) { hbar += "–" }

  let str = ""
  if (rule.label !== undefined) { str += `[${rule.label}]` + "\n" }
  if (rule.vars !== undefined && rule.vars.length > 0) { str += rule.vars.join(" ") + "\n" }
  if (rule.hyps !== undefined && rule.hyps.length > 0) { str += rule.hyps.join("\n") + "\n" }
  str += hbar + "\n"
  str += rule.con

  // console.log(str)

  return str
}

// Returns new node's key
var nodeIndex = 0
export function makeNode(text: String): Key {
  // console.log(`[makeNode] text = ${text}`)
  let key = nodeIndex++
  network.nodes.push({ text, key })
  return key
}

export function makeLink(from: Key, text: String, to: Key) {
  network.links.push({ from, to, text })
}

export function makeLinks(froms: Key[], text: String, tos: Key[]) {
  froms.forEach(from => {
    tos.forEach(to => {
      makeLink(from, text, to)
    })
  })
}

export function makeAxiom(rule: Rule): Key {
  return makeNode(renderRule(rule))
}
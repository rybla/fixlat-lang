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
function makeNode(text: String): Key {
  // console.log(`[makeNode] text = ${text}`)
  let key = nodeIndex++
  network.nodes.push({ text, key })
  return key
}

var linkIndex = 0
function makeLink(from: Key, to: Key, text: String) {
  // console.log(`[makeLink] from = ${from}; to = ${to}; text = ${text}`)
  network.links.push({ from, to, text: `[${linkIndex}] ${text}` })
  linkIndex++
}

export function makeAxiom(rule: Rule): Key {
  return makeNode(renderRule(rule))
}

export function makeInstance(rule: Rule, generalKey: Key, wantKeys: Key[]): Key {
  let key = makeNode(renderRule(rule))

  makeLink(generalKey, key, "inst")

  // var i = 0
  // wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants #${i++}`))
  wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants`))

  return key
}

export function makeApplication(rule: Rule, wantKeys: Key[]): Key {
  let key = makeNode(renderRule(rule))

  // var i = 0
  // wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants #${i++}`))
  wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants`))

  return key
}
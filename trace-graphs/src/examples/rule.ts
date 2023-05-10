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

let prev_node_key: Key | undefined = undefined

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

type LinkMode = 'silent' | 'traced'
var linkIndex = 0
export function makeLink(from: Key | undefined, text: String, to: Key, mode: LinkMode) {
  if (from === undefined) return
  // console.log(`[makeLink] from = ${from}; to = ${to}; text = ${text}`)
  // network.links.push({ from, to, text: `[${linkIndex}] ${text}` })

  // network.links.push({ from, to, text })

  // // links that traces algorithm
  // if (prev_node_key !== undefined) {
  //   network.links.push({ from: prev_node_key, to, text: `[${linkIndex}]` })
  // }

  switch (mode) {
    case 'silent': {
      network.links.push({ from, to, text: `${text}` })
      break
    }
    case 'traced': {
      network.links.push({ from, to, text: `[${linkIndex}] ${text}` })
      if ((prev_node_key !== undefined && prev_node_key == from) || (prev_node_key == undefined)) {
        // combine input links and trace links (same endpoints), so don't need
        // another link for trace
      } else if (prev_node_key !== undefined) {
        // need separate link for trace
        network.links.push({ from: prev_node_key, to, text: `[${linkIndex}]` })
      }

      prev_node_key = to
      linkIndex++
    }
  }
}

export function makeAxiom(rule: Rule): Key {
  return makeNode(renderRule(rule))
}

export function makeStep(fromKey: Key, text: String, rule: Rule, mode: LinkMode): Key {
  let toKey = makeNode(renderRule(rule))
  makeLink(fromKey, text, toKey, mode)
  return toKey
}

export function makeInstance(rule: Rule, generalKey: Key, wantKeys: Key[]): Key {
  let key = makeNode(renderRule(rule))

  makeLink(generalKey, "insts", key, 'silent')

  // var i = 0
  // wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants #${i++}`))
  wantKeys.forEach(wantKey => makeLink(key, `wants`, wantKey, 'traced'))

  return key
}

export function makeApplication(rule: Rule, wantKeys: Key[]): Key {
  let key = makeNode(renderRule(rule))

  // var i = 0
  // wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants #${i++}`))
  wantKeys.forEach(wantKey => makeLink(key, `wants`, wantKey, 'traced'))

  return key
}
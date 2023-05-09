export var data = {
  nodes: [],
  links: []
}

export function renderRule(rule) {
  let l = 0
  if (rule.hyps != undefined) {
    rule.hyps.forEach(hyp => l = Math.max(l, hyp.length))
  }
  l = Math.max(l, rule.con.length)

  let hbar = ""
  for (let i = 0; i < l; i++) { hbar += "–" }

  let str = ""
  if (rule.vars !== undefined && rule.vars.length > 0) { str += rule.vars.join(" ") + "\n" }
  if (rule.hyps != undefined && rule.hyps.length > 0) { str += rule.hyps.join("\n") + "\n" }
  str += hbar + "\n"
  str += rule.con

  // console.log(str)

  return str
}

// Returns new node's key
var nodeIndex = 0
function makeNode(text) {
  // console.log(`[makeNode] text = ${text}`)
  let key = nodeIndex++
  data.nodes.push({ text, key })
  return key
}

var linkIndex = 0
function makeLink(from, to, text) {
  // console.log(`[makeLink] from = ${from}; to = ${to}; text = ${text}`)
  data.links.push({ from, to, text: `[${linkIndex}] ${text}` })
  linkIndex++
}

export function makeAxiom(rule) {
  return makeNode(renderRule(rule))
}

export function makeInstance(rule, generalKey, wantKeys) {
  let key = makeNode(renderRule(rule))

  makeLink(generalKey, key, "instantiates to")

  // var i = 0
  // wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants #${i++}`))
  wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants`))

  return key
}

export function makeApplication(rule, wantKeys) {
  let key = makeNode(renderRule(rule))

  // var i = 0
  // wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants #${i++}`))
  wantKeys.forEach(wantKey => makeLink(key, wantKey, `wants`))

  return key
}
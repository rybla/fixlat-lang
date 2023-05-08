export default function gen() {
  var labels = []
  for (let i = 0; i < 100; i++) {
    labels.push(`#${i}`)
  }

  var nodes = []

  let i = 0;

  labels.forEach(label => {
    nodes.push({
      "label": label,
      id: i
    })
    i++
  })

  var links = []

  nodes.forEach(sourceNode => {
    let targetNode = nodes[Math.floor(Math.random()*nodes.length)]
    links.push({
      "source": sourceNode.id,
      "target": targetNode.id,
      "label": `${sourceNode.label} -> ${targetNode.label}`
    })
  })

  return ({
    "nodes": nodes,
    "links": links
  })
}
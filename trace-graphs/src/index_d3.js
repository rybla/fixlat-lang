import * as d3 from 'd3'

// import data from './graphs/test1.json'

// import gen from './generation/gen1'
// let data = gen()

import gen from './generation/add'
let data = gen()


// import gen from './generation/gen2'
// let data = gen()

console.log(data)

const width = 1000;
const height = 700;

let zoom = d3.zoom()
  .on('zoom', e => {
    d3.select('svg g')
      .attr('transform', e.transform)
  })

d3.select('svg').call(zoom)

let svg = d3.select('svg g')
  .attr('width', width)
  .attr('height', height)
  .call(zoom)

const markerBoxWidth = 80
const markerBoxHeight = 20
const refX = markerBoxWidth / 2
const refY = markerBoxHeight / 2
const markerWidth = markerBoxWidth / 2
const markerHeight = markerBoxHeight / 2
const arrowPoints = [[0, 5], [0, 15], [10, 10]]

svg.append('defs').append('marker')
  .attr('id', 'arrowhead')
  .attr('viewBox', [0, 0, markerBoxWidth, markerBoxHeight])
  .attr('refX', refX)
  .attr('refY', refY)
  .attr('markerWidth', markerBoxWidth)
  .attr('markerHeight', markerBoxHeight)
  .attr('orient', 'auto-start-reverse')
  .append('path')
  .attr('d', d3.line()(arrowPoints))
  .attr('stroke', 'black')

let links = svg.selectAll('.link')
  .data(data.links)
// .enter()
// .append('line')
// .attr('marker-end', 'url(#arrowhead)')

let linkLines = links.enter().append('line')
  .attr('class', d => d.label ?? "")

let linkLabels = links.enter().append('text')
  .text(d => d.label ?? "")
  .attr('class', d => d.label ?? "")

// .attr('marker-middle', 'url(#arrowhead)')
// .attr('marker-start', 'url(#arrowhead)')

let nodes = svg.selectAll('.node')
  .data(data.nodes)
  .enter()
  .append('g')

// nodes.append('circle').attr('r', 20)
nodes.append('rect')
  .attr('width', 120)
  .attr('height', 80)
  .attr('x', -60)
  .attr('y', -40)
nodes.append('text').text(d => {
  if (d.label instanceof Array && d.label.length > 0) {
    return d.label[0]
  }
  return d.label
})
  .attr('y', -0.5 * 12)
nodes.append('text').text(d => {
  if (d.label instanceof Array && d.label.length > 1) {
    return d.label[1]
  }
  return ""
})
  .attr('y', 0.5 * 12)
nodes.append('text').text(d => {
  if (d.label instanceof Array && d.label.length > 2) {
    return d.label[2]
  }
  return ""
})
  .attr('y', 1.5 * 12)

d3.forceSimulation(data.nodes)
  .force('charge', d3.forceManyBody().strength(-1500))
  .force('center', d3.forceCenter(width / 2, height / 2))
  .force('link', d3.forceLink().distance(150).links(data.links))
  .on('tick', () => {
    linkLines
      .attr('x1', d => d.source.x)
      .attr('y1', d => d.source.y)
      .attr('x2', d => d.target.x)
      .attr('y2', d => d.target.y)

    linkLabels
      .attr('transform', d => {
        let x = (d.source.x + d.target.x) / 2
        let y = (d.source.y + d.target.y) / 2
        const dsty = d.source.y - d.target.y;
        let angle = Math.atan2(dsty, (d.source.x - d.target.x)) * 180 / Math.PI;
        return `translate(${x}, ${y})rotate(${angle + 180})`
      })

    nodes
      .attr('transform', d => `translate(${d.x}, ${d.y})`)
      .attr('dy', d => 5)
  })



/*
setup download
*/

let set_download_button = document.createElement("button")
set_download_button.innerText = "set_download"
set_download_button.onclick = (event) => {
  //get svg element.
  let svg = document.getElementById("svg");
  console.log(svg)

  //get svg source.
  let serializer = new XMLSerializer();
  let source = serializer.serializeToString(svg);

  //add name spaces.
  if (!source.match(/^<svg[^>]+xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)) {
    source = source.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
  }
  if (!source.match(/^<svg[^>]+"http\:\/\/www\.w3\.org\/1999\/xlink"/)) {
    source = source.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
  }

  //add xml declaration
  source = '<?xml version="1.0" standalone="no"?>\r\n' + source;

  //convert svg source to URI data scheme.
  let url = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(source);

  //set url value to a element's href attribute.
  document.getElementById("link").href = url;
  //you can download svg file by right click menu.
}

document.body.appendChild(set_download_button)

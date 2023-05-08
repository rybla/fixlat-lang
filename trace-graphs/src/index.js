import * as d3 from 'd3'

// import data from './graphs/test1.json'

// import gen from './generation/gen1'
// var data = gen()

import gen from './generation/add'
var data = gen()


// import gen from './generation/gen2'
// var data = gen()

console.log(data)

const width = 600;
const height = 600;

var zoom = d3.zoom()
  .on('zoom', e => {
    d3.select('svg g')
      .attr('transform', e.transform)
  })

d3.select('svg').call(zoom)

var svg = d3.select('svg g')
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

var links = svg.selectAll('.link')
  .data(data.links)
  // .enter()
  // .append('line')
  // .attr('marker-end', 'url(#arrowhead)')

var linkLines = links.enter().append('line')

var linkLabels = links.enter().append('text').text(d => d.label ?? "")

// .attr('marker-middle', 'url(#arrowhead)')
// .attr('marker-start', 'url(#arrowhead)')

var nodes = svg.selectAll('.node')
  .data(data.nodes)
  .enter()
  .append('g')

// nodes.append('circle').attr('r', 20)
nodes.append('rect')
  .attr('width', 80)
  .attr('height', 40)
  .attr('x', -40)
  .attr('y', -20)
nodes.append('text').text(d => {
  if (d.label instanceof Array && d.label.length > 0) {
    return d.label[0]
  }
  return d.label
})
nodes.append('text').text(d => {
  if (d.label instanceof Array && d.label.length > 1) {
    return d.label[1]
  }
  return ""
})
  .attr('y', 12)
nodes.append('text').text(d => {
  if (d.label instanceof Array && d.label.length > 2) {
    return d.label[2]
  }
  return ""
})
  .attr('y', 24)

d3.forceSimulation(data.nodes)
  .force('charge', d3.forceManyBody().strength(-1000))
  .force('center', d3.forceCenter(width / 2, height / 2))
  .force('link', d3.forceLink().distance(200).links(data.links))
  .on('tick', () => {
    linkLines
      .attr('x1', d => d.source.x)
      .attr('y1', d => d.source.y)
      .attr('x2', d => d.target.x)
      .attr('y2', d => d.target.y)

    linkLabels
      .attr('transform', d => {
        let x = (d.source.x + d.target.x)/2
        let y = (d.source.y + d.target.y)/2
        const dsty = d.source.y - d.target.y;
        let angle = Math.atan2(dsty, (d.source.x - d.target.x)) * 180 / Math.PI;
        return `translate(${x}, ${y})rotate(${angle + 180})`
      })

    nodes
      .attr('transform', d => `translate(${d.x}, ${d.y})`)
      .attr('dy', d => 5)
  })

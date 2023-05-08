import * as d3 from 'd3'

// import data from './graphs/test1.json'

import gen from './generation/gen1'
var data = gen()

console.log(data)

const width = 600;
const height = 600;

var svg = d3.select('svg')

var links = svg.selectAll('.link')
  .data(data.links)
  .enter()
  .append('line')
  .attr('marker-end', 'url(#arrowhead)')

var nodes = svg.selectAll('.node')
  .data(data.nodes)
  .enter()
  .append('g')

nodes.append('circle').attr('r', 10)
nodes.append('text').text(d => d.label)

d3.forceSimulation(data.nodes)
  .force('charge', d3.forceManyBody().strength(-10))
  .force('center', d3.forceCenter(width / 2, height / 2))
  .force('link', d3.forceLink().links(data.links))
  .on('tick', () => {
    links
      .attr('x1', d => d.source.x)
      .attr('y1', d => d.source.y)
      .attr('x2', d => d.target.x)
      .attr('y2', d => d.target.y)

    nodes
      // .attr('x', d => d.x)
      // .attr('y', d => d.y)
      .attr('transform', d => `translate(${d.x}, ${d.y})`)
      .attr('dy', d => 5)
  })

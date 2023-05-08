import * as d3 from 'd3'
import graph from './graphs/test1.json'

var colors = d3.scaleOrdinal(d3.schemeCategory10);

var
  svg = d3.select("svg"),
  width = 600, // svg.attr("width"),
  height = 600, // svg.attr("height"),
  node,
  link,
  edgepaths,
  edgelabels;

console.log(svg.width)

svg.append('defs').append('marker')
  .attr('id', 'arrowhead')
  .attr('viewBox', '-0 -5 10 10')
  .attr('refX', 13)
  .attr('refY', 0)
  .attr('orient', 'auto')
  .attr('markerWidth', 13)
  .attr('markerHeight', 13)
  .attr('xoverflow', 'visible')
  .append('svg:path')
  .attr('d', 'M 0,-5 L 10 ,0 L 0,5')
  .attr('fill', '#999')
  .style('stroke', 'none');

var simulation = d3.forceSimulation()
  .force("link", d3.forceLink().id(function (d) { return d.id; }).distance(300).strength(1))
  .force("charge", d3.forceManyBody())
  .force("center", d3.forceCenter(width / 2, height / 2));

function update(links, nodes) {
  link = svg.selectAll(".link")
    .data(links)
    .enter()
    .append("line")
    .attr("class", "link")
    // .attr('marker-end', 'url(#arrowhead)')

  link.append("title")
    .text(function (d) { return d.type; });

  edgepaths = svg.selectAll(".edgepath")
    .data(links)
    .enter()
    .append('path')
    .attr('class', 'edgepath')
    // .attr('fill-opacity', 0)
    // .attr('stroke-opacity', 0)
    .attr('id', function (d, i) { return 'edgepath' + i })
    // .style("pointer-events", "none");

  edgelabels = svg.selectAll(".edgelabel")
    .data(links)
    .enter()
    .append('text')
    // .style("pointer-events", "none")
    .attr('class', 'edgelabel')
    .attr('id', function (d, i) { return 'edgelabel' + i })
    // .attr('font-size', 10)
    // .attr('fill', '#aaa')

  edgelabels.append('textPath')
    .attr('xlink:href', function (d, i) { return '#edgepath' + i })
    // .style("text-anchor", "middle")
    // .style("pointer-events", "none")
    .attr("startOffset", "50%")
    .text(function (d) { return d.type });

  node = svg.selectAll(".node")
    .data(nodes)
    .enter()
    .append("g")
    .attr("class", "node")

  node.append("circle")
    .attr("r", 5)
    .style("fill", function (d, i) { return colors(i); })

  node.append("title")
    .text(function (d) { return d.id; });

  node.append("text")
    .attr("dy", -3)
    .text(function (d) { return `${d.name} :: ${d.label}` });

  simulation
    .nodes(nodes)
    .on("tick", ticked);

  simulation.force("link")
    .links(links);
}

function ticked(event) {
  link
    .attr("x1", function (d) { return d.source.x; })
    .attr("y1", function (d) { return d.source.y; })
    .attr("x2", function (d) { return d.target.x; })
    .attr("y2", function (d) { return d.target.y; });

  node
    .attr("transform", function (d) { return "translate(" + d.x + ", " + d.y + ")"; });

  edgepaths.attr('d', function (d) {
    return 'M ' + d.source.x + ' ' + d.source.y + ' L ' + d.target.x + ' ' + d.target.y;
  });

  edgelabels.attr('transform', function (d) {
    if (d.target.x < d.source.x) {
      var bbox = this.getBBox();
      var rx = bbox.x + bbox.width / 2;
      var ry = bbox.y + bbox.height / 2;
      return 'rotate(180 ' + rx + ' ' + ry + ')';
    }
    else {
      return 'rotate(0)';
    }
  });
}

// console.log(graph)
update(graph.links, graph.nodes)
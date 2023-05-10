import go from 'gojs'

// import generateData from './generation/add'
// import generateData from './examples/dijkstra'
import generateData from './examples/parsingLR'
import { network } from './examples/rule'

const maxIterations = 1000
const defaultElectricalCharge = 100
const defaultSpringLength = 20

function init() {

  // Since 2.2 you can also author concise templates with method chaining instead of GraphObject.make
  // For details, see https://gojs.net/latest/intro/buildingObjects.html
  const $ = go.GraphObject.make;  // for conciseness in defining templates

  let myDiagram =
    $(go.Diagram, "myDiagramDiv",  // must name or refer to the DIV HTML element
      {
        initialAutoScale: go.Diagram.Uniform,  // an initial automatic zoom-to-fit
        contentAlignment: go.Spot.Center,  // align document to the center of the viewport
        layout:
          $(go.ForceDirectedLayout,  // automatically spread nodes apart
            { maxIterations, defaultSpringLength, defaultElectricalCharge })
      });

  // define each Node's appearance
  myDiagram.nodeTemplate =
    $(go.Node, "Auto",  // the whole node panel
      { locationSpot: go.Spot.Center },
      // define the node's outer shape, which will surround the TextBlock
      $(go.Shape, "Rectangle",
        { fill: $(go.Brush, "Linear", { 0: "rgb(254, 201, 0)", 1: "rgb(254, 162, 0)" }), stroke: "black" }),
      $(go.TextBlock,
        { font: "bold 10pt serif, bold arial, sans-serif", margin: 4 },
        new go.Binding("text", "text"))
    );

  // replace the default Link template in the linkTemplateMap
  myDiagram.linkTemplate =
    $(go.Link,  // the whole link panel
      $(go.Shape,  // the link shape
        { stroke: "black" }),
      $(go.Shape,  // the arrowhead
        { toArrow: "standard", stroke: null }),
      $(go.Panel, "Auto",
        $(go.Shape,  // the label background, which becomes transparent around the edges
          {
            fill: $(go.Brush, "Radial", { 0: "rgb(240, 240, 240)", 0.3: "rgb(240, 240, 240)", 1: "rgba(240, 240, 240, 0)" }),
            stroke: null
          }),
        $(go.TextBlock,  // the label text
          {
            textAlign: "center",
            font: "10pt helvetica, arial, sans-serif",
            stroke: "#555555",
            margin: 4
          },
          new go.Binding("text", "text"))
      )
    );

  generateData()
  console.log("network", network)
  let nodeDataArray = network.nodes
  let linkDataArray = network.links
  myDiagram.model = new go.GraphLinksModel(nodeDataArray, linkDataArray);

  const button = document.createElement('button')
  button.onclick = (event) => {
    const canvas = document.getElementsByTagName('canvas')[0]
    const img_src = canvas.toDataURL('image/png')
    
    const img = document.createElement('img')
    img.src = img_src
    document.body.appendChild(img)
  }
  button.innerHTML = "create image"
  document.body.appendChild(button)
}

window.addEventListener('DOMContentLoaded', init);

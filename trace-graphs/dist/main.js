/******/ (() => { // webpackBootstrap
var __webpack_exports__ = {};
/*!**********************!*\
  !*** ./src/index.js ***!
  \**********************/
function init() {

  // Since 2.2 you can also author concise templates with method chaining instead of GraphObject.make
  // For details, see https://gojs.net/latest/intro/buildingObjects.html
  const $ = go.GraphObject.make;  // for conciseness in defining templates

  myDiagram =
    $(go.Diagram, "myDiagramDiv",  // must name or refer to the DIV HTML element
      {
        initialAutoScale: go.Diagram.Uniform,  // an initial automatic zoom-to-fit
        contentAlignment: go.Spot.Center,  // align document to the center of the viewport
        layout:
          $(go.ForceDirectedLayout,  // automatically spread nodes apart
            { maxIterations: 200, defaultSpringLength: 30, defaultElectricalCharge: 100 })
      });

  // define each Node's appearance
  myDiagram.nodeTemplate =
    $(go.Node, "Auto",  // the whole node panel
      { locationSpot: go.Spot.Center },
      // define the node's outer shape, which will surround the TextBlock
      $(go.Shape, "Rectangle",
        { fill: $(go.Brush, "Linear", { 0: "rgb(254, 201, 0)", 1: "rgb(254, 162, 0)" }), stroke: "black" }),
      $(go.TextBlock,
        { font: "bold 10pt helvetica, bold arial, sans-serif", margin: 4 },
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

  // create the model for the concept map
  var nodeDataArray = [
    { key: 1, text: "Concept Maps" },
    { key: 2, text: "Organized Knowledge" },
    { key: 3, text: "Context Dependent" },
    { key: 4, text: "Concepts" },
    { key: 5, text: "Propositions" },
    { key: 6, text: "Associated Feelings or Affect" },
    { key: 7, text: "Perceived Regularities" },
    { key: 8, text: "Labeled" },
    { key: 9, text: "Hierarchically Structured" },
    { key: 10, text: "Effective Teaching" },
    { key: 11, text: "Crosslinks" },
    { key: 12, text: "Effective Learning" },
    { key: 13, text: "Events (Happenings)" },
    { key: 14, text: "Objects (Things)" },
    { key: 15, text: "Symbols" },
    { key: 16, text: "Words" },
    { key: 17, text: "Creativity" },
    { key: 18, text: "Interrelationships" },
    { key: 19, text: "Infants" },
    { key: 20, text: "Different Map Segments" }
  ];
  var linkDataArray = [
    { from: 1, to: 2, text: "represent" },
    { from: 2, to: 3, text: "is" },
    { from: 2, to: 4, text: "is" },
    { from: 2, to: 5, text: "is" },
    { from: 2, to: 6, text: "includes" },
    { from: 2, to: 10, text: "necessary\nfor" },
    { from: 2, to: 12, text: "necessary\nfor" },
    { from: 4, to: 5, text: "combine\nto form" },
    { from: 4, to: 6, text: "include" },
    { from: 4, to: 7, text: "are" },
    { from: 4, to: 8, text: "are" },
    { from: 4, to: 9, text: "are" },
    { from: 5, to: 9, text: "are" },
    { from: 5, to: 11, text: "may be" },
    { from: 7, to: 13, text: "in" },
    { from: 7, to: 14, text: "in" },
    { from: 7, to: 19, text: "begin\nwith" },
    { from: 8, to: 15, text: "with" },
    { from: 8, to: 16, text: "with" },
    { from: 9, to: 17, text: "aids" },
    { from: 11, to: 18, text: "show" },
    { from: 12, to: 19, text: "begins\nwith" },
    { from: 17, to: 18, text: "needed\nto see" },
    { from: 18, to: 20, text: "between" }
  ];
  myDiagram.model = new go.GraphLinksModel(nodeDataArray, linkDataArray);
}

window.addEventListener('DOMContentLoaded', init);
/******/ })()
;
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibWFpbi5qcyIsIm1hcHBpbmdzIjoiOzs7OztBQUFBOztBQUVBO0FBQ0E7QUFDQSxrQ0FBa0M7O0FBRWxDO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0EsY0FBYywyRUFBMkU7QUFDekYsT0FBTzs7QUFFUDtBQUNBO0FBQ0E7QUFDQSxRQUFRLDhCQUE4QjtBQUN0QztBQUNBO0FBQ0EsVUFBVSw4QkFBOEIsOENBQThDLG9CQUFvQjtBQUMxRztBQUNBLFVBQVUsZ0VBQWdFO0FBQzFFO0FBQ0E7O0FBRUE7QUFDQTtBQUNBO0FBQ0E7QUFDQSxVQUFVLGlCQUFpQjtBQUMzQjtBQUNBLFVBQVUsbUNBQW1DO0FBQzdDO0FBQ0E7QUFDQTtBQUNBLDBDQUEwQyxpRkFBaUY7QUFDM0g7QUFDQSxXQUFXO0FBQ1g7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0EsV0FBVztBQUNYO0FBQ0E7QUFDQTs7QUFFQTtBQUNBO0FBQ0EsTUFBTSw4QkFBOEI7QUFDcEMsTUFBTSxxQ0FBcUM7QUFDM0MsTUFBTSxtQ0FBbUM7QUFDekMsTUFBTSwwQkFBMEI7QUFDaEMsTUFBTSw4QkFBOEI7QUFDcEMsTUFBTSwrQ0FBK0M7QUFDckQsTUFBTSx3Q0FBd0M7QUFDOUMsTUFBTSx5QkFBeUI7QUFDL0IsTUFBTSwyQ0FBMkM7QUFDakQsTUFBTSxxQ0FBcUM7QUFDM0MsTUFBTSw2QkFBNkI7QUFDbkMsTUFBTSxxQ0FBcUM7QUFDM0MsTUFBTSxzQ0FBc0M7QUFDNUMsTUFBTSxtQ0FBbUM7QUFDekMsTUFBTSwwQkFBMEI7QUFDaEMsTUFBTSx3QkFBd0I7QUFDOUIsTUFBTSw2QkFBNkI7QUFDbkMsTUFBTSxxQ0FBcUM7QUFDM0MsTUFBTSwwQkFBMEI7QUFDaEMsTUFBTTtBQUNOO0FBQ0E7QUFDQSxNQUFNLG1DQUFtQztBQUN6QyxNQUFNLDRCQUE0QjtBQUNsQyxNQUFNLDRCQUE0QjtBQUNsQyxNQUFNLDRCQUE0QjtBQUNsQyxNQUFNLGtDQUFrQztBQUN4QyxNQUFNLHlDQUF5QztBQUMvQyxNQUFNLHlDQUF5QztBQUMvQyxNQUFNLDBDQUEwQztBQUNoRCxNQUFNLGlDQUFpQztBQUN2QyxNQUFNLDZCQUE2QjtBQUNuQyxNQUFNLDZCQUE2QjtBQUNuQyxNQUFNLDZCQUE2QjtBQUNuQyxNQUFNLDZCQUE2QjtBQUNuQyxNQUFNLGlDQUFpQztBQUN2QyxNQUFNLDZCQUE2QjtBQUNuQyxNQUFNLDZCQUE2QjtBQUNuQyxNQUFNLHNDQUFzQztBQUM1QyxNQUFNLCtCQUErQjtBQUNyQyxNQUFNLCtCQUErQjtBQUNyQyxNQUFNLCtCQUErQjtBQUNyQyxNQUFNLGdDQUFnQztBQUN0QyxNQUFNLHdDQUF3QztBQUM5QyxNQUFNLDBDQUEwQztBQUNoRCxNQUFNO0FBQ047QUFDQTtBQUNBOztBQUVBLGtEIiwic291cmNlcyI6WyJ3ZWJwYWNrOi8vZ3JhcGhzLy4vc3JjL2luZGV4LmpzIl0sInNvdXJjZXNDb250ZW50IjpbImZ1bmN0aW9uIGluaXQoKSB7XG5cbiAgLy8gU2luY2UgMi4yIHlvdSBjYW4gYWxzbyBhdXRob3IgY29uY2lzZSB0ZW1wbGF0ZXMgd2l0aCBtZXRob2QgY2hhaW5pbmcgaW5zdGVhZCBvZiBHcmFwaE9iamVjdC5tYWtlXG4gIC8vIEZvciBkZXRhaWxzLCBzZWUgaHR0cHM6Ly9nb2pzLm5ldC9sYXRlc3QvaW50cm8vYnVpbGRpbmdPYmplY3RzLmh0bWxcbiAgY29uc3QgJCA9IGdvLkdyYXBoT2JqZWN0Lm1ha2U7ICAvLyBmb3IgY29uY2lzZW5lc3MgaW4gZGVmaW5pbmcgdGVtcGxhdGVzXG5cbiAgbXlEaWFncmFtID1cbiAgICAkKGdvLkRpYWdyYW0sIFwibXlEaWFncmFtRGl2XCIsICAvLyBtdXN0IG5hbWUgb3IgcmVmZXIgdG8gdGhlIERJViBIVE1MIGVsZW1lbnRcbiAgICAgIHtcbiAgICAgICAgaW5pdGlhbEF1dG9TY2FsZTogZ28uRGlhZ3JhbS5Vbmlmb3JtLCAgLy8gYW4gaW5pdGlhbCBhdXRvbWF0aWMgem9vbS10by1maXRcbiAgICAgICAgY29udGVudEFsaWdubWVudDogZ28uU3BvdC5DZW50ZXIsICAvLyBhbGlnbiBkb2N1bWVudCB0byB0aGUgY2VudGVyIG9mIHRoZSB2aWV3cG9ydFxuICAgICAgICBsYXlvdXQ6XG4gICAgICAgICAgJChnby5Gb3JjZURpcmVjdGVkTGF5b3V0LCAgLy8gYXV0b21hdGljYWxseSBzcHJlYWQgbm9kZXMgYXBhcnRcbiAgICAgICAgICAgIHsgbWF4SXRlcmF0aW9uczogMjAwLCBkZWZhdWx0U3ByaW5nTGVuZ3RoOiAzMCwgZGVmYXVsdEVsZWN0cmljYWxDaGFyZ2U6IDEwMCB9KVxuICAgICAgfSk7XG5cbiAgLy8gZGVmaW5lIGVhY2ggTm9kZSdzIGFwcGVhcmFuY2VcbiAgbXlEaWFncmFtLm5vZGVUZW1wbGF0ZSA9XG4gICAgJChnby5Ob2RlLCBcIkF1dG9cIiwgIC8vIHRoZSB3aG9sZSBub2RlIHBhbmVsXG4gICAgICB7IGxvY2F0aW9uU3BvdDogZ28uU3BvdC5DZW50ZXIgfSxcbiAgICAgIC8vIGRlZmluZSB0aGUgbm9kZSdzIG91dGVyIHNoYXBlLCB3aGljaCB3aWxsIHN1cnJvdW5kIHRoZSBUZXh0QmxvY2tcbiAgICAgICQoZ28uU2hhcGUsIFwiUmVjdGFuZ2xlXCIsXG4gICAgICAgIHsgZmlsbDogJChnby5CcnVzaCwgXCJMaW5lYXJcIiwgeyAwOiBcInJnYigyNTQsIDIwMSwgMClcIiwgMTogXCJyZ2IoMjU0LCAxNjIsIDApXCIgfSksIHN0cm9rZTogXCJibGFja1wiIH0pLFxuICAgICAgJChnby5UZXh0QmxvY2ssXG4gICAgICAgIHsgZm9udDogXCJib2xkIDEwcHQgaGVsdmV0aWNhLCBib2xkIGFyaWFsLCBzYW5zLXNlcmlmXCIsIG1hcmdpbjogNCB9LFxuICAgICAgICBuZXcgZ28uQmluZGluZyhcInRleHRcIiwgXCJ0ZXh0XCIpKVxuICAgICk7XG5cbiAgLy8gcmVwbGFjZSB0aGUgZGVmYXVsdCBMaW5rIHRlbXBsYXRlIGluIHRoZSBsaW5rVGVtcGxhdGVNYXBcbiAgbXlEaWFncmFtLmxpbmtUZW1wbGF0ZSA9XG4gICAgJChnby5MaW5rLCAgLy8gdGhlIHdob2xlIGxpbmsgcGFuZWxcbiAgICAgICQoZ28uU2hhcGUsICAvLyB0aGUgbGluayBzaGFwZVxuICAgICAgICB7IHN0cm9rZTogXCJibGFja1wiIH0pLFxuICAgICAgJChnby5TaGFwZSwgIC8vIHRoZSBhcnJvd2hlYWRcbiAgICAgICAgeyB0b0Fycm93OiBcInN0YW5kYXJkXCIsIHN0cm9rZTogbnVsbCB9KSxcbiAgICAgICQoZ28uUGFuZWwsIFwiQXV0b1wiLFxuICAgICAgICAkKGdvLlNoYXBlLCAgLy8gdGhlIGxhYmVsIGJhY2tncm91bmQsIHdoaWNoIGJlY29tZXMgdHJhbnNwYXJlbnQgYXJvdW5kIHRoZSBlZGdlc1xuICAgICAgICAgIHtcbiAgICAgICAgICAgIGZpbGw6ICQoZ28uQnJ1c2gsIFwiUmFkaWFsXCIsIHsgMDogXCJyZ2IoMjQwLCAyNDAsIDI0MClcIiwgMC4zOiBcInJnYigyNDAsIDI0MCwgMjQwKVwiLCAxOiBcInJnYmEoMjQwLCAyNDAsIDI0MCwgMClcIiB9KSxcbiAgICAgICAgICAgIHN0cm9rZTogbnVsbFxuICAgICAgICAgIH0pLFxuICAgICAgICAkKGdvLlRleHRCbG9jaywgIC8vIHRoZSBsYWJlbCB0ZXh0XG4gICAgICAgICAge1xuICAgICAgICAgICAgdGV4dEFsaWduOiBcImNlbnRlclwiLFxuICAgICAgICAgICAgZm9udDogXCIxMHB0IGhlbHZldGljYSwgYXJpYWwsIHNhbnMtc2VyaWZcIixcbiAgICAgICAgICAgIHN0cm9rZTogXCIjNTU1NTU1XCIsXG4gICAgICAgICAgICBtYXJnaW46IDRcbiAgICAgICAgICB9LFxuICAgICAgICAgIG5ldyBnby5CaW5kaW5nKFwidGV4dFwiLCBcInRleHRcIikpXG4gICAgICApXG4gICAgKTtcblxuICAvLyBjcmVhdGUgdGhlIG1vZGVsIGZvciB0aGUgY29uY2VwdCBtYXBcbiAgdmFyIG5vZGVEYXRhQXJyYXkgPSBbXG4gICAgeyBrZXk6IDEsIHRleHQ6IFwiQ29uY2VwdCBNYXBzXCIgfSxcbiAgICB7IGtleTogMiwgdGV4dDogXCJPcmdhbml6ZWQgS25vd2xlZGdlXCIgfSxcbiAgICB7IGtleTogMywgdGV4dDogXCJDb250ZXh0IERlcGVuZGVudFwiIH0sXG4gICAgeyBrZXk6IDQsIHRleHQ6IFwiQ29uY2VwdHNcIiB9LFxuICAgIHsga2V5OiA1LCB0ZXh0OiBcIlByb3Bvc2l0aW9uc1wiIH0sXG4gICAgeyBrZXk6IDYsIHRleHQ6IFwiQXNzb2NpYXRlZCBGZWVsaW5ncyBvciBBZmZlY3RcIiB9LFxuICAgIHsga2V5OiA3LCB0ZXh0OiBcIlBlcmNlaXZlZCBSZWd1bGFyaXRpZXNcIiB9LFxuICAgIHsga2V5OiA4LCB0ZXh0OiBcIkxhYmVsZWRcIiB9LFxuICAgIHsga2V5OiA5LCB0ZXh0OiBcIkhpZXJhcmNoaWNhbGx5IFN0cnVjdHVyZWRcIiB9LFxuICAgIHsga2V5OiAxMCwgdGV4dDogXCJFZmZlY3RpdmUgVGVhY2hpbmdcIiB9LFxuICAgIHsga2V5OiAxMSwgdGV4dDogXCJDcm9zc2xpbmtzXCIgfSxcbiAgICB7IGtleTogMTIsIHRleHQ6IFwiRWZmZWN0aXZlIExlYXJuaW5nXCIgfSxcbiAgICB7IGtleTogMTMsIHRleHQ6IFwiRXZlbnRzIChIYXBwZW5pbmdzKVwiIH0sXG4gICAgeyBrZXk6IDE0LCB0ZXh0OiBcIk9iamVjdHMgKFRoaW5ncylcIiB9LFxuICAgIHsga2V5OiAxNSwgdGV4dDogXCJTeW1ib2xzXCIgfSxcbiAgICB7IGtleTogMTYsIHRleHQ6IFwiV29yZHNcIiB9LFxuICAgIHsga2V5OiAxNywgdGV4dDogXCJDcmVhdGl2aXR5XCIgfSxcbiAgICB7IGtleTogMTgsIHRleHQ6IFwiSW50ZXJyZWxhdGlvbnNoaXBzXCIgfSxcbiAgICB7IGtleTogMTksIHRleHQ6IFwiSW5mYW50c1wiIH0sXG4gICAgeyBrZXk6IDIwLCB0ZXh0OiBcIkRpZmZlcmVudCBNYXAgU2VnbWVudHNcIiB9XG4gIF07XG4gIHZhciBsaW5rRGF0YUFycmF5ID0gW1xuICAgIHsgZnJvbTogMSwgdG86IDIsIHRleHQ6IFwicmVwcmVzZW50XCIgfSxcbiAgICB7IGZyb206IDIsIHRvOiAzLCB0ZXh0OiBcImlzXCIgfSxcbiAgICB7IGZyb206IDIsIHRvOiA0LCB0ZXh0OiBcImlzXCIgfSxcbiAgICB7IGZyb206IDIsIHRvOiA1LCB0ZXh0OiBcImlzXCIgfSxcbiAgICB7IGZyb206IDIsIHRvOiA2LCB0ZXh0OiBcImluY2x1ZGVzXCIgfSxcbiAgICB7IGZyb206IDIsIHRvOiAxMCwgdGV4dDogXCJuZWNlc3NhcnlcXG5mb3JcIiB9LFxuICAgIHsgZnJvbTogMiwgdG86IDEyLCB0ZXh0OiBcIm5lY2Vzc2FyeVxcbmZvclwiIH0sXG4gICAgeyBmcm9tOiA0LCB0bzogNSwgdGV4dDogXCJjb21iaW5lXFxudG8gZm9ybVwiIH0sXG4gICAgeyBmcm9tOiA0LCB0bzogNiwgdGV4dDogXCJpbmNsdWRlXCIgfSxcbiAgICB7IGZyb206IDQsIHRvOiA3LCB0ZXh0OiBcImFyZVwiIH0sXG4gICAgeyBmcm9tOiA0LCB0bzogOCwgdGV4dDogXCJhcmVcIiB9LFxuICAgIHsgZnJvbTogNCwgdG86IDksIHRleHQ6IFwiYXJlXCIgfSxcbiAgICB7IGZyb206IDUsIHRvOiA5LCB0ZXh0OiBcImFyZVwiIH0sXG4gICAgeyBmcm9tOiA1LCB0bzogMTEsIHRleHQ6IFwibWF5IGJlXCIgfSxcbiAgICB7IGZyb206IDcsIHRvOiAxMywgdGV4dDogXCJpblwiIH0sXG4gICAgeyBmcm9tOiA3LCB0bzogMTQsIHRleHQ6IFwiaW5cIiB9LFxuICAgIHsgZnJvbTogNywgdG86IDE5LCB0ZXh0OiBcImJlZ2luXFxud2l0aFwiIH0sXG4gICAgeyBmcm9tOiA4LCB0bzogMTUsIHRleHQ6IFwid2l0aFwiIH0sXG4gICAgeyBmcm9tOiA4LCB0bzogMTYsIHRleHQ6IFwid2l0aFwiIH0sXG4gICAgeyBmcm9tOiA5LCB0bzogMTcsIHRleHQ6IFwiYWlkc1wiIH0sXG4gICAgeyBmcm9tOiAxMSwgdG86IDE4LCB0ZXh0OiBcInNob3dcIiB9LFxuICAgIHsgZnJvbTogMTIsIHRvOiAxOSwgdGV4dDogXCJiZWdpbnNcXG53aXRoXCIgfSxcbiAgICB7IGZyb206IDE3LCB0bzogMTgsIHRleHQ6IFwibmVlZGVkXFxudG8gc2VlXCIgfSxcbiAgICB7IGZyb206IDE4LCB0bzogMjAsIHRleHQ6IFwiYmV0d2VlblwiIH1cbiAgXTtcbiAgbXlEaWFncmFtLm1vZGVsID0gbmV3IGdvLkdyYXBoTGlua3NNb2RlbChub2RlRGF0YUFycmF5LCBsaW5rRGF0YUFycmF5KTtcbn1cblxud2luZG93LmFkZEV2ZW50TGlzdGVuZXIoJ0RPTUNvbnRlbnRMb2FkZWQnLCBpbml0KTsiXSwibmFtZXMiOltdLCJzb3VyY2VSb290IjoiIn0=
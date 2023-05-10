export default function gen() {
  var nodes = [
    {
      'label': "A",
      'id': 1,
    },
    {
      'label': "B",
      'id': 2
    },
  ]
  var links = [
    {
      'label': "A -> B",
      'source': 0,
      'target': 1
    }
  ]
  return ({
    "nodes": nodes,
    "links": links
  })
}
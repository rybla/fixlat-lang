# Dijkstra

Index: expression-queryable index of lattice values
- insert -- given a value, modify the internal state of the index such that
  future queries of an expression that can match the inserted value yield the a
  value that is the lattice max/min among all inserted values that match that
  expression
- query -- given an expression, yields a substitution of free variables
  in that expression that yields a value that was inserted into the index

Operations:
- initialize unvisiteds
  - enumerate vertices
- get neighbors of vertex
- get vertex distance
- check if relation is "new"
  - concretely, check if "next" relation already has a derived distance

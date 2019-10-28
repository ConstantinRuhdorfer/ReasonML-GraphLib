open Vertex;
open Edge;

/**
 * Edges only have implicit direction a -> b
 */
type directedGraph =
  | Empty
  | DirectedGraph(vertecies, edges);

module type DirectedGraph = {
  let addVertex: (vertex, directedGraph) => directedGraph;
  let removeVertex: (vertex, directedGraph) => directedGraph;
  let addEdge: (edge, directedGraph) => directedGraph;
  let removeEdge: (edge, directedGraph) => directedGraph;
  let getNeighbors: (vertex, directedGraph) => vertecies;
  let getReachableNeighbors: (vertex, directedGraph) => vertecies;
};

module DirectedGraph: DirectedGraph = {
  let addVertex = (vertex, directedGraph) => {
    switch (directedGraph) {
    | Empty => DirectedGraph([vertex], [])
    | DirectedGraph(vertecies, edges) =>
      DirectedGraph([vertex, ...vertecies], edges)
    };
  };
  let removeVertex = (vertex, directedGraph) => {
    switch (directedGraph) {
    | Empty => Empty
    | DirectedGraph([], edges) => DirectedGraph([], edges)
    | DirectedGraph(vertecies, edges) =>
      DirectedGraph(
        List.filter(v => v != vertex, vertecies),
        List.filter(
          e => e.vertecie_a != vertex && e.vertecie_b != vertex,
          edges,
        ),
      )
    };
  };
  let addEdge = (edge, directedGraph) => {
    switch (directedGraph) {
    | Empty => DirectedGraph([], [edge])
    | DirectedGraph(vertecies, edges) =>
      DirectedGraph(vertecies, [edge, ...edges])
    };
  };
  let removeEdge = (edge, directedGraph) => {
    switch (directedGraph) {
    | Empty => Empty
    | DirectedGraph(vertecies, []) => DirectedGraph(vertecies, [])
    | DirectedGraph(vertecies, edges) =>
      DirectedGraph(
        vertecies,
        List.filter(
          e =>
            e.vertecie_a != edge.vertecie_a || e.vertecie_b != edge.vertecie_b,
          edges,
        ),
      )
    };
  };
  let getNeighbors = (from, directedGraph) => {
    switch (directedGraph) {
    | Empty => []
    | DirectedGraph(_, edges) =>
      List.fold_left(
        (list, edge) =>
          if (edge.vertecie_a == from) {
            [edge.vertecie_b, ...list];
          } else if (edge.vertecie_b == from) {
            [edge.vertecie_a, ...list];
          } else {
            list;
          },
        [],
        edges,
      )
    };}
  let getReachableNeighbors = (from, directedGraph) => {
    switch (directedGraph) {
    | Empty => []
    | DirectedGraph(_, edges) =>
      List.fold_left(
        (list, edge) =>
          if (edge.vertecie_a == from) {
            [edge.vertecie_b, ...list];
          } else {
            list;
          },
        [],
        edges,
      )
    };}
};
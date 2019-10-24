open Vertex;
open Edge;

/**
 * A graph is either a module graph or empty...
 */
type graph =
  | Empty
  | Graph(vertecies, edges);

/**
 * A graph implements the following features:
 */
module type Graph = {
  let addVertex: (vertex, graph) => graph;
  let removeVertex: (vertex, graph) => graph;
  let addEdge: (edge, graph) => graph;
  let removeEdge: (edge, graph) => graph;
};

/**
 * The actual implementation:
 */
module Graph: Graph = {
  let addVertex = (vertex, graph) => {
    switch (graph) {
    | Empty => Graph([vertex], [])
    | Graph(vertecies, edges) => Graph([vertex, ...vertecies], edges)
    };
  };
  let removeVertex = (vertex, graph) => {
    switch (graph) {
    | Empty => Empty
    | Graph([], edges) => Graph([], edges)
    | Graph(vertecies, edges) =>
      Graph(
        List.filter((v: vertex) => v != vertex, vertecies),
        List.filter(
          (e: edge) => e.vertecie_a != vertex && e.vertecie_b != vertex,
          edges,
        ),
      )
    };
  };
  let addEdge = (edge, graph) => {
    switch (graph) {
    | Empty => Graph([], [edge])
    | Graph(vertecies, edges) => Graph(vertecies, [edge, ...edges])
    };
  };
  let removeEdge = (edge, graph) => {
    switch (graph) {
    | Empty => Empty
    | Graph(vertecies, []) => Graph(vertecies, [])
    | Graph(vertecies, edges) =>
      Graph(
        vertecies,
        List.filter(
          (e: edge) =>
            (
              e.vertecie_a != edge.vertecie_a || e.vertecie_b != edge.vertecie_b
            )
            && (
              e.vertecie_a != edge.vertecie_b
              || e.vertecie_b != edge.vertecie_a
            ),
          edges,
        ),
      )
    };
  };
};
open Vertex;
open Edge;
open Print;
open IntHashtbl;

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
  let getNeighbors: (vertex, graph) => vertecies;
  let visitConnections: (vertex, intHashtbl, graph) => vertecies;
  let getConnectedComponents: (graph, int) => list(vertecies);
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
  let getNeighbors = (from: vertex, graph) => {
    switch (graph) {
    | Empty => []
    | Graph(_, edges) =>
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
    };
  };
  let rec visitConnections = (vertex, alreadyVisited, graph) => {
    switch (Hashtbl.find(alreadyVisited, vertex)) {
      | vertex => [];
      | exception Not_found => {
        let neighbours = getNeighbors(vertex, graph);
        Hashtbl.add(alreadyVisited, vertex, vertex);
        List.fold_left(
          (list: vertecies, vertex: vertex) => {
            let partWork = visitConnections(vertex, alreadyVisited, graph);
            if(List.length(partWork) == 0) {
              list;
            } else {
              List.iter(elem => Hashtbl.add(alreadyVisited, elem, elem), partWork);
              List.concat([partWork, list]);
            }
          },
          [vertex],
          neighbours
        )
      };
    }
  };
  let getConnectedComponents = (graph, hashTblSize: int) => {
    switch (graph) {
      | Empty => []
      | Graph(vertecies, []) => 
        List.fold_left(
          (list, vertex) => [[vertex], ...list],
          [],
          vertecies,
        );
      | Graph(vertecies, edges) => {
        let alreadyVisited = Hashtbl.create(hashTblSize)
        List.fold_left(
          (list: list(vertecies), vertex) => {
            let component = visitConnections(vertex, alreadyVisited, graph);
            if (List.length(component) == 0) {
              list;
            } else {
              [component, ...list];
            } 
          },
          [],
          vertecies,
        )
      }
    }
  };
};
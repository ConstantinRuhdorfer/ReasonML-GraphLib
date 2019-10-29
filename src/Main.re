open Edge
open Vertex
open Print
open Graph

/**
 * Reads in a file with the format:
 * 
 * <Number of vertecies>
 * <edge e1 vertex a> <edge e1 vertexb>
 * … 
 * 
 * And performs calculations on the resulting graph.
 */
let file_in_channel = open_in("input/graph8.plain");

let file_stream =
  Stream.from(_i =>
    switch (Pervasives.input_line(file_in_channel)) {
    | line => Some(line)
    | exception End_of_file => None
    }
  );

let rec parse_input_vertecies = (number: int, list) => {
  switch (number) {
  | 0 => list
  | _ => parse_input_vertecies(number - 1, [number - 1, ...list]);
  }
};

let rec parse_input_edges = (stream, list) => { 
  switch (Stream.next(stream)) {
    | "" => list
    | exception Stream.Failure => list
    | string => {
      let sub_strings = Str.split((Str.regexp("[ \t]+")), string)
      let vertex_a: vertex = int_of_string(List.nth(sub_strings, 0))
      let vertex_b: vertex = int_of_string(List.nth(sub_strings, 1))
      parse_input_edges(stream, [{vertecie_a: vertex_a, vertecie_b: vertex_b}, ...list])
    };
  };
}

let inS = file_stream

let num_vertecies = int_of_string(Stream.next(inS))
let input_vertecies: list(int) = parse_input_vertecies(num_vertecies, [])

let input_edges = parse_input_edges(inS, [])

let aGraph = Graph(input_vertecies, input_edges);
let connected = Graph.getConnectedComponents(aGraph, num_vertecies)

print_newline()
print_string("Result.")

print_list(connected, print_vertecies)

print_newline()
print_string("Finished.")
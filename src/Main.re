open DirectedGraph
open Edge
open Vertex

// type input_vertecies = vertecies;

let file_in_channel = Pervasives.open_in("input/digraph8.plain");

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

let directedGraph = DirectedGraph(input_vertecies, input_edges);

print_newline()
print_string("Finished.")
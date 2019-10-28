open Edge

let rec print_list = (list, func) => {
  switch list {
  | [] => print_newline()
  | [head, ...tail] => {
    print_string("[")
    func(head);
    print_string("]")
    print_list(tail, func);
  }
  };
}

let print_edge = edge => {
  switch edge {
    | edge => {
      print_int(edge.vertecie_a)
      print_string("->")
      print_int(edge.vertecie_b)
      print_newline()
      };
  } 
}

let rec print_vertecies = vertecies => {
  switch vertecies {
    | [] => print_string("") 
    | [head, ...tail] => {
      print_int(head)
      print_vertecies(tail)
    }
  }
}

let print_list_list = (list_of_lists) => {
  print_list(list_of_lists, print_list)
}

let print_list_int = list => {
  print_list(list, print_int)
}

let print_list_str = list => {
  print_list(list, print_string)
}

let print_list_edge = list => {
  print_list(list, print_edge)
}

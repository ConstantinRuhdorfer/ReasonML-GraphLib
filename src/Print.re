open Edge

let rec print_list_int = list => {
  switch list {
  | [] => print_string("Finished")
  | [head, ...tail] => {
    print_int(head);
    print_list_int(tail);
  }
  };
}

let rec print_list_str = list => {
  switch list {
  | [] => print_newline()
  | [head, ...tail] => {
    print_string(head);
    print_list_str(tail);
  }
  };
}

let rec print_list = (list, func) => {
  switch list {
  | [] => print_newline()
  | [head, ...tail] => {
    func(head);
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
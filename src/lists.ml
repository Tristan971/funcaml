let print_list_int (l: (int list)) : unit = 
  let rec print_list_elem (li: ('a list)) = match li with
    | [] -> print_string "]"
    | h::[] -> print_int h; print_char ']'
    | h::n::t -> print_int h; print_string ", "; print_list_elem (n::t)
  in print_char '['; print_list_elem l

let print_list_str (l: (string list)): unit =
  let rec print_list_elem (li: ('a list)) = match li with
    | [] -> print_string "]"
    | h::[] -> print_string h; print_char ']'
    | h::n::t -> print_string h; print_string ", "; print_list_elem (n::t)
  in print_char '['; print_list_elem l

let main : unit = print_list_int [1;2;3]

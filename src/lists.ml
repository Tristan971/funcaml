let print_list (l: (int list)) : unit = 
  let rec print_list_elem (li: ('a list)) = match li with
    | [] -> print_string "]"
    | h::[] -> print_int h; print_char ']'
    | h::n::t -> print_int h; print_string ", "; print_list_elem (n::t)
  in print_char '['; print_list_elem l

let main : unit = print_int(List.length([])); print_list [1;2;3]

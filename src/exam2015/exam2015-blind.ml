#load "str.cma"

(* UTILS *)
let print_list_str (l: (string list)): unit =
  let rec print_list_elem (li: ('a list)) = match li with
    | [] -> print_string "]"
    | h::[] -> print_string h; print_char ']'
    | h::n::t -> print_string h; print_string ", "; print_list_elem (n::t)
  in print_char '['; print_list_elem l

let print_list_list (l: (string list) list): unit = 
  let rec print_list_elem (li: (string list) list) = match li with
    | [] -> print_string "]"
    | h::[] -> print_list_str h; print_char ']'
    | h::n::t -> print_list_str h; print_string ", "; print_list_elem (n::t)
  in print_char '['; print_list_elem l

(* Q1.1 *)
let split_lines (lines: (string list)) : (string list) list = 
  let split_line (line: string) : string list = Str.split (Str.regexp " ") line
  in List.map split_line lines

(* T 1.1 *)
let test_split: unit = 
  let test_lines : (string list) = [ "14/02 11:45 --:-- 6:03 18:29"; "15/02 0:09 12:37 6:51 19:20" ] in
  let res: (string list) list = split_lines test_lines in
  let expected : (string list) list = [ 
    [ "14/02"; "11:45"; "--:--"; "6:03"; "18:29" ];
    [ "15/02"; "0:09"; "12:37"; "6:51"; "19:20" ] 
  ] in 
  print_string "Expected : "; print_list_list expected; print_char '\n'; 
  print_string "Got : "; print_list_list res; print_char '\n'


(* EXECTESTS *)
let main: unit = test_split
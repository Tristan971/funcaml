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

(* T1.1 *)
let test_split: unit = 
  let test_lines : (string list) = [ "14/02 11:45 --:-- 6:03 18:29"; "15/02 0:09 12:37 6:51 19:20" ] in
  let res: (string list) list = split_lines test_lines in
  let expected : (string list) list = [ 
    [ "14/02"; "11:45"; "--:--"; "6:03"; "18:29" ];
    [ "15/02"; "0:09"; "12:37"; "6:51"; "19:20" ] 
  ] in 
  print_string "Expected : "; print_list_list expected; print_char '\n'; 
  print_string "Got : "; print_list_list res; print_char '\n'

(* Q1.2 *)
type tide_data = {
  date : string;
  hmam : string;
  hmpm : string;
  bmam : string;
  bmpm : string
}

let to_tide_data (line: (string list)) : tide_data =
  match line with
  | [d;h1;h2;h3;h4] -> { date=d; bmam=h1; hmam=h2; bmpm=h3; hmpm=h4 }
  | _ -> failwith "bad line format"

let to_tide_datas (lines: (string list) list) : (tide_data) list = 
  List.map to_tide_data lines

(* T1.2 *)
let test_parse: unit =
  let expected_result = to_tide_data [ "14/02"; "11:45"; "--:--"; "6:03"; "18:29" ] in
  match expected_result with 
    | {date;hmam;hmpm;bmam;bmpm} -> print_string "Success parse : OK"

(* EXECTESTS *)
let main: unit = 
  test_split; 
  test_parse;

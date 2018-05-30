#load "str.cma"

(* BEGIN_UTILS *)

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

let expect_string_value (expected: string) (actual: string) : bool =
  let cmpRes : int = String.compare expected actual in
  if cmpRes == 0 then 
    true 
  else
    let _ = Printf.printf "Actual != Expected! (CMP_RES = %i)\n\t-> Expected : %s\n\t-> Actual : %s\n" cmpRes expected actual in
    false

(* END_UTILS *)

(* Q1.1 *)
let split_lines (lines: (string list)) : (string list) list = 
  let split_line (line: string) : string list = Str.split (Str.regexp " ") line
  in List.map split_line lines

(* T1.1 *)
let test_split: bool = 
  let test_lines : (string list) = [ "14/02 11:45 --:-- 6:03 18:29"; "15/02 0:09 12:37 6:51 19:20" ] in
  let res: (string list) list = split_lines test_lines in
  let expected : (string list) list = [ 
    [ "14/02"; "11:45"; "--:--"; "6:03"; "18:29" ];
    [ "15/02"; "0:09"; "12:37"; "6:51"; "19:20" ] 
  ] in 
  List.length res == List.length expected

(* TYPE tide_data *)
type tide_data = {
  date : string;
  hmam : string;
  hmpm : string;
  bmam : string;
  bmpm : string
}

(* Q1.2 *)
let to_tide_data (line: (string list)) : tide_data =
  match line with
  | [d;h1;h2;h3;h4] -> { date=d; bmam=h1; hmam=h2; bmpm=h3; hmpm=h4 }
  | _ -> failwith "bad line format"

(* T1.2 *)
let test_parse: bool =
  let res = to_tide_data [ "14/02"; "11:45"; "--:--"; "6:03"; "18:29" ] in
  match res with 
    | {date;hmam;hmpm;bmam;bmpm} -> true

(* Q1.3 *)
let to_tide_datas (lines: (string list) list) : (tide_data) list = 
  List.map to_tide_data lines

(* T1.3 *)
let test_parse_list: bool =
  let res = to_tide_datas [ [ "14/02"; "11:45"; "--:--"; "6:03"; "18:29" ];[ "15/02"; "0:09"; "12:37"; "6:51"; "19:20" ] ] in
  match res with
    | d1::d2::[] -> true
    | _ -> false

(* Q1.4 *)
let reorder_data (tide_data: tide_data) : string = 
  let sorted_hours : string = 
    if (String.compare tide_data.bmam tide_data.hmam) < 0 then
      Format.asprintf "%s(BM) %s(HM) %s(BM) %s(HM)" tide_data.bmam tide_data.hmam tide_data.bmpm tide_data.hmpm
    else
      Format.asprintf "%s(BM) %s(HM) %s(BM) %s(HM)" tide_data.hmam tide_data.bmam tide_data.hmpm tide_data.bmpm
    in
  Format.asprintf "%s %s" tide_data.date sorted_hours

(* T1.4 *)
let test_reorder_data : bool =
  let expected : string = "15/02 0:09(HM) 6:51(BM) 12:37(HM) 19:20(BM)" in
  let actual : string = reorder_data { date="15/02"; hmam="0:09"; hmpm="12:37"; bmam="6:51"; bmpm="19:20" } in
  expect_string_value expected actual

(* EXECTESTS *)
let exec_test (name: string) (test_fn: bool) : unit =
  let _ = Printf.printf "%s -> " name in
  if test_fn then
    output_string stdout (Format.asprintf "%s" "OK\n")
  else
    output_string stderr (Format.asprintf "<!!!> NOK <!!!>\n")

let main: unit = 
  print_string "====================== TESTS ======================\n";
  flush stdout;
  exec_test "SPLIT_STR" test_split;
  exec_test "PARSE_SINGLE_TIDE_DATA" test_parse;
  exec_test "PARSE_MULTIPLE_TIDE_DATA_LINES" test_parse_list;
  exec_test "REORDER_DATA" test_reorder_data;

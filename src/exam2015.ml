(* q1.1 *)
let rec split_lines (lines: (string list)) : ((string list) list) =
    match lines with
        | [] -> []
        | line::lines -> (Str.split (Str.regexp " ") line)::(split_lines lines)


type tide_data = {
    date: string;
    hmam: string;
    hmpm: string;
    bmam: string;
    bmpm: string;
}

let to_tide_data (line: string list) : tide_data = 
    match line with
        | [d; hm1; hm2; bm1; bm2] -> { date=d; hmam = hm1; hmpm = hm2; bmam = bm1; bmpm = bm2}
        | _ -> failwith "Bad line format"

let to_tide_datas (lines: (string list) list) : tide_data list = List.map to_tide_data lines

let reorder_data (data: tide_data) : string = 
    if (compare data.bmam data.hmam) < 0 then
        Printf.sprintf "%s %s(BM) %s(HM) %s(BM) %s(HM)\n" data.date data.bmam data.hmam data.bmpm data.hmpm 
    else
        Printf.sprintf "%s %s(BM) %s(HM) %s(BM) %s(HM)\n" data.date data.hmam data.bmam data.hmpm data.bmpm

let reorder_datas(datas: tide_data list) : string list = List.map reorder_data datas

let reorder_lines(lines: string list) : string list = reorder_datas (to_tide_datas (split_lines lines))

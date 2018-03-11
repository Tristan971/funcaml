let read_file (path: string) : (string list) =
    let channel = open_in path in
    let rec read_lines (channel: in_channel) (lines:(string list)) : (string list) =
        try
            let line = input_line channel in
            read_lines channel (line::lines)
        with e -> 
            lines
    in 
    let res = read_lines (channel) [] in
    close_in_noerr channel;
    res

let join (list: (string list)) (joining_string: string) = 
    let rec acc (elem: string) (prev: string) : string = prev ^ elem ^ joining_string
    in List.fold_right acc list ""

let main : unit = print_string (join (read_file "test.txt") "\n")

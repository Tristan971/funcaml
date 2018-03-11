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

let write_string_to_file (path: string) (content: string) : unit = 
    let channel: out_channel = open_out path in
    try
        output_string channel content;
        close_out channel
    with e -> 
        close_out_noerr channel

let write_lines_to_file (path: string) (lines: string list) =
    let channel = open_out path in
    List.iter (output_string channel) lines;
    close_out channel

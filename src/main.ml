include String

let xor (x:bool) (y:bool) : bool = 
  if x then not y 
  else y

(*let rec pow x n: int =
  if n < 0 then failwith "Exponent should be nonnegative"
  else if n == 0 then 1
  else x * pow x (n-1)*)

let pow (x:int) (n:int) : int =
  let rec pow_impl (x:int) (n:int) =
    if n == 0 then 1
    else x * pow_impl x (n-1)
  in
  if n < 0 then failwith "Exponent should be nonnegative"
  else pow_impl x n

let succ (x:int) : int = x+1

let len_str (str: string) : int = String.length(str)

let shift_char (c: char) (n: int) : char = char_of_int(int_of_char(c) + n mod 255)

let code_cesar (str: string) (d: int) : string = map(fun (c: char) -> shift_char c d) str

let code_cesari_enc (str: string) : string = mapi(fun (i: int) (c: char) -> shift_char c i) str

let code_cesari_dec (str: string) : string = mapi(fun (i:int) (c: char) -> shift_char c (-i)) str

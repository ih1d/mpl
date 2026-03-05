(* Humberto writes *)
use "auto" (* automatically selects GPU/CPU of available to computer *)
type option a = some a | none

let effect na = string -> int -> a

let handler skip_na
    | return x -> some x
    | na col row k -> none

(* bioinformatics write *)
let df = read_csv "file.csv"

let result = df |> skip_na |> select ["id", "seq"]
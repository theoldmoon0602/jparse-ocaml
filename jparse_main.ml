open Jparse

let read_hole_file f =
    let chan = open_in f in
    let len = in_channel_length chan in
    let s = really_input_string chan len in
    close_in chan;
    s

let hashtbl_map f tbl =
    Hashtbl.fold (fun k v acc -> (f k v) :: acc) tbl []

let rec string_of_json depth json = 
    let string_of_json_obj_item depth k v =
        Printf.sprintf "\"%s\": %s" k (string_of_json (depth+1) v)
    in
    let tab_str depth = String.make (2*depth) ' ' in

    match json with
    |Null -> "null"
    |Boolean(true) -> "true"
    |Boolean(false) -> "false"
    |Number(x) -> string_of_int x
    |Floating(x) -> string_of_float x
    |String(x) -> "\""^x^"\""
    |Array(xs) -> "[" ^ (String.concat ", " (List.map (string_of_json (depth+1)) xs)) ^ "]"
    |Object(tbl) -> "{\n" ^
                    (String.concat ",\n"
                        (hashtbl_map
                            (fun k v -> (tab_str depth) ^ (string_of_json_obj_item (depth+1) k v))
                            tbl)) ^
                    "\n" ^ (tab_str (depth-1)) ^ "}"

let () =
    if Array.length Sys.argv <= 1
    then Printf.printf "usage: %s <json_file>" (Sys.argv.(0))
    else
        try
            let f = Sys.argv.(1) in
            let jsonstr = read_hole_file f in
            let json = parse_json jsonstr in
            print_endline (string_of_json 1 json)
        with 
        |Parse_error s -> print_endline s
        |Sys_error s -> print_endline s




type json_type = 
    | Null
    | Boolean of bool
    | Number of int
    | Floating of float
    | String of string
    | Array of json_type list
    | Object of (string, json_type) Hashtbl.t

type parse_info = {
    mutable i: int;
    str: string;
}

exception Parse_error of string
exception Arienai_error

let rec ignore_space info =
    if String.length info.str <= info.i then ()
    else match info.str.[info.i] with
    |' '|'\t'|'\n'|'\r' -> (info.i <- info.i+1; ignore_space info)
    |_ -> ()

let match_str info s =
    begin
        ignore_space info;
        let slen = String.length s in
        if String.length info.str < info.i + slen then false
        else if String.sub info.str info.i slen = s then (info.i <- info.i + slen; true)
        else false
    end

let rec parse_number info = 
    let rec read_numstr () =
        if String.length info.str <= info.i then ""
        else 
            let c = info.str.[info.i] in
            match c with
            |'0'..'9' -> (info.i<-info.i+1; (Char.escaped c) ^ (read_numstr ()))
            |_ -> "" in
    if String.length info.str <= info.i then 
        raise (Parse_error ("expected number literal at:"^(string_of_int(info.i))))
    else
        if info.str.[info.i] = '-'
        then
            begin
                info.i <- info.i+1; 
                match parse_number info with
                |Number(x) -> Number(-x)
                |Floating(x) -> Floating(-.x)
                |_-> raise Arienai_error
            end 
        else
            let numstr = read_numstr () in
            if String.length info.str <= info.i then Number(int_of_string numstr)
            else
                match info.str.[info.i] with
                    |'.' -> begin 
                                info.i <- info.i+1;
                                Floating(float_of_string (numstr^"."^(read_numstr ())))
                            end
                    |_ -> Number(int_of_string numstr)

let parse_string info =
    let rec read_string () = 
        if String.length info.str <= info.i
        then raise (Parse_error ("unclosed string at:"^(string_of_int(info.i))))
        else
            let c = info.str.[info.i] in
            if c == '"' then (info.i<-info.i+1; "")
            else
                let r = match c with
                    |'\\' -> begin
                                info.i <- info.i+1;
                                if String.length info.str <= info.i
                                then raise (Parse_error ("unclosed string at:"^(string_of_int(info.i))))
                                else
                                    let c2 = info.str.[info.i] in
                                    match c2 with
                                    |'n' -> "\n"
                                    |'"' -> Char.escaped '"'
                                    |'\\' -> "\\"
                                    |_ -> "\\"^(Char.escaped c2)
                             end
                    |_ -> Char.escaped c in
                (info.i <- info.i+1; r^(read_string()))
    in
    if String.length info.str <= info.i || info.str.[info.i] <> '"' then 
        raise (Parse_error ("expected string literal at:"^(string_of_int(info.i))))
    else (info.i<-info.i+1; String(read_string()))

let rec parse_array info =
    let rec read_array () =
        let json_item = parse info in
        if match_str info "," then json_item :: read_array ()
        else if match_str info "]" then  json_item ::[]
        else raise (Parse_error (", or ] is expected at:"^(string_of_int(info.i))))
    in
    if String.length info.str <= info.i || info.str.[info.i] <> '[' then 
        raise (Parse_error ("expected array literal at:"^(string_of_int(info.i))))
    else begin
        info.i<-info.i+1;
        if match_str info "]"
        then Array([])
        else Array(read_array())
    end
and parse_object info =
    let rec read_object tbl =
        begin
            ignore_space info;
            let key = match parse_string info with 
            |String(k) -> k
            |_ -> raise (Parse_error ("key string is expected at:"^(string_of_int(info.i)))) in
            if match_str info ":" then
                let value = parse info in
                begin 
                    Hashtbl.add tbl key value;
                    if match_str info "," then read_object tbl
                    else if match_str info "}" then tbl
                    else raise (Parse_error (", or } is expected at:"^(string_of_int(info.i))))
                end
            else
                raise (Parse_error (": is expected at:"^(string_of_int(info.i))))
        end
    in
    if String.length info.str <= info.i || info.str.[info.i] <> '{' then 
        raise (Parse_error ("expected object literal at:"^(string_of_int(info.i))))
    else begin
        info.i<-info.i+1;
        if match_str info "}"
        then Object(Hashtbl.create 1)
        else Object(read_object(Hashtbl.create 1000))
    end

and parse info =
    if String.length info.str <= info.i then raise (Parse_error ("expected token at:"^(string_of_int(info.i))))
    else if match_str info "null" then Null
    else if match_str info "true" then Boolean(true)
    else if match_str info "false" then Boolean(false)
    else begin
        match info.str.[info.i] with
        |'0'..'9'|'-' -> parse_number info
        |'"' -> parse_string info
        |'[' -> parse_array info
        |'{' -> parse_object info
        |_ -> raise (Parse_error ("unexpected character at:"^string_of_int(info.i)))
    end

let parse_json str = 
    let info = {i=0; str=str} in
    let result = parse info in
    begin
        ignore_space info;
        if String.length info.str > info.i then
            raise (Parse_error ("non-json string is detected at:"^string_of_int(info.i)))
        else result
    end

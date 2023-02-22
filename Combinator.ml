type result =
    | Success of string * char list
    | Failure of string * char list
;;

type parser = Parser of (char list -> result);;

let whitespace = [' '; '\n'];;

let parse_char x =
    let inner_parser x queue =
        match queue with
        | [] -> Failure ([x] |> List.to_seq |> String.of_seq, queue)
        | s :: ss ->
            if s = x then
                Success ([s] |> List.to_seq |> String.of_seq, ss)
            else
                Failure ([x] |> List.to_seq |> String.of_seq, queue)
    in
    Parser (inner_parser x)
;;

let parse_string str =
    let inner_parser str queue =
        let rec loop acc ss qq =
            match ss, qq with
            | [], [] -> Success (List.rev acc |> List.to_seq |> String.of_seq, [])
            | [], _ :: _ -> Success (List.rev acc |> List.to_seq |> String.of_seq, qq)
            | _ :: _, [] -> Failure (str, queue)
            | x :: xs, y :: ys ->
                if x = y then
                    loop (x :: acc) xs ys
                else
                    Failure (str, queue)
        in
        let c_str = List.init (String.length str) (String.get str) in
        loop [] c_str queue
    in
    Parser (inner_parser str)
;;

let parse_str_literal =
    let inner_parser queue =
        let rec loop acc qq =
            match qq with
            | [] -> Failure ("closing \"", queue)
            | x :: xs ->
                if x = '"' then
                    Success (List.append (List.rev acc) ['"'] |> List.append ['"'] |> List.to_seq |> String.of_seq, xs)
                else
                    loop (x :: acc) xs
        in
        match queue with
        | [] -> Failure ("\"", queue)
        | x :: xs ->
            if x = '"' then
                loop [] xs
            else
                Failure ("\"", queue)
    in
    Parser (inner_parser)
;;

let parse_int_literal =
    let inner_parser queue =
        let rec loop acc qq =
            match acc, qq with
            | [], [] -> Failure ("digit", queue)
            | _, [] -> Success (List.rev acc |> List.to_seq |> String.of_seq, qq)
            | _, x :: xs ->
                if Char.code x >= Char.code '0' && Char.code x <= Char.code '9' then
                    loop (x :: acc) xs
                else if List.exists (fun a -> x = a) whitespace then
                    if List.length acc > 0 then
                        Success (List.rev acc |> List.to_seq |> String.of_seq, qq)
                    else
                        Failure ("digit", queue)
                else
                    Failure ("digit", queue)
        in
        loop [] queue
    in
    Parser (inner_parser)
;;

let and_combinator pa pb =
    let inner_parser (Parser p_a) (Parser p_b) queue =
        match p_a queue with
        | Success (a, qq) -> (
            match p_b qq with
            | Success (b, qq2) -> Success (String.cat a b, qq2)
            | Failure (b, qq2) -> Failure (String.cat " and " b |> String.cat a, queue)
        )
        | Failure (a, qq) -> Failure (a, queue)
    in
    Parser (inner_parser pa pb)
;;

let or_combinator pa pb =
    let inner_parser (Parser pa) (Parser pb) queue =
        match pa queue with
        | Success (a, b) -> Success (a, b)
        | Failure (a, b) -> pb queue
    in
    Parser (inner_parser pa pb)
;;

let (#~) = and_combinator;;
let (#|) = or_combinator;;

let run_parser (Parser par) queue = par queue;;

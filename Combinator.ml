type 'a result =
    | Success of 'a * char list
    | Failure of 'a * char list
;;

type 'a parser = Parser of (char list -> 'a result);;

let whitespace = [' '; '\n'];;

let run_parser (Parser par) queue = par queue;;

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
                else
                    Success (List.rev acc |> List.to_seq |> String.of_seq, qq)
        in
        loop [] queue
    in
    Parser (inner_parser)
;;

let repeating_sep_combinator p sep =
    let inner_parser queue =
        let rec loop acc queue =
            match run_parser (parse_string sep) queue with
            | Failure (_, qq) -> List.rev acc
            | Success (_, qq) ->
                match run_parser (p) qq with
                | Failure (a, qq2) -> List.rev acc
                | Success (a, qq2) -> loop (a :: acc) qq2
        in
        match run_parser (p) queue with
        | Failure (a, qq) -> Success ("", queue)
        | Success (a, qq) ->
            let ris = loop [a] qq |> List.fold_left (fun a b -> String.cat a b) "" in
            Success (ris, qq)
    in
    Parser (inner_parser)
;;

let and_combinator pa pb =
    let inner_parser queue =
        match run_parser (pa) queue with
        | Success (a, qq) -> (
            match run_parser (pb) qq with
            | Success (b, qq2) -> Success (String.cat a b, qq2)
            | Failure (b, qq2) -> Failure (String.cat " and " b |> String.cat a, queue)
        )
        | Failure (a, qq) -> Failure (a, queue)
    in
    Parser (inner_parser)
;;

let or_combinator pa pb =
    let inner_parser queue =
        match run_parser (pa) queue with
        | Success (a, b) -> Success (a, b)
        | Failure (a, b) -> run_parser (pb) queue
    in
    Parser (inner_parser)
;;

let ignore_left_combinator left right =
    let inner_parser queue =
        match run_parser (left) queue with
        | Failure (a, qq) -> Failure (a, qq)
        | Success (_, qq) -> run_parser (right) qq
    in
    Parser (inner_parser)
;;

let ignore_right_combinator left right =
    let inner_parser queue =
        match run_parser (left) queue with
        | Failure (a, qq) -> Failure (a, qq)
        | Success (a, qq) ->
            match run_parser (right) qq with
            | Failure (b, qq2) -> Failure (b, qq2)
            | Success (_, qq2) -> Success (a, qq2)
    in
    Parser (inner_parser)
;;

let (#~) = and_combinator;;
let (#|) = or_combinator;;
let (#~>) = ignore_left_combinator;;
let (#<~) = ignore_right_combinator;;
let (#*) = repeating_sep_combinator;;

let skip_whitespace =
    let inner_parser queue =
        let rec loop qq =
            match qq with
            | [] -> Success ("", qq)
            | x :: xs ->
                if List.mem x whitespace then
                    loop xs
                else
                    Success ("", qq)
        in
        loop queue
    in
    Parser (inner_parser)
;;

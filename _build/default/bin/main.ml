open List
open String

let keywords = ["if"; "elif"; "else"]
let operators = ["+"; "-"; "=="; "="; "<"; "<="]
let all_symbols = operators @ ["("; ")"; " "]

type lexeme_type = 
        | OpenParen
        | CloseParen
        | Space
        | Identifier of string
        | Keyword of string
        | Operator of string

type token = { lexeme_type : lexeme_type; line_number : int }

let to_error_string (t : token) err =
        err ^ " (line " ^ (string_of_int t.line_number) ^ ")"

(* removes all duplicates from a list *)
let distinct ls = 
        List.fold_left 
                (fun accum elem ->
                        if List.mem elem accum then
                                accum
                        else
                                elem :: accum)
                []
                ls
(* trigger division is a list of chars at the start of each operator/symbol *)
let trigger_division = 
        distinct (List.map (fun str -> String.get str 0) all_symbols)  

(* takes one lexeme and generates a token *)
let token_from_lexeme lexeme line_number = 
        let data = 
                if List.mem lexeme keywords then
                        Keyword lexeme
                else if List.mem lexeme operators then
                        Operator lexeme
                else if lexeme = "(" then
                        OpenParen
                else if lexeme = ")" then
                        CloseParen
                else if lexeme = " " then
                        Space
                else 
                        Identifier lexeme
        in { lexeme_type = data; line_number}

(* Given a string and some chars, finds the index of the first char which occurs, returning None if none are found. offset is added to the return value. *)
let rec find_first_offset str chars offset =
        if String.length str = 0 then
                None
        else if List.mem (String.get str 0) chars then
                Some offset
        else
                find_first_offset (String.sub str 1 (String.length str - 1)) chars (offset + 1)

let find_first str chars = find_first_offset str chars 0 

(* Given a string and a list of strings, this will find the largest string in the list which is a substring (starting at index 0) of the given string, returning "" if no substring works. *)
let rec find_longest_recursive word substrings =
        let longest = List.hd substrings
        and longest_length = String.length (List.hd substrings)
        in
                if ((String.length word) >= longest_length) && (longest = String.sub word 0 longest_length) then
                        longest
                else if List.length substrings > 1 then
                        find_longest_recursive word (List.tl substrings)
                else
                        ""

let find_longest word substrings = 
        find_longest_recursive 
                word 
                (List.sort 
                        (fun s1 s2 -> 
                                String.length s2 - String.length s1)
                        substrings)

(* divides a word into a list of lexemes *)
let rec divide_word word = 
        match find_first word trigger_division with
        | Some index -> 
                if index = 0 then
                        let operator_length = String.length (find_longest word all_symbols) in
                                List.cons
                                        (String.sub word 0 operator_length)
                                        (divide_word (String.sub word operator_length (String.length word - operator_length)))
                else 
                        List.cons
                                (String.sub word 0 index)
                                (divide_word (String.sub word index (String.length word - index)))
        | None -> if String.length word = 0 then [] else [word]

let tokenize_word word line_number = List.map (fun x -> token_from_lexeme x line_number) (divide_word word)

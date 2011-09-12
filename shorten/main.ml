open Util
open Shorten

let max_length = 1000

let read filename =
  let file_in = open_in filename in
  let rec aux acc =
    try aux (input_line file_in :: acc) with End_of_file -> close_in file_in; acc
  in
  aux [] |> List.filter ((<>) "") |> List.remove_duplicated identity |> List.rev

let add table ss =
  match List.filter ((<>) "") ss
    |> List.map delete_longcut
    |> List.sort (fun s1 s2 -> compare (String.length s1) (String.length s2))
  with
    [] -> ()
  | hd :: tl ->
      let aux s =
        let len = String.length
        and (short, long) = delete_shared hd s in
        if is_included short long && len short < len long && len long <= max_length then
	  let open Hashtbl in
          try
            if len short < len (find table long) then
              replace table long short
          with Not_found ->
            add table long short
      in
      List.iter aux tl

let iter f = List.iter (fun n ->
  let filename = "/home/t2ladmin/workspace/devquiz/2011/puzzle/answers/" ^ string_of_int n ^ ".txt" in
  f filename
) (List.range 1 5000 1)

let make_converter () =
  let converter = Hashtbl.create 100 in
  iter (fun filename -> read filename |> add converter);
  converter

let rec apply converter s =
  let len = String.length s in
  let rec aux l n = (* search in ascending order wrt n and decsending order wrt l *)
    if l < 3 then s
    else if n + l > len then aux (l - 1) 0
    else if Hashtbl.mem converter (String.sub s n l) then
      let converted = String.sub s 0 n
        ^ Hashtbl.find converter (String.sub s n l)
        ^ String.sub s (n + l) (len - n - l)
      in
      apply converter converted
    else aux l (n + 1)
  in
  aux (min max_length len) 0

let sole_filename name =
  let open Str in
  let e = regexp ".*/\\([^/]+\\)$" in
  if string_match e name 0 then
    matched_group 1 name
  else invalid_arg "sole_filename"


let _ =
  let converter = make_converter () in
  let f filename =
    let out = open_out ("/home/t2ladmin/workspace/devquiz/2011/puzzle/answers_shortened/" ^ sole_filename filename) in
    let _ =
      try
        read filename |> List.map (apply converter) |>
            List.find_min (String.length) |> output_string out
      with
        Invalid_argument _ -> ()
    in
    close_out out
  in
  iter f


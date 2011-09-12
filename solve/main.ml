open Util
open Solve

let num = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1

let file = open_in "/home/t2ladmin/workspace/devquiz/2011/puzzle/honban.txt"

let line () = input_line file

let limits s = (* lx, rx, ux, dx *)
  let open List in
  let l = Str.split (Str.regexp " ") s |> map int_of_string in
  nth l 0, nth l 1, nth l 2, nth l 3

let print_result x = List.map dir_to_string x |> String.concat "" |> print_endline

(* initialization *)
let limits = line () |> limits
let n = line () |> int_of_string
let _ = for i = 1 to num - 1 do ignore(line ()) done (* skip *)
let init = input (line ())

let masks = make_masks init

let _ = Printf.eprintf "n : %d\n#masks : %d\n" num (List.length masks)

let rec main = function
    [] -> ()
  | hd :: tl -> try
      let init' = mask init hd in
      prerr_newline (); perr init';
      match solve init' with
        None -> raise Not_found
      | Some dirs ->
          match solve (apply init dirs) with
            None -> raise Not_found
          | Some dirs' -> Printf.eprintf "Found\n"; dirs @ dirs' |> print_result; main tl
    with
      _ -> main tl

let _ = if Array.length Sys.argv > 1 then main masks else ()

open Util

let num = 5000

let get_answer n =
  try
    let file = open_in ("/home/t2ladmin/workspace/devquiz/2011/puzzle/answers_shortened/" ^ string_of_int n ^ ".txt") in
    let rec read acc =
      try
        let line = input_line file in
        read (line :: acc)
      with End_of_file -> acc
    in
    let r = read [] in
    close_in file;
    List.filter ((<>) "") r
    |> fun s -> try List.find_min String.length s with Invalid_argument "find_max_with" -> ""
  with Sys_error _ -> ""

let input () =
  let answers = Array.make num (0, "") in
  for i = 1 to num do
    answers.(i - 1) <- i, get_answer i
  done;
  Array.to_list answers

let count s =
  let rec aux (l, r, u, d) n =
    if n < 0 then (l, r, u, d)
    else match s.[n] with
      'L' -> aux (l + 1, r, u, d) (n - 1)
    | 'R' -> aux (l, r + 1, u, d) (n - 1)
    | 'U' -> aux (l, r, u + 1, d) (n - 1)
    | 'D' -> aux (l, r, u, d + 1) (n - 1)
    | _ -> invalid_arg "count"
  in
  aux (0, 0, 0, 0) (String.length s - 1)

let add (a, b, c, d) (e, f, g, h) = (a + e, b + f, c + g, d + h)

let add_until_max (lm, rm, um, dm) answers =
  let answers =
    List.filter (fun ans -> snd ans <> "") answers
 |> List.sort (fun ans1 ans2 -> compare (String.length (snd ans1)) (String.length (snd ans2)))
  and is_ok (l, r, u, d) = l <= lm && r <= rm && u <= um && d <= dm in
  let rec aux acc_dirs acc_ans = function
      [] -> acc_ans
    | hd :: tl ->
        let added = add acc_dirs (count (snd hd)) in
        if is_ok added then aux added (hd :: acc_ans) tl
        else acc_ans
  in
  aux (0, 0, 0, 0) [] answers

let max = 72187, 81749, 72303, 81778

let out_answer () =
  let answers = input () |> add_until_max max
  and answers_array = Array.make num "" in
  List.iter (fun (n, s) -> answers_array.(n - 1) <- s) answers;
  Array.iter print_endline answers_array

let count_whole () =
  input () |> List.filter (fun ans -> snd ans <> "")
 |> List.map (fun ans -> snd ans |> count)
 |> List.fold_left add (0, 0, 0, 0)

let nums_above len =
  input () |> List.filter (fun ans -> String.length (snd ans) >= len) |> List.map fst

let not_answered () =
  input () |> List.filter (fun ans -> snd ans = "") |> List.map fst

let num_answered () =
  num - (not_answered () |> List.length)

let _ = out_answer ()

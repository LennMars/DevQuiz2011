open Util

type state = int * int * int * int array

let debug = ref false

let closed_max_length = 20000 (* 900 *)

type dir = L | R | U | D
exception Invalid_flip

let input s =
  let x = Str.split (Str.regexp ",") s in
  let w, h = int_of_string (List.nth x 0), int_of_string (List.nth x 1)
  and b = List.nth x 2 (* and dirs = if List.length x = 3 then [] else List.nth x 3 |> string_to_dirs  *) in
  let a = Array.make (w * h) 0
  and z = ref 0 in
  let _ = for i = 0 to w * h - 1 do
    if b.[i] = '0' then z := i;
    let p = try
      int_of_string (String.sub b i 1)
    with
      _ -> let x = Char.code b.[i] - 55 in if x = 6 then 36 else x
    in
    a.(i) <- p
  done in
  (w, h, !z, a)

let get_size (w, h, _, _) = w, h

let is_right i p = i = p - 1

let is_piece p = 0 <> p && p < 36

let opposite = function
    L -> R
  | R -> L
  | U -> D
  | D -> U

let dir_to_string = function
    L -> "L"
  | R -> "R"
  | U -> "U"
  | D -> "D"

let char_to_dir = function
    'L' -> L
  | 'R' -> R
  | 'U' -> U
  | 'D' -> D
  | _ -> invalid_arg "char_to_dir"

let pos (w, _) i =
  let hi = i / w in
  i - hi * w, hi

let is_wall d w h i = match d with
    L -> i mod w = 0
  | R -> (i + 1) mod w = 0
  | U -> i < w
  | D -> i >= w * (h - 1)

let flip d (w, h, z, a) =
  if is_wall d w h z then raise Invalid_flip
  else
    let z' = match d with
        L -> z - 1 | R -> z + 1 | U -> z - w | D -> z + w in
    if a.(z') = 36 then raise Invalid_flip
    else
      let a' = Array.copy a in
      (w, h, z', (Array.swap z z' a'; a')) (* not checked *)

let apply = List.fold_left (swap_arg flip)

let mask (w, h, z, a) ps =
  let aux p =
    if p = 36 || List.exists ((=) p) ps then p
    else 37
  in
  (w, h, z, Array.map aux a)

let fix (w, h, z, a) ps =
  let aux i p =
    if List.exists ((=) p) ps then
      if is_right i p then 36 (* wall *)
      else invalid_arg "fix"
    else p
  in
  (w, h, z, Array.mapi aux a)

let fill_isolated (w, h, _, a) mask = (* problem : filling may make another isolation *)
  let is_isolated i =
    if List.exists ((=) (i + 1)) mask then false (* to be wall *)
    else begin (* remain to be piece *)
      let num_exit = ref 4 in
      let check dir move =
        if (is_wall dir w h i) || a.(i + move) = 36 || List.exists ((=) (i + move + 1)) mask then
          decr num_exit
      in
      check L (-1); check R 1; check U (-w); check D w;
      if !num_exit <= 1 then true else false end
  in
  List.fold_left (fun m i -> if is_isolated i then (i + 1) :: m else m) mask (List.range 0 (w * h - 2) 1)

let is_included a b x = (a <= x && x <= b) || (b <= x && x <= a)

let make_masks1 ((w, h, _, a) as x) =
  let is_in_rect corner1 corner2 p =
    let pos = pos (w, h) in
    let w1, h1 = pos corner1 and w2, h2 = pos corner2 and wi, hi = pos (p - 1) in
    is_included w1 w2 wi && is_included h1 h2 hi
  in
  let aux c1 c2 = List.filter (is_in_rect c1 c2) (List.range 1 (w * h - 1) 1) in
  List.map (fun c1 -> List.map (aux c1) (List.range 0 (w * h - 1) 1))
    [0; w - 1; w * (h - 1)] (* without lower right *)
  |> List.flatten |> List.map (fill_isolated x)
  |> List.map (List.sort compare) |> List.remove_duplicated identity

let make_masks (w, h, z, a) =
  make_masks1 (w, h, z, a)


let show (w, h, z, a) =
  let to_c p =
    if p = 0 then ' '
    else if p <= 9 then char_of_int (p + 48)
    else if p <= 35 then char_of_int (p + 55)
    else if p = 36 then '='
    else if p = 37 then '.'
    else invalid_arg "p"
  and ans = String.create ((w + 1) * h - 1) in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      ans.[i * (w + 1) + j] <- to_c a.(i * w + j)
    done;
    if i < h - 1 then ans.[i * (w + 1) + w] <- '\n'
  done;
  ans

let p x = show x |> print_endline
let perr x = show x |> prerr_endline

let eval_manhattan (w, h, z, a) = (* returns zero only if it is the solution *)
  let manhattan i j =
    let (wi, hi), (wj, hj) = pos (w, h) i, pos (w, h) j in
    abs (hi - hj) + abs (wi - wj)
  in
  let aux i p =
    if p >= 36 || p = 0 then 0 (* wall, wildcard or space *)
    else manhattan i (p - 1)
  in
  Array.mapi aux a |> Array.fold_left (+) 0

let eval_linear_conflict (w, h, z, a) =
  let is_righter p1 p2 =
    p1 < p2
    && p2 <= (p1 + w - 1) / w * w
    && p2 <> w * h
  and is_lower p1 p2 =
    p1 < p2
    && (p2 - p1) mod w = 0
    && p2 < w * h
  and add = List.fold_left (fun xs i -> if is_piece a.(i) then a.(i) :: xs else xs) [] in
  let uppers i = add (List.range (i - w) 0 (-w))
  and lefters i = add (List.range (i - 1) (i / w * w) (-1)) in
  Array.mapi (fun i p -> if is_piece p then List.count (is_righter p) (lefters i) + List.count (is_lower p) (uppers i) else 0) a
 |> Array.fold_left (+) 0

let eval_wildcards (w, h, _, a) =
(*   let is_not_masked p = Array.fold_left (fun is p' -> if p = p' then true else is) false a in *)
  let is_not_masked p = Array.exists ((=) p) a in
  Array.mapi (fun i p -> if p = 37 && is_not_masked (i + 1) then 1 else 0) a
 |> Array.fold_left (+) 0

let eval s = eval_manhattan s + 2 * eval_linear_conflict s

let answer (w, h, _, a) =
  let a' = Array.copy a
  and alist = Array.to_list a in
  let aux x =
    if x = 0 then a'.(w * h - 1) <- 0
    else if List.exists ((=) x) alist then a'.(x - 1) <- x
    else if a.(x - 1) = 36 then ()
    else a'.(x - 1) <- 37
  in
  List.iter aux (List.range 0 (w * h - 1) 1);
  (w, h, w * h - 1, a')


module State : Heap.OrderedType with type t = state * int * int * dir = struct
  type t = state * int * int * dir (* state, depth, score *)
  let compare (x, _, _, _) (y, _, _, _) = compare (eval x) (eval y)
end
module Opens = Heap.Make(State)

module State' : Hashtbl.HashedType with type t = state = struct
  type t = state
  let equal = (=)
  let hash = Hashtbl.hash
end
module Closed = Hashtbl.Make(State')

let astar eval init =
  let opens = Opens.singleton (init, 0, eval init, L) (* imperative *)
  and closed = Closed.create 100 (* imperative *)
  in
  let rec aux () =
    if Opens.is_empty opens || Closed.length closed > closed_max_length then None (* not found *)
    else begin
      let (xstate, xdepth, xscore, xdir) = Opens.remove opens in
      if !debug then (print_endline "step start :"; p xstate; print_newline ());
      Closed.replace closed xstate (xscore, xdir);
      if xscore = 0 then Some closed (* found *)
      else
        let dirs = [L;R;U;D]
        and step dir = try
          let next = flip dir xstate in
          let next_score = eval next in
          if !debug then p next;
          if Closed.mem closed next then begin
            if !debug then Printf.printf "exists. ";
            let old_score = Closed.find closed next |> fst in
            if next_score < old_score then begin
              if !debug then Printf.printf "replace.\n";
              Opens.insert opens (next, xdepth + 1, next_score, dir);
              Closed.remove closed next end
            else
              (if !debug then print_newline ()) end
          else
            Opens.insert opens (next, xdepth + 1, next_score, dir)
        with Invalid_flip -> ()
         in
         List.iter step dirs;
         aux ()
     end
   in
   aux ()

let trace init = function
    None -> None
  | Some x ->
      let last_state, last_dir =
        let find state (score, dir) a = if score = 0 then state, dir else a in
        Closed.fold find x ((0, 0, 0, [||]), D)
      in
      let rec aux (state, dir) acc =
        if state = init then Some (List.tl acc) (* remove dummy *)
        else
          let state = flip (opposite dir) state in
          let dir = Closed.find x state |> snd in
          aux (state, dir) (dir :: acc)
      in
      aux (last_state, last_dir) [last_dir]

let solve init =
  let x = astar eval init in
  trace init x

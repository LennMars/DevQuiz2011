open Util

let opposite = function
    'D' -> 'U'
  | 'U' -> 'D'
  | 'L' -> 'R'
  | 'R' -> 'L'
  | _ -> invalid_arg "opposite"

let find s =
  let rec aux acc n =
    if n >= String.length s - 1 then acc
    else aux (if s.[n + 1] = opposite s.[n] then n :: acc else acc) (n + 1)
  in
  aux [] 0

let paint_longcut s n =
  let rec aux len =
    let l, r = n - len, n + len + 1 in
    if l < 0 || r >= String.length s then ()
    else if s.[l] = '_' || s.[r] = '_' then ()
    else if s.[l] = opposite s.[r] then
      (s.[l] <- '_'; s.[r] <- '_'; aux (len + 1))
    else ()
  in
  aux 0;
  s

let extract s =
  let len = ref 0 in
  String.iter (fun c -> if c <> '_' then incr len) s;
  let s' = String.make !len ' ' in
  let i = ref 0 in
  String.iter (fun c -> if c <> '_' then (s'.[!i] <- c; incr i)) s;
  s'


let delete_longcut s =
  let s = String.copy s in
  List.fold_left paint_longcut s (find s) |> extract


let delete_shared s1 s2 =
  if s1 = s2 then "", "" else
    let open String in
    let len1, len2 = length s1, length s2 in
    let rec aux1 n =
      if s1.[n] = s2.[n] then aux1 (succ n)
      else n
    and aux2 n =
      if s1.[len1 - n - 1] = s2.[len2 - n - 1] then aux2 (succ n)
      else n
    in
    let l, r = aux1 0, aux2 0 in
    sub s1 l (len1 - l - r), sub s2 l (len2 - l - r)

module Place = struct
  type t = int * int
  let compare = compare
end

module S = Set.Make(Place)

let travel_range s =
  let rec aux acc (x, y) n =
    if n >= String.length s then acc else
      let next = match s.[n] with
          'L' -> (x - 1, y)
        | 'R' -> (x + 1, y)
        | 'D' -> (x, y - 1)
        | 'U' -> (x, y + 1)
        | _ -> invalid_arg "travel_range"
      in
      aux (S.add next acc) next (n + 1)
  in
  aux S.empty (0, 0) 0

let is_included s1 s2 = S.subset (travel_range s1) (travel_range s2)


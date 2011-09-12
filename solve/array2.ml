open Util

let width_log = 21 (* 21 *)
let ceiling_log = 10 (* 10 *)

let width = int_exp 2 width_log
let ceiling = int_exp 2 ceiling_log

type 'a t = int ref * 'a * 'a array array

let length (m, _, _) = !m

let height n =
  if n <= 0 then 0 else
(*     int_exp 2 (minimum_bigger_power_of_two (pred n / width) + 1) *)
    (n - 1) / width + 1

let make n x =
  let a = Array.make ceiling (Array.make 0 x) in
  for h = 0 to height n - 1 do
    a.(h) <- Array.make width x
  done;
  ref n, x, a

let clean (m, _, a) =
  for h = 0 to height !m - 1 do
    a.(h) <- [||]
  done;
  m := 0

let pos n =
  let h = n lsr width_log in
  let w = n - h * width in
  h, w

let get (m, _, a) n =
  if n < 0 || n >= !m then invalid_arg "get"
  else
    let h, w = pos n in
    a.(h).(w)

let set (m, x, a) n y =
  if n < 0 then invalid_arg "set"
  else
    let hn, hm = min (height (n + 1)) ceiling, height !m in
    if hn > hm then
      for h = hm to hn - 1 do
        a.(h) <- Array.make width x
      done;
    let h, w = pos n in
    a.(h).(w) <- y;
    m := max !m (n + 1)

let swap m n a =
  let x = get a m in
  set a m (get a n);
  set a n x

let init n f = ()
let sub (m, _, a) start len = ()
let copy (m, _, a) = ()
let iter f (m, _, a) = ()
let map f (m, _, a) = ()

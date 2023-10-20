type 'a t = { before : 'a list; after : 'a list; rnd : Random.State.t}

exception Empty
let empty = { before = []; after = []; rnd = Random.State.make_self_init ()}
let empty_init i = { empty with rnd = Random.State.make i}

let from_list ?init:(init = [||]) xs = 
  if Array.length init = 0 then
    { before = []; after = xs; rnd = Random.State.make_self_init()}
  else 
    { before = []; after = xs; rnd = Random.State.make init}

let push x z = { z with after = x :: z.after}
let rotate1 z = match z.after with
  | x :: [] -> { z with after = List.rev(x :: z.before); before = []}
  | x :: xs  -> { z with after = xs; before = x :: z.before }
  | _ -> { z with after = List.rev z.before; before = [] }

let rec rotaten n z = match n with 
  | 0 -> z
  | _ -> rotaten (n - 1) (rotate1 z) 

let peek z = match z with
  | { after = []; _ } -> raise Empty
  | { after = x :: _; _ } -> x

let pop z = match z with
  | { after = _ :: []; _ } -> { z with after = List.rev z.before; before = []}
  | { after = _ :: xs ; _ } -> { z with after = xs } 
  | _ -> z (* For an empty rotator, just do nothing *)

let is_empty { after ; _ } = List.length after = 0

let length z = List.length z.before + List.length z.after

let rotate_rnd z = 
  let l = length z in 
    if l >= 1 then
      let n = Random.State.int z.rnd (length z) in 
      rotaten n z
    else
      z

let rotate_and_peek z = 
  let z' = rotate_rnd z in 
  (z', peek z')

let rotate_and_pop z =
  let z' = rotate_rnd z in
  let v = peek z' in 
  (pop z', v)
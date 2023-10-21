let empty () = 
  Alcotest.check_raises "Can't peek empty" Rotatable.Empty @@ (fun () -> Rotatable.(empty |> peek));
  Alcotest.(check bool) "popping empty is empty" true Rotatable.(empty |> pop |> is_empty);
  Alcotest.(check bool) "empty is empty" true Rotatable.(empty |> is_empty);
  Alcotest.(check bool) "empty rotates to empty" true Rotatable.(empty |> rotate1 |> is_empty)
  
let init_r () = Rotatable.from_list ~init:([|0|]) ["a"; "b"; "c"; "d"; "e"]

let reordering () = 
  let (rap, rotate_and_pop) = Rotatable.(init_r() |> rotate_and_pop) in 
  let r_rotated = Rotatable.(init_r() |> rotate_rnd) in
  Alcotest.(check string) "Rotate and pop can be done in two steps" rotate_and_pop (r_rotated |> Rotatable.peek);
  Alcotest.(check string) "Rotated remainders match" (rap |> Rotatable.peek) Rotatable.(r_rotated |> pop |> peek)

let others () = 
  let rec rpop_n n r =
    if n = 0 then r else 
      let (r, _) = Rotatable.rotate_and_pop r in 
      rpop_n (n - 1) r in
  let initial_length = Rotatable.length @@ init_r () in 
  Alcotest.(check int) "Pop one reduces size by one" (initial_length - 1) Rotatable.(init_r () |> rotate1 |> pop |> length);
  Alcotest.(check int) "Rotate doesn't reduce size" initial_length Rotatable.(init_r () |> rotate1 |> length);
  Alcotest.(check int) "Rotate and peek doesn't reduce size" initial_length Rotatable.(init_r () |> rotate_and_peek |> fst |> length);
  Alcotest.(check int) "Rotate and pop reduces size by one" (initial_length - 1) Rotatable.(init_r () |> rotate_and_pop |> fst |> length);
  Alcotest.(check bool) "Popping all is empty" true Rotatable.(init_r () |> rpop_n initial_length |> is_empty) 

let empty_set = [("empty", `Quick, empty)]
let others_set = [("others", `Quick, others); ("reordering", `Quick, reordering)]

let () = 
  Alcotest.run "Rotatable tests"
    [("Tests on Empty", empty_set); ("Tests on non-Empty", others_set)]
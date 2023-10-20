
let rec full_order r = 
  if Rotatable.is_empty r then () else
    let (r, v) = Rotatable.rotate_and_pop r in 
    print_endline v;
    full_order r

let r = 
  if Array.length Sys.argv > 1 then
    Array.to_list Sys.argv |> List.tl |> Rotatable.from_list
  else
    Rotatable.from_list ["Matt"; "John"; "James"; "Peter"; "Paul"; "Harry"]

let () =  full_order r

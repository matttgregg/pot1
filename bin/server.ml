let options = ref Rotatable.empty
let chosen = ref []
let next c = 
  if List.is_empty c then 
    "" 
  else List.hd c

let reset () = 
  options := !chosen @ (Rotatable.as_list !options) |> Rotatable.from_list;
  chosen := []

let clear () = 
  options := Rotatable.empty;
  chosen := [] 

let demo () = 
  options := Rotatable.from_list ["alpha"; "beta"; "gamma"; "delta"; "epsilon"];
  chosen := [] 

let spin () = 
  (if not @@ Rotatable.is_empty !options then
    let (o, n) = Rotatable.rotate_and_pop !options in 
      options := o; chosen := n :: !chosen)

let putback () =
  (if not @@ (List.is_empty !chosen || Rotatable.is_empty !options) then
    let saved = List.hd !chosen in
    chosen := List.tl !chosen; spin (); options := Rotatable.push saved !options;)


let () =
  Dream.run 
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [
    Dream.get "/reset" (fun r -> reset (); Dream.redirect r "/");
    Dream.get "/clear" (fun r -> clear (); Dream.redirect r "/");
    Dream.get "/demo" (fun r -> demo (); Dream.redirect r "/");
    Dream.get "/next" (fun r -> spin (); Dream.redirect r "/");
    Dream.get "/putback" (fun r -> putback (); Dream.redirect r "/");
    Dream.get "/" (fun r -> Template.render r (next !chosen) options chosen |> Dream.html);
    Dream.post "/add" (fun request ->
      match%lwt Dream.form request with
      | `Ok ["item", item] -> options := (Rotatable.push item !options); Dream.redirect request "/" 
      | _ ->
        Dream.empty `Bad_Request);
  ]
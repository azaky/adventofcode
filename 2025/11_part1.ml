open Core

let parse_line line =
  match String.split line ~on:':' with
  | [ from; tl ] -> (from, String.split (String.drop_prefix tl 1) ~on:' ')
  | _ -> assert false

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:parse_line

let solve adj_list =
  let n = List.length adj_list in
  (* turn to map *)
  let adj = Hashtbl.create (module String) ~size:n in
  let () =
    List.iter adj_list ~f:(fun (from, list) ->
      List.iter list ~f:(fun item -> Hashtbl.add_multi adj ~key:from ~data:item))
  in
  (* dp? *)
  let ways = Hashtbl.create (module String) ~size:n in
  let rec dfs node =
    if String.equal node "out"
    then 1
    else if Hashtbl.mem ways node
    then Hashtbl.find_exn ways node
    else (
      let next = Hashtbl.find_multi adj node in
      let total = List.fold next ~init:0 ~f:(fun acc u -> acc + dfs u) in
      let () = Hashtbl.add_exn ways ~key:node ~data:total in
      total)
  in
  dfs "you"

let _ = get_input "11.txt" |> solve |> Printf.printf "%d\n"

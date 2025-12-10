open Core

let parse_lights lights =
  let () = Printf.printf "lights = '%s'\n" lights in
  String.sub lights ~pos:1 ~len:(String.length lights - 2)
  |> String.to_list
  |> List.map ~f:(fun c -> if Char.equal '#' c then 1 else 0)

let parse_ints ints =
  String.sub ints ~pos:1 ~len:(String.length ints - 2)
  |> String.split ~on:','
  |> List.map ~f:Int.of_string

let parse_line line =
  let () = Printf.printf "line = '%s'\n" line in
  match String.split line ~on:' ' with
  | lights :: tl ->
    (match List.rev tl with
     | joltage :: tl ->
       (parse_lights lights, List.map tl ~f:parse_ints |> List.rev, parse_ints joltage)
     | _ -> assert false)
  | _ -> assert false

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:parse_line

let debug_list list =
  List.iter list ~f:(Printf.printf "%d ")
let debug_array arr =
  Array.iter arr ~f:(Printf.printf "%d ")

let solve (lights, toggles, joltage) =
  let n = List.length joltage in
  (* sort lexicographically *)
  let toggles =
    let rec cmp l1 l2 =
      match l1 with
      | [] -> -1
      | hd1 :: tl1 -> (
        match l2 with
        | [] -> 1
        | hd2 :: tl2 ->
          if hd1 <> hd2 then (Int.compare hd1 hd2)
          else (cmp tl1 tl2)
      )
    in
    List.sort toggles ~compare:cmp
  in
  (* map toggles to include suffix aggregates *)
  let _, toggles =
    toggles
    |> List.rev
    |> List.fold_map ~init:(Array.create ~len:n 0) ~f:(fun acc t ->
      let acc = Array.copy acc in
      let () = List.iter t ~f:(fun i -> acc.(i) <- 1) in
      let () = Printf.printf "t = "; debug_list t; Printf.printf "\n\tacc = "; debug_array acc; Printf.printf "\n"; Out_channel.flush stdout in
      (acc, (t, acc)))
  in
  let toggles = List.rev toggles in
  let best = ref (-1) in
  let update_ans cnt =
    Printf.printf "update_ans %d\n" cnt;
    if !best = -1 || cnt < !best then best := cnt in
  let joltage = List.to_array joltage in
  let rec dfs joltage toggles cnt =
    (* let () =
      Printf.printf "dfs(joltage = ";
      Array.iter joltage ~f:(fun i -> Printf.printf "%d " i);
      Printf.printf ", cnt = %d, rest = %d)\n" cnt (List.length toggles)
    in *)
    match toggles with
    | [] -> if Array.for_all joltage ~f:(Int.equal 0) then update_ans cnt
    | (t, rem) :: tl ->
      (* let () =
        Printf.printf "\trem = ";
        Array.iter rem ~f:(fun i -> Printf.printf "%d " i);
        Printf.printf "\n";
        Printf.printf "\tt = ";
        List.iter t ~f:(fun i -> Printf.printf "%d " i);
        Printf.printf "\n";
      in *)

      (* pruning: check if remaining joltage can cover *)
      if Array.for_all2_exn rem joltage ~f:(fun r j -> j = 0 || (j > 0 && r > 0))
      then (
        let joltage = Array.copy joltage in
        let valid = ref true in
        let cnt = ref cnt in
        while !valid do
          dfs joltage tl !cnt;
          (* apply current toggles one by one *)
          List.iter t ~f:(fun i -> joltage.(i) <- joltage.(i) - 1; if joltage.(i) < 0 then valid := false);
          incr cnt;
          (* let () =
            Printf.printf "\t\tjoltage = ";
            Array.iter joltage ~f:(fun i -> Printf.printf "%d " i);
            Printf.printf ", cnt = %d)\n" !cnt
          in
          () *)
          (* if Array.exists joltage ~f:(fun i -> i < 0) then (
            valid := false;
            Printf.printf "\t\tpruned at cnt = %d\n" !cnt
          ) *)
        done)
      (* else
        Printf.printf "\tpruned\n" *)
  in
  let () = dfs joltage toggles 0 in
  let () = Printf.printf "ans = %d\n" !best in
  !best

let _ =
  get_input "10.txt"
  |> List.map ~f:solve
  |> List.sum (module Int) ~f:Fn.id
  |> Printf.printf "%d\n"

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

let solve (lights, toggles, joltage) =
  let n = List.length lights in
  let dp = Array.init (1 lsl n) ~f:(fun i -> if i = 0 then 0 else -1) in
  let () = List.iteri toggles ~f:(fun idx t ->
    let bit = List.fold t ~init:0 ~f:(fun acc i -> acc lor (1 lsl i)) in
    let () = Printf.printf "toggles:" in
    let () = List.iter t ~f:(fun i -> Printf.printf " %d" i) in
    let () = Printf.printf " (bit = %d)\n" bit in
    for mask = 1 to (1 lsl n) - 1 do
      let prev = (bit lxor mask) in
      if dp.(prev) <> -1 then
        let () = Printf.printf "dp i = %d mask = %d prev = %d\n" idx mask prev in
        if (dp.(mask) = -1) || (dp.(mask) > dp.(prev) + 1) then
          dp.(mask) <- dp.(prev) + 1
    done
  ) in
  let target = List.foldi lights ~init:0 ~f:(fun i acc c -> acc lor (c lsl i)) in
  let () = Printf.printf "target = %d\n" target in
  dp.(target)

let _ =
  get_input "10.txt"
  |> List.map ~f:solve
  |> List.sum (module Int) ~f:Fn.id
  |> Printf.printf "%d\n"

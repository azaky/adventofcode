open Core

let parse_area area =
  Scanf.sscanf area "%dx%d" (fun a b -> (a, b))

let parse_ints ints =
  String.strip ints
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string

let parse_line line =
  try
    match String.split line ~on:':' with
    | [area; list] ->
      Some (parse_area area, parse_ints list)
    | _ -> None
  with
  | _ -> None

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:parse_line
  |> List.filter_opt

let presents_area = [7; 7; 6; 7; 7; 5]

let solve ((w, h), l) =
  let area = w*h in
  let p = List.fold2_exn l presents_area ~init:0 ~f:(fun acc a b -> acc + a * b) in
  let parity = if (List.nth_exn l 0) % 2 = 1 then 1 else 0 in
  let ans = if area >= (p + parity) then 1 else 0 in
  let () = Printf.printf "Case %dx%d: area = %d, p = %d, par = %d: %d\n" w h area p parity ans in
  ans

let _ =
  get_input "12.txt"
  |> List.map ~f:solve
  |> List.sum (module Int) ~f:Fn.id
  |> Printf.printf "%d\n"

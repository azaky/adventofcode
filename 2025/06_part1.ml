open Core

let parse_ops ops =
  String.to_list ops |> List.filter ~f:(fun ch -> not (Char.is_whitespace ch))

let parse_nums nums =
  let stream = Scanf.Scanning.from_string nums in
  let rec parse acc =
    try Scanf.bscanf stream " %d " (fun num -> num :: parse acc) with
    | Scanf.Scan_failure _ -> acc
    | End_of_file -> acc
  in
  parse []

let get_input input_filename =
  let lines =
    In_channel.read_all input_filename
    |> String.strip
    |> String.split ~on:'\n'
    |> List.rev
  in
  match lines with
  | ops :: nums ->
    List.zip_exn (parse_ops ops) (List.map ~f:parse_nums nums |> List.transpose_exn)
  | _ -> assert false

let solve lst =
  List.fold lst ~init:0 ~f:(fun acc (op, num) ->
    acc
    +
    match op with
    | '*' -> List.fold ~init:1 ~f:( * ) num
    | '+' -> List.fold ~init:0 ~f:( + ) num
    | _ -> assert false)

let _ = get_input "06.txt" |> solve |> Printf.printf "%d\n"

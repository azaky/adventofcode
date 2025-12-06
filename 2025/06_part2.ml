open Core

let get_input input_filename =
  let transposed_lines =
    In_channel.read_lines input_filename
    |> List.filter ~f:(String.( <> ) "")
    (* convert into char list list and transpose *)
    |> List.map ~f:String.to_list
    |> List.transpose_exn
  in
  let rec parse acc lines =
    match lines with
    | line :: rest ->
      if List.for_all line ~f:Char.is_whitespace
      then
        (* empty line, continue *)
        parse acc rest
      else (
        let op = List.last_exn line in
        let num =
          String.of_list (List.drop_last_exn line) |> String.strip |> Int.of_string
        in
        if Char.is_whitespace op
        then (
          (* is a sole number, append to the last acc *)
          match acc with
          | (op, nums) :: tl -> parse ((op, num :: nums) :: tl) rest
          | _ -> assert false)
        else
          (* is a number with op, create new item in acc *)
          parse ((op, [ num ]) :: acc) rest)
    | [] -> acc
  in
  parse [] transposed_lines

let solve lst =
  List.fold lst ~init:0 ~f:(fun acc (op, num) ->
    acc
    +
    match op with
    | '*' -> List.fold ~init:1 ~f:( * ) num
    | '+' -> List.fold ~init:0 ~f:( + ) num
    | _ -> assert false)

let _ = get_input "06.txt" |> solve |> Printf.printf "%d\n"

open Core

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun line -> Scanf.sscanf line "%d,%d" (fun a b -> (a, b)))
  |> List.to_array

let area (x1, y1) (x2, y2) =
  let dx = abs (x1 - x2) + 1 in
  let dy = abs (y1 - y2) + 1 in
  dx * dy

let solve points =
  let n = Array.length points in
  let ans = ref 0 in
  let () =
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        ans := max !ans (area points.(i) points.(j))
      done
    done
  in
  !ans

let _ = get_input "09.txt" |> solve |> Printf.printf "%d\n"

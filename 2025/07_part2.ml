open Core

let get_input input_filename =
  In_channel.read_lines input_filename |> List.filter ~f:(String.( <> ) "")

let solve grid =
  match grid with
  | first :: rest ->
    let width = String.length first in
    (* convert first row to ints *)
    let first_cnt =
      String.to_array first |> Array.map ~f:(fun c -> if Char.equal c 'S' then 1 else 0)
    in
    let cnt =
      List.fold rest ~init:first_cnt ~f:(fun prev line ->
        let cur = Array.init width ~f:(fun _ -> 0) in
        let () =
          for i = 0 to width - 1 do
            if Char.equal line.[i] '^'
            then (
              if i > 0 then cur.(i - 1) <- cur.(i - 1) + prev.(i);
              if i + 1 < width then cur.(i + 1) <- cur.(i + 1) + prev.(i))
            else cur.(i) <- cur.(i) + prev.(i)
          done
        in
        cur)
    in
    Array.fold ~init:0 ~f:( + ) cnt
  | _ -> 0

let _ = get_input "07.txt" |> solve |> Printf.printf "%d\n"

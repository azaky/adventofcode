open Core

let empty_line_sep = String.Search_pattern.create "\n\n"
let parse_ranges = List.map ~f:(fun line -> Scanf.sscanf line "%d-%d" (fun a b -> (a, b)))
let parse_ints = List.map ~f:Int.of_string

let get_input input_filename =
    let parts = In_channel.read_all input_filename
        |> String.strip
        |> String.Search_pattern.split_on empty_line_sep
        |> List.map ~f:(String.split_lines)
    in
    match parts with
    | [ranges; ints] -> (parse_ranges ranges, parse_ints ints)
    | _ -> raise Exit

let solve ranges =
    ranges
    (* sort based on first item *)
    |> List.sort ~compare:(fun (a, _) (c, _) -> Int.compare a c)
    |> List.fold
        ~init:(0, (0, -1))
        ~f:(fun (acc, (a, b)) (c, d) ->
            if c > b then
                (acc + (b - a + 1), (c, d))
            else 
                (acc, ((min a c), (max b d)))
        )
    |> (fun (acc, (a, b)) -> acc + b - a + 1)

let () = get_input "05.txt"
    (* for part 2: only pick ranges *)
    |> fst
    |> solve
    |> Printf.printf "%d\n";;

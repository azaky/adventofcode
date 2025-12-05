let get_input input_filename =
    In_channel.with_open_text input_filename In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.fold_left (fun ((ranges_acc, list_acc) as acc, mode) raw_line ->
        let line = String.trim raw_line in
        if mode = 1 then (* single integer *)
            let num = int_of_string line in
            ((ranges_acc, list_acc @ [num]), mode)
        else if line = "" then (* empty line, change mode *)
            (acc, 1)
        else
            let range = Scanf.sscanf line "%d-%d" (fun a b -> (a, b)) in
            ((ranges_acc @ [range], list_acc), mode)
        )
        (([], []), 0)
    |> fst

let solve ranges =
    ranges
    (* sort based on second item *)
    |> List.sort (fun (a, b) (c, d) -> if b = d then (a-c) else (b-d))
    |> List.fold_left
        (fun (acc, (a, b)) (c, d) ->
            if c > b then
                (acc + (b - a + 1), (c, d))
            else 
                (acc, ((min a c), (max b d)))
        )
        (0, (0, -1))
    |> (fun (acc, (a, b)) -> acc + b - a + 1)

let input = get_input "05.txt"
    (* for part 2: only pick ranges *)
    |> fst
    |> solve
    |> Printf.printf "%d\n";;

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


let in_ranges ranges num =
    ranges |> List.exists (fun (a, b) -> a <= num && num <= b)

let solve (ranges, list) =
    List.fold_left (fun acc num -> acc + (if in_ranges ranges num then 1 else 0)) 0 list

let input = get_input "05.txt"
    |> solve
    |> Printf.printf "%d\n";;

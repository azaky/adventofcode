let (ans, last_pos) =
    In_channel.with_open_text "01.txt" In_channel.input_all
    |> String.split_on_char '\n'
    |> List.map
        (fun line -> (line.[0], String.sub line 1 (String.length line - 1) |> int_of_string))
    |> List.fold_left
        (fun (acc, pos) (dir, len) ->
            let d = match dir with
                | 'L' -> -1
                | 'R' -> 1
                | _ -> raise Exit in
            let next_pos = pos + d * len in
            let times = if d = 1 then next_pos / 100
                else if next_pos > 0 then 0
                else (-next_pos) / 100 + (if pos = 0 then 0 else 1) in
            ((acc + times), ((next_pos mod 100) + 100) mod 100)
        )
        (0, 50);;

Printf.printf "%d\n" ans;;

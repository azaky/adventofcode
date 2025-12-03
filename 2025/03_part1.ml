let get_input input_filename =
    In_channel.with_open_text input_filename In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map
        (fun s -> List.init (String.length s) (String.get s) |> List.map (fun c -> (int_of_char c) - 48))

let get_joltage battery =
    let rec max_with list max_first_digit acc =
        match list with
        | [] -> acc
        | hd :: tl -> max_with tl (max max_first_digit hd) (max acc (10 * max_first_digit + hd))
    in
    max_with battery 0 0;;

let () = get_input "03.txt"
    |> List.fold_left (fun acc battery -> acc + (get_joltage battery)) 0
    |> Printf.printf "%d\n";;

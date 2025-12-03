let get_input input_filename =
    In_channel.with_open_text input_filename In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map
        (fun s -> List.init (String.length s) (String.get s) |> List.map (fun c -> (int_of_char c) - 48))

let get_joltage battery t =
    let n = List.length battery in
    let memo = Hashtbl.create (n * t) in
    let rec dp i k =
        if i = 0 || k = 0 then 0
        else
            let hash = n * k + i in
            if Hashtbl.mem memo hash then
                Hashtbl.find memo hash
            else
                let d = List.nth battery (i-1) in
                let mx = max (dp (i-1) k) (10 * (dp (i-1) (k-1)) + d) in
                let () = Hashtbl.add memo hash mx in
                mx
    in
    dp n t

let () = get_input "03.txt"
    |> List.fold_left (fun acc battery -> acc + (get_joltage battery 12)) 0
    |> Printf.printf "%d\n";;

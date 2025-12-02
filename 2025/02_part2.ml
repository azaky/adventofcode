let input_filename = "02.txt"

let ranges = In_channel.with_open_text input_filename In_channel.input_all
    |> String.split_on_char ','
    |> List.map
        (fun p -> Scanf.sscanf p "%d-%d" (fun a b -> (a, b)));;

let in_ranges num =
    ranges |> List.exists (fun (a, b) -> a <= num && num <= b);;

let rec rep num t =
    if t = 1 then
        num
    else
        Printf.sprintf "%d%d" num (rep num (t-1)) |> int_of_string;;

let hash = Hashtbl.create 1;;

let rec sum_rep num t acc =
    let d = rep num t in
    if d < 0 || d > 999999999999 then
        acc
    else
        let found = match Hashtbl.find_opt hash d with
            | None -> false
            | Some _ -> true in
        if found then
            sum_rep num (t+1) acc
        else
            let () = Hashtbl.add hash d true in
            let cur =
                if in_ranges d then
                    let () = Format.printf "%dx%d = %d\n" num t d in
                    d
                else 0
            in
            sum_rep num (t+1) (acc+cur);;

let rec sum_invalid_ids i max acc =
    if i > max then
        acc
    else
        let cur = sum_rep i 2 0 in
        sum_invalid_ids (i+1) max (acc+cur);;

let ans = sum_invalid_ids 1 999999 0;;

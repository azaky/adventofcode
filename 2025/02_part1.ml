let input_filename = "02.txt"

let ranges = In_channel.with_open_text input_filename In_channel.input_all
    |> String.split_on_char ','
    |> List.map
        (fun p -> Scanf.sscanf p "%d-%d" (fun a b -> (a, b)));;

let in_ranges num =
    ranges |> List.exists (fun (a, b) -> a <= num && num <= b);;

let double num = Printf.sprintf "%d%d" num num |> int_of_string;;

let rec sum_invalid_ids i max acc =
    if i > max then
        acc
    else
        let d = double i in
        let cur = if in_ranges d then d else 0 in
        sum_invalid_ids (i+1) max (acc+cur);;

let ans = sum_invalid_ids 1 1000000 0;;

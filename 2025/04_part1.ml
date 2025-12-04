let get_input input_filename =
    In_channel.with_open_text input_filename In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'

let solve grid =
    let n = List.length grid in
    let m = List.nth grid 0 |> String.length in
    let c i j =
        if i < 0 || i >= n || j < 0 || j >= m then 0
        else if (List.nth grid i).[j] = '@' then 1
        else 0
    in
    let adj i j =
        let sum = ref 0 in
        let () =
            for ii = (i-1) to (i+1) do
                for jj = (j-1) to (j+1) do
                    if (not ((ii = i) && (jj = j))) then
                        sum := !sum + (c ii jj)
                done
            done
        in
        !sum
    in
    let sum = ref 0 in
    let () =
        for i = 0 to (n-1) do
            for j = 0 to (m-1) do
                if ((List.nth grid i).[j] = '@') && (adj i j) < 4 then
                    sum := !sum + 1
            done
        done
    in
    !sum

let () = get_input "04.txt"
    |> solve
    |> Printf.printf "%d\n";;

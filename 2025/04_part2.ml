let get_input input_filename =
    In_channel.with_open_text input_filename In_channel.input_all
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map (fun s -> Array.init (String.length s) (String.get s))
    |> Array.of_list

let solve grid =
    let n = Array.length grid in
    let m = Array.length grid.(0) in
    let c i j =
        if i < 0 || i >= n || j < 0 || j >= m then 0
        else if grid.(i).(j) = '@' then 1
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
    let stop = ref false in
    let sum = ref 0 in
    let () =
        while not !stop do
            stop := true;
            for i = 0 to (n-1) do
                for j = 0 to (m-1) do
                    if (grid.(i).(j) = '@') && (adj i j) < 4 then begin
                        sum := !sum + 1;
                        stop := false;
                        grid.(i).(j) <- '.'
                    end
                done
            done
        done
    in
    !sum

let () = get_input "04.txt"
    |> solve
    |> Printf.printf "%d\n";;

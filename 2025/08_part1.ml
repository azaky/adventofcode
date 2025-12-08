open Core

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun line -> Scanf.sscanf line "%d,%d,%d" (fun a b c -> (a, b, c)))
  |> List.to_array

let distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

let solve n_connect n_ans points =
  (* i, j, dist *)
  let n = Array.length points in
  let edges = Array.create ~len:(n * (n-1) / 2) (0, 0, 0) in
  let idx = ref 0 in
  let () =
    for i = 0 to n-2 do
      for j = i+1 to n-1 do
        ( edges.(!idx) <- (i, j, distance points.(i) points.(j)); idx := !idx + 1 )
      done
    done
  in
  let () = Printf.printf "edges created: length = %d\n" (Array.length edges) in
  let () = Array.sort edges ~compare:(fun (_, _, a) (_, _, b) -> a - b) in
  let edges = Array.sub edges ~pos:0 ~len:n_connect in
  (* DSU *)
  (* TODO: create module *)
  let p = Array.init n ~f:(fun i -> i) in
  let rec find i =
    if p.(i) = i then i
    else
      let () = p.(i) <- find p.(i) in
      p.(i)
  in
  let merge a b =
    let pa = find a in
    let pb = find b in
    p.(pa) <- p.(pb)
  in
  (* merge *)
  let () = Array.iter edges ~f:(fun (i, j, _) -> merge i j) in
  (* count occurrences *)
  let occ = Hashtbl.create (module Int) in
  let () = for i = 0 to n-1 do
    let key = find i in
    Hashtbl.update occ key ~f:(fun v ->
      match v with
      | Some c -> c+1
      | None -> 1)
    done
  in
  (* pick top n_ans *)
  Hashtbl.fold occ ~init:([]) ~f:(fun ~key ~data acc -> data :: acc)
    |> List.sort ~compare:(fun a b -> Int.compare b a)
    |> Fn.flip List.take n_ans
    |> List.fold ~init:1 ~f:( * )

(* let _ = get_input "08_sample.txt" |> solve 10 3 |> Printf.printf "%d\n" *)
let _ = get_input "08.txt" |> solve 1000 3 |> Printf.printf "%d\n"

open Core

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun line -> Scanf.sscanf line "%d,%d,%d" (fun a b c -> (a, b, c)))
  |> List.to_array

let distance (x1, y1, z1) (x2, y2, z2) =
  ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)) + ((z1 - z2) * (z1 - z2))

let solve points =
  (* i, j, dist *)
  let n = Array.length points in
  let edges = Array.create ~len:(n * (n - 1) / 2) (0, 0, 0) in
  let idx = ref 0 in
  let () =
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        edges.(!idx) <- (i, j, distance points.(i) points.(j));
        idx := !idx + 1
      done
    done
  in
  let () = Array.sort edges ~compare:(fun (_, _, a) (_, _, b) -> a - b) in
  (* DSU *)
  (* TODO: create module *)
  let p = Array.init n ~f:(fun i -> i) in
  let rec find i =
    if p.(i) = i
    then i
    else (
      let () = p.(i) <- find p.(i) in
      p.(i))
  in
  (* merge returns true if two components are different *)
  let merge a b =
    let pa = find a in
    let pb = find b in
    let () = p.(pa) <- p.(pb) in
    pa <> pb
  in
  (* merge *)
  Array.fold_until edges ~init:n ~finish:Fn.id ~f:(fun n_components (i, j, _) ->
    let merged = merge i j in
    let n_components = n_components - if merged then 1 else 0 in
    if n_components = 1
    then (
      let xi, _, _ = points.(i) in
      let xj, _, _ = points.(j) in
      Stop (xi * xj))
    else Continue n_components)

let _ = get_input "08.txt" |> solve |> Printf.printf "%d\n"

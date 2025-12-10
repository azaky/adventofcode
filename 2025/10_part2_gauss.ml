open Core

let eps = 1.e-9

let parse_lights lights =
  (* let () = Printf.printf "lights = '%s'\n" lights in *)
  String.sub lights ~pos:1 ~len:(String.length lights - 2)
  |> String.to_list
  |> List.map ~f:(fun c -> if Char.equal '#' c then 1 else 0)

let parse_ints ints =
  String.sub ints ~pos:1 ~len:(String.length ints - 2)
  |> String.split ~on:','
  |> List.map ~f:Int.of_string

let parse_line line =
  (* let () = Printf.printf "line = '%s'\n" line in *)
  match String.split line ~on:' ' with
  | lights :: tl ->
    (match List.rev tl with
     | joltage :: tl ->
       (parse_lights lights, List.map tl ~f:parse_ints |> List.rev, parse_ints joltage)
     | _ -> assert false)
  | _ -> assert false

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:parse_line

let debug_list list =
  List.iter list ~f:(Printf.printf "%d ");
  Out_channel.flush stdout

let debug_array arr =
  Array.iter arr ~f:(Printf.printf "%8.2f");
  Out_channel.flush stdout

let debug_matrix a =
  Array.iter a ~f:(fun row ->
    debug_array row;
    Printf.printf "\n";
    Out_channel.flush stdout)

(* 
h := 1 /* Initialization of the pivot row */
k := 1 /* Initialization of the pivot column */

while h ≤ m and k ≤ n:
    /* Find the k-th pivot: */
    i_max := argmax (i = h ... m, abs(A[i, k]))
    if A[i_max, k] = 0:
        /* No pivot in this column, pass to next column */
        k := k + 1
    else:
        swap rows(h, i_max)
        /* Do for all rows below pivot: */
        for i = h + 1 ... m:
            f := A[i, k] / A[h, k]
            /* Fill with zeros the lower part of pivot column: */
            A[i, k] := 0
            /* Do for all remaining elements in current row: */
            for j = k + 1 ... n:
                A[i, j] := A[i, j] - A[h, j] * f
        /* Increase pivot row and column */
        h := h + 1
        k := k + 1
*)

(* https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode *)
let gauss a =
  let h = ref 0 in
  let k = ref 0 in
  let m = Array.length a in
  let n = Array.length a.(0) in
  let () =
    while !h < m && !k < n do
      let imax =
        Sequence.fold
          (Sequence.range ~start:`inclusive ~stop:`exclusive !h m)
          ~init:!h
          ~f:(fun argmax i ->
            if Float.(abs a.(i).(!k) > abs a.(argmax).(!k)) then i else argmax)
      in
      if Float.(abs a.(imax).(!k) < eps)
      then incr k
      else (
        let () =
          let tmp = Array.copy a.(!h) in
          a.(!h) <- Array.copy a.(imax);
          a.(imax) <- tmp
        in
        let p = a.(!h).(!k) in
        Array.iteri a.(!h) ~f:(fun i v -> a.(!h).(i) <- Float.(v / p));
        for i = !h + 1 to m - 1 do
          let f = Float.(a.(i).(!k) / a.(!h).(!k)) in
          a.(i).(!k) <- 0.;
          for j = !k + 1 to n - 1 do
            a.(i).(j) <- Float.(a.(i).(j) - (a.(!h).(j) * f))
          done
        done;
        incr h;
        incr k)
    done
  in
  (* normalize epsilon *)
  let () =
    Array.iteri a ~f:(fun i row ->
      Array.iteri row ~f:(fun j v -> if Float.(abs v < eps) then a.(i).(j) <- 0.))
  in
  a

let solve itc (lights, toggles, joltage) =
  let () =
    Printf.printf "Case %d:\n" itc;
    Out_channel.flush stdout
  in
  let n = List.length joltage in
  let m = List.length toggles in
  let maxv = List.fold joltage ~init:0 ~f:max in
  (* create matrix *)
  let a = Array.make_matrix ~dimx:n ~dimy:(m + 1) 0. in
  let () = List.iteri toggles ~f:(fun i t -> List.iter t ~f:(fun j -> a.(j).(i) <- 1.)) in
  let () = List.iteri joltage ~f:(fun i v -> a.(i).(m) <- Float.of_int v) in
  let () =
    Printf.printf "original matrix:\n";
    debug_matrix a
  in
  let g = gauss a in
  let () =
    Printf.printf "after gauss:\n";
    debug_matrix g
  in
  let ans = ref (-1) in
  let update_ans sol =
    let is_valid =
      Array.for_all a ~f:(fun row ->
        let sum =
          Array.foldi sol ~init:0. ~f:(fun j acc s -> Float.(acc + (s * row.(j))))
        in
        Float.(abs (sum - row.(m)) < eps))
    in
    let sum = Array.sum (module Float) sol ~f:Fn.id in
    let sum = Float.to_int sum in
    if is_valid && (!ans = -1 || sum < !ans)
    then (
      Printf.printf "found better ans: %d\n" sum;
      debug_array sol;
      Printf.printf "\n";
      Out_channel.flush stdout;
      ans := sum)
  in
  (* recursively find solution *)
  let rec dfs sol =
    (* let () =
      Printf.printf "dfs: ";
      debug_array sol;
      Printf.printf "\n"
    in *)
    let isol =
      let isol = ref (-1) in
      let () =
        for i = 0 to m - 1 do
          if Float.(sol.(i) < 0.) then isol := i
        done
      in
      !isol
    in
    if isol < 0
    then update_ans sol
    else (
      (* find last row with isol *)
      let irow =
        let irow = ref (-1) in
        let () =
          for i = 0 to n - 1 do
            if Float.(g.(i).(isol) <> 0.) then irow := i
          done;
          assert (!irow <> -1)
        in
        !irow
      in
      let is_pivot =
        let is_pivot = ref true in
        let () =
          for i = 0 to isol - 1 do
            if Float.(g.(irow).(i) <> 0.) then is_pivot := false
          done
        in
        !is_pivot
      in
      (* if this row is pivot, the value can be determined directly *)
      if is_pivot
      then (
        let v = ref g.(irow).(m) in
        let () =
          for i = isol + 1 to m - 1 do
            v := Float.(!v - (g.(irow).(i) * sol.(i)))
          done
        in
        if Float.(!v >= -eps && abs (round !v - !v) < eps)
        then (
          let sol = Array.copy sol in
          let () = sol.(isol) <- Float.(round !v) in
          dfs sol)
        (* else Printf.printf "found invalid value: %.5f\n" !v;
        Out_channel.flush stdout) *))
      else
        (* otherwise, let's brute force. what's the upper bound? *)
        for v = 0 to maxv do
          let sol = Array.copy sol in
          sol.(isol) <- Float.of_int v;
          dfs sol
        done)
  in
  let () = dfs (Array.create ~len:m (-1.)) in
  let () =
    Printf.printf "ans: %d\n" !ans;
    Out_channel.flush stdout
  in
  let () = assert (!ans <> -1) in
  !ans

let _ =
  get_input "10.txt"
  (* |> Fn.flip List.take 1 *)
  |> List.mapi ~f:solve
  |> List.sum (module Int) ~f:Fn.id
  |> Printf.printf "%d\n"

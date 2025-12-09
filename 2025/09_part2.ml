open Core

let get_input input_filename =
  In_channel.read_lines input_filename
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun line -> Scanf.sscanf line "%d,%d" (fun a b -> (a, b)))
  |> List.to_array

let cut (minx, maxx, miny, maxy) (x1, y1) (x2, y2) =
  not
    ((x1 <= minx && x2 <= minx)
     || (x1 >= maxx && x2 >= maxx)
     || (y1 <= miny && y2 <= miny)
     || (y1 >= maxy && y2 >= maxy))

let raycast (gx, gy) ((x1, y1), (x2, y2)) =
  if y1 = y2
  then 0
  else if x1 < gx
  then 0
  else if (y1 < gy && y2 < gy) || (y1 > gy && y2 > gy)
  then 0
  else if (y1 < gy && gy < y2) || (y2 < gy && gy < y1)
  then 1 (* probably should do some cross product for below *)
  else (
    let () = assert (gy = y1 || gy = y2) in
    if (y1 > y2 && gy = y1) || (y1 < y2 && gy = y2) then 1 else 0)

let solve points =
  let n = Array.length points in
  let ans = ref 0 in
  let () =
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        let x1, y1 = points.(i) in
        let x2, y2 = points.(j) in
        let minx = min x1 x2 in
        let maxx = max x1 x2 in
        let miny = min y1 y2 in
        let maxy = max y1 y2 in
        let area = (maxx - minx + 1) * (maxy - miny + 1) in
        let intersect = ref false in
        let () =
          for k = 0 to n - 1 do
            if cut (minx, maxx, miny, maxy) points.(k) points.((k + 1) % n)
            then intersect := true
          done
        in
        (* it's probably incorrect but we're disregarding rectangles with width 1 or 2 here *)
        (* then use (some sort of) ray casting algorithm *)
        if not !intersect
        then (
          let gx = (minx + maxx) / 2 in
          let gy = (miny + maxy) / 2 in
          let cnt = ref 0 in
          let () =
            for k = 0 to n - 1 do
              cnt := !cnt + raycast (gx, gy) (points.(k), points.((k + 1) % n))
            done
          in
          if !cnt % 2 = 1
          then (
            let a = area in
            if a > !ans
            then (
              Printf.printf
                "found bigger area: points.(%d) = (%d, %d) ... points.(%d) = (%d, %d), \
                 area = %d, cast = %d\n"
                i
                (fst points.(i))
                (snd points.(i))
                j
                (fst points.(j))
                (snd points.(j))
                a
                !cnt;
              ans := a)))
      done
    done
  in
  !ans

let _ = get_input "09.txt" |> solve |> Printf.printf "%d\n"

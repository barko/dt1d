(** learn a regression tree with one feature as input *)
open Model_t

type t = Model_t.ft

let rec infer x = function
  | `Leaf y -> y
  | `Node { left; right; split } ->
    infer x (
      if x <= split then
        left
      else
        right
    )

let fold_left_range =
  let rec loop e f i x =
    if i > e then
      x
    else
      let x = f i x in
      loop e f (i+1) x
  in
  fun ~s ~e f x ->
    loop e f s x

let rec build min_n max_depth (z_left, b_left, i_left) (z_right, b_right, i_right) depth =
  let best = fold_left_range ~s:i_left ~e:(i_right-1) (
    fun i best ->
      let n_left = i - i_left + 1 in
      let n_right = i_right - i in
      (*
      if n_left < min_n || n_right < min_n then
        best
      else *)
        let n_left = float n_left in
        let n_right = float n_right in
        let sum_z_left = z_left.(i) -. b_left in
        let sum_z_right = z_right.(i+1) -. b_right in
        let gamma_left = sum_z_left /. n_left in
        let gamma_right = sum_z_right /. n_right in
        let reduction_left =
          2. *. gamma_left *. sum_z_left -. n_left *. gamma_left *. gamma_left in
        let reduction_right =
          2. *. gamma_right *. sum_z_right -. n_right *. gamma_right *. gamma_right in

        let reduction = reduction_left +. reduction_right in
        match best with
        | Some (_, best_reduction, _, _) ->
          if reduction > best_reduction then
            (* new best *)
            Some (i, reduction, gamma_left, gamma_right)
          else
            best
        | None ->
          Some (i, reduction, gamma_left, gamma_right)
  ) None in
  match best with
  | None -> assert false
  | Some (i_split, _, gamma_left, gamma_right) ->
    let n_left = i_split - i_left + 1 in
    let n_right = i_right - i_split in
    let left =
      if n_left >= min_n && depth < max_depth then
        let b_right = z_right.(i_split + 1) in
        build min_n max_depth
          (z_left, b_left, i_left)
          (z_right, b_right, i_split)
          (depth + 1)
      else
        `Leaf gamma_left
    in
    let right =
      if n_right >= min_n && depth < max_depth then
        let b_left = z_left.(i_split) in
        build min_n max_depth
          (z_left, b_left, i_split + 1)
          (z_right, b_right, i_right)
          (depth + 1)
      else
        `Leaf gamma_right
    in
    `Node { split = i_split; left; right }


let rec fix mean_y x = function
  | `Leaf z_hat -> `Leaf (mean_y +. z_hat)
  | `Node { left; right; split = i } ->
    let x_i = x.(i) in
    let left = fix mean_y x left in
    let right = fix mean_y x right in
    `Node { left; right; split = x_i }


let learn ~min_n ~max_depth xy =
  if min_n < 1 then
    raise (Invalid_argument "learn: min_n must be at least 1");
  if max_depth < 1 then
    raise (Invalid_argument "learn: max_depth must be at least 1");

  Array.sort (fun (x1, _) (x2, _) -> Float.compare x1 x2) xy;
  let n = Array.length xy in
  let mean_y =
    let sum_y = Array.fold_left (
      fun sum_y (_x_i, y_i) ->
        sum_y +. y_i
    ) 0.0 xy in
    sum_y /. (float n)
  in
  let z = Array.map snd xy in
  for i = 0 to n-1 do
    z.(i) <- z.(i) -. mean_y
  done;
  let z_left = Array.make n 0.0 in
  let z_right = Array.make n 0.0 in

  z_left.(0) <- z.(0) *. z.(0);
  for i = 1 to n-1 do
    z_left.(i) <- z_left.(i-1) +. z.(i);
  done;

  z_right.(n-1) <- z.(n-1) *. z.(n-1);
  for i = n-2 downto 0 do
    z_right.(i) <- z_right.(i+1) +. z.(i);
  done;

  let t = build min_n max_depth (z_left, 0.0, 0) (z_right, 0.0, n-1) 1 in
  (* don't need z_left anymore; reuse it *)
  let x = z_left in
  for i = 0 to n-1 do
    x.(i) <- fst xy.(i)
  done;
  let t = fix mean_y x t in
  t

let json_of_t t =
  Model_j.string_of_ft t

let binou_of_t t =
  Model_b.string_of_ft t

let t_of_json s =
  Model_j.ft_of_string s

let t_of_binou s =
  Model_b.ft_of_string s

(*
  print_endline s;
  Array.iter (
    fun (x, y) ->
      let y_hat = eval x t in
      Printf.printf "%.0f %+.4e %+.4e\n" x y y_hat
  ) xy
*)

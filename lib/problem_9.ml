(** [find_triplet ()] finds a Pythagorean triplet (a, b, c) such that a + b + c
    = 1000 *)
let find_triplet_recursive () =
  (* [aux a b] recursive function for finding the Pythagorean triplet *)
  let rec aux a b =
    let c = 1000 - a - b in
    let is_triplet = (a * a) + (b * b) = c * c in
    match (is_triplet, a >= b) with
    | true, _ -> Some (a, b, c)
    | _, true -> None
    | false, false when b < 1000 - a -> aux a (b + 1)
    | _ -> aux (a + 1) a
  in
  let rec search a =
    match a < 500 with
    | true -> (
        match aux a (a + 1) with
        | Some (a, b, c) -> a * b * c
        | None -> search (a + 1))
    | false -> failwith "No Pythagorean triplet found"
  in
  search 1

let is_pythagorean_triplet (a, b, c) = (a * a) + (b * b) = c * c

(** [find_triplet_module] module realization of nine Euler's project problem*)
let generate_triplets () =
  let is_bigger a b = a >= b in
  let c a b = 1000 - a - b in
  let rec gen_triplets_aux a b acc =
    match is_bigger a (1000 / 3) with
    | true -> acc
    | false -> (
        match is_bigger b (c a b) with
        | true -> gen_triplets_aux (a + 1) (a + 2) acc
        | false -> gen_triplets_aux a (b + 1) ((a, b, c a b) :: acc))
  in
  gen_triplets_aux 1 2 []

let find_triplet_module () =
  generate_triplets ()
  (*same as
     |> List.filter is_pythagorean_triplet
     |> List.hd *)
  |> List.find is_pythagorean_triplet
  |> fun (a, b, c) -> a * b * c

(** [find_triplet_map] map generation realization of ninth Equler's project
    problem *)
let find_triplet_map () =
  List.init (1000 / 3) (fun i -> i + 1)
  |> List.map (fun a -> List.init a (fun b -> (a, a + b + 1)))
  |> List.flatten
  |> List.map (fun (a, b) -> (a, b, 1000 - a - b))
  |> List.filter (fun (a, b, c) -> (a * a) + (b * b) = c * c)
  |> List.hd
  |> fun (a, b, c) -> a * b * c

let find_triplet_cycle () =
  let triplet = ref None in
  for a = 1 to 1000 / 3 do
    for b = a + 1 to 1000 - a do
      let c = 1000 - a - b in
      if (a * a) + (b * b) = c * c then triplet := Some (a, b, c)
    done
  done;
  match !triplet with
  | Some (a, b, c) -> a * b * c
  | None -> failwith "No Pythagorean triplet found"

(** [find_triplet_seq] *)
let find_triplet_seq () =
  Seq.ints 1
  |> Seq.map (fun a -> (a, Seq.init (1000 - a - 1) (fun b -> a + 1 + b)))
  |> Seq.flat_map (fun (a, b_seq) ->
         Seq.map (fun b -> (a, b, 1000 - a - b)) b_seq)
  |> Seq.filter (fun (a, b, c) -> is_pythagorean_triplet (a, b, c))
  |> Seq.find (fun _ -> true)
  |> function
  | Some (a, b, c) -> a * b * c
  | None -> failwith "No Pythagorean triplet found"

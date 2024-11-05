(** [find_divisors_sum n] returns a sum of all divisors of the integer [n]. A
    divisor is any integer that divides [n] without leaving a remainder.*)
let find_divisors_sum n =
  let rec aux i acc =
    match i with
    | i when i * i > n -> List.fold_left ( + ) 0 acc
    | i when n mod i = 0 -> (
        match i with
        | 1 -> aux (i + 1) (i :: acc)
        | _ -> aux (i + 1) (i :: (n / i) :: acc))
    | _ -> aux (i + 1) acc
  in
  aux 1 []

(** [find_amicable_pairs_recursive limit] finds all amicable pairs up to the
    given [limit] *)
let find_amicable_pairs_recursive limit =
  let rec aux i =
    match i with
    | i when i >= limit -> 0
    | i -> (
        let amicable = find_divisors_sum i in
        match (i <> amicable, find_divisors_sum amicable = i) with
        | true, true -> i + aux (i + 1)
        | _ -> aux (i + 1))
  in
  aux 1

(** [find_amicable_pairs_tail limit] finds all amicable pairs up to the given
    [limit] using tail recursion *)
let find_amicable_pairs_tail limit =
  let rec aux i acc =
    match i with
    | i when i >= limit -> acc
    | i -> (
        let amicable = find_divisors_sum i in
        match (i <> amicable, find_divisors_sum amicable = i) with
        | true, true -> aux (i + 1) (acc + i)
        | _ -> aux (i + 1) acc)
  in
  aux 1 0

(** [generate_pairs limit] generate a list of pairs [i, sum] where [sum] is a
    sum of [i] divisors*)
let generate_pairs limit =
  let rec aux i acc =
    match i with
    | i when i >= limit -> acc
    | i -> aux (i + 1) ((i, find_divisors_sum i) :: acc)
  in
  aux 1 []

let find_amicable_pairs_module limit =
  generate_pairs limit
  |> List.filter (fun (a, b) -> a <> b && find_divisors_sum b = a)
  |> List.map fst
  |> List.fold_left ( + ) 0

let find_amicable_pairs_map limit =
  List.init limit (fun i -> i + 1)
  |> List.map (fun i -> (i, find_divisors_sum i))
  |> List.filter (fun (a, b) -> a <> b && find_divisors_sum b = a)
  |> List.map fst
  |> List.fold_left ( + ) 0

let find_amicable_pairs_cycle limit =
  let sum = ref 0 in
  for i = 1 to limit - 1 do
    let amicable = find_divisors_sum i in
    match (i <> amicable, find_divisors_sum amicable = i) with
    | true, true -> sum := !sum + i
    | _ -> ()
  done;
  !sum

let find_amicable_pairs_seq limit =
  Seq.init limit (fun i -> i + 1)
  |> Seq.filter (fun i ->
         find_divisors_sum i <> i && find_divisors_sum (find_divisors_sum i) = i)
  |> Seq.fold_left ( + ) 0

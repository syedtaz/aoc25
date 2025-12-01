open Core

type op =
  | R of int
  | L of int

let delta = function
  | R x -> x
  | L x -> -x
;;

module IO = struct
  let op_of_line l =
    match String.chop_prefix l ~prefix:"L" with
    | Some x -> L (int_of_string x)
    | None ->
      let x = String.chop_prefix_exn l ~prefix:"R" in
      R (int_of_string x)
  ;;

  let parse_input () =
    let file = In_channel.create ~binary:false "input" in
    let lines =
      In_channel.fold_lines file ~init:[] ~f:(fun acc l -> op_of_line l :: acc)
    in
    List.rev lines
  ;;
end

module V2 = struct
  module Comp = struct
    let div' op sum =
      match op with
      | R x ->
        let rem = x mod 100 in
        let crossed = if sum + rem >= 100 && not (sum = 0) then 1 else 0 in
        let count = (x - rem) / 100 in
        count + crossed, (sum + rem) % 100
      | L x ->
        let rem = x mod 100 in
        let crossed = if sum - rem <= 0 && not (sum = 0) then 1 else 0 in
        let count = (x - rem) / 100 in
        count + crossed, (sum - rem) % 100
    ;;

    let rec step lst sum pass =
      match lst with
      | [] -> pass
      | h :: t ->
        let incr, sum' = div' h sum in
        step t sum' (pass + incr)
    ;;

    let eval lst = step lst 50 0
  end

  let answer () =
    let lines = IO.parse_input () in
    let result = Comp.eval lines in
    Format.printf "Answer: %d\n" result
  ;;
end

module V1 = struct
  module Comp = struct
    let rec step lst sum pass =
      match lst with
      | h :: t ->
        let sum' = sum + delta h in
        let pass' = pass + if sum' % 100 = 0 then 1 else 0 in
        step t sum' pass'
      | [] -> pass
    ;;

    let eval lst = step lst 50 0
  end

  let answer () =
    let lines = IO.parse_input () in
    let result = Comp.eval lines in
    Format.printf "Answer: %d\n" result
  ;;
end

(* let () = V1.answer () *)
let () = V2.answer ()

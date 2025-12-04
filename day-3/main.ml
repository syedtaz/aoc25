open! Core

module IO = struct
  let parse_input () =
    let file = In_channel.create ~binary:false "input" in
    let lines =
      In_channel.fold_lines file ~init:[] ~f:(fun acc l ->
        (String.to_list l |> List.map ~f:Char.get_digit_exn) :: acc)
    in
    List.map lines ~f:Array.of_list |> List.rev
  ;;
end

module Comp : sig
  val compute : int array -> int -> int
end = struct
  let compute arr n =
    let length = Array.length arr in
    let is = List.init n ~f:(fun i -> i) in
    let acc, _ =
      List.fold is ~init:(0, 0) ~f:(fun (acc, start) pos ->
        let len = length - start - (n - pos) + 1 in
        let range = Array.sub arr ~pos:start ~len in
        let value = Array.max_elt range ~compare:Int.compare |> Option.value_exn in
        let start', _ = Array.findi_exn range ~f:(fun _ x -> Int.equal x value) in
        (acc * 10) + value, start + start' + 1)
    in
    acc
  ;;
end

module V2 = struct
  let eval lst = List.fold ~init:0 ~f:(fun acc x -> acc + Comp.compute x 12) lst

  let answer () =
    let lines = IO.parse_input () in
    let result = eval lines in
    Format.printf "Answer: %d\n" result
  ;;
end

module V1 = struct
  let eval lst = List.fold ~init:0 ~f:(fun acc x -> acc + Comp.compute x 2) lst

  let answer () =
    let lines = IO.parse_input () in
    let result = eval lines in
    Format.printf "Answer: %d\n" result
  ;;
end

let () = V2.answer ()
(* let () = V1.answer () *)

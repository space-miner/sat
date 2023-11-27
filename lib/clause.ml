open Base

module T = struct
  type t = Literal.t list [@@deriving sexp]

  let contains_literal cls lit = List.mem cls lit ~equal:Literal.equal
  let remove_literal cls lit = List.filter cls ~f:(fun l -> l = lit)
  let is_empty cls = List.length cls = 0

  (* name for this function is bad returns and option if it's an union otherwise it's None *)
  (* not sure if this function should even exist -- makes the api jank *)
  let unit_literal_opt cls = if List.length cls = 1 then Some (List.hd_exn cls) else None

  let resolve cls1 cls2 : t =
    let cls1' = List.filter cls1 ~f:(fun lit -> contains_literal cls2 (neg lit)) in
    let cls2' = List.filter cls2 ~f:(fun lit -> contains_literal cls1 (neg lit)) in
    List.append cls1' cls2' |> List.dedup_and_sort ~compare:Literal.compare
  ;;

  let check_satisfiable cls assignment : Bool.t =
    With_return.with_return (fun { return } ->
      List.fold_left assignment ~init:false ~f:(fun acc (var, bool) ->
        let witness =
          let contains_lit = contains_literal cls var in
          let contains_neg_lit = contains_literal cls (neg var) in
          if contains_lit || contains_neg_lit
          then (
            let lit = if contains_lit then var else neg var in
            return
              (if Bool.equal bool true then Int.equal lit var else Int.equal lit (neg var)))
          else false
        in
        acc || witness))
  ;;

  let equal cls1 cls2 = List.equal Literal.equal cls1 cls2

  let choose_variable cls assignment : Literal.t option =
    With_return.with_return (fun { return } ->
      let _ =
        List.iter cls ~f:(fun lit ->
          let var : Int.t = Literal.to_var lit in
          let bool_opt = Assignment.get assignment var in
          match bool_opt with
          | Some truth_assignment -> ()
          | None -> return (Some var))
      in
      None)
  ;;

  let rec compare cls1 cls2 =
    let cls1' = cls1 |> List.dedup_and_sort ~compare:Literal.compare in
    let cls2' = cls2 |> List.dedup_and_sort ~compare:Literal.compare in
    match cls1', cls2' with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: t1, h2 :: t2 -> if h1 < h2 then -1 else if h1 > h2 then 1 else compare t1 t2
  ;;

  let sexp_of_t cls = List.sexp_of_t Literal.sexp_of_t cls
end

include T
include Comparator.Make (T)

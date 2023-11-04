open Base
open Stdio
open Assignment
open Clause
open Literal

module CNF_Formula = struct
  module T = struct
    type t = Clause.t list [@@deriving sexp]

    (* not sure i want this formula, eliminate literals from fml, where literals is a *)
    (* list of literals *)
    let eliminate fml literals =
      (* we create a new list with the current clause and the literals, if there's a dup *)
      (*  then the clause contains a pure so we can eliminate it *)
      List.filter fml ~f:(fun cls ->
        List.contains_dup (List.append cls literals) ~compare:Literal.compare)
    ;;

    let unit_literals fml =
      List.fold fml ~init:[] ~f:(fun acc cls -> Clause.unit_literal_opt cls :: acc)
      |> List.filter_opt
    ;;

    let pure_literals fml =
      (* if both polarities exist in dedup_literals then it's not a pure *)
      let dedup_literals =
        List.concat fml |> List.dedup_and_sort ~compare:Literal.compare
      in
      let pure_literals =
        List.fold dedup_literals ~init:[] ~f:(fun acc lit ->
          if List.mem dedup_literals (Literal.neg lit) ~equal:Literal.equal
          then acc
          else lit :: acc)
      in
      pure_literals
    ;;

    let unit_propogation fml assignment =
      let units = unit_literals fml in
      print_s [%sexp (units : Literal.t list)];
      let assignment' = Assignment.add_literals assignment units in
      let fml' = eliminate fml units in
      if Assignment.is_consistent assignment'
      then Ok (assignment', fml')
      else Error "todo"
    ;;

    let pure_literal_elimination fml assignment =
      let pures = pure_literals fml in
      print_s [%sexp (pures : Literal.t list)];
      let assignment' = Assignment.add_literals assignment pures in
      let fml' = eliminate fml pures in
      if Assignment.is_consistent assignment'
      then Ok (assignment', fml')
      else Error "todo"
    ;;

    let choose_variable_opt fml assignment : Literal.t option =
      With_return.with_return (fun { return } ->
        let _ =
          List.iter fml ~f:(fun cls ->
            let var_opt = Clause.choose_variable cls assignment in
            print_s [%sexp (var_opt : int option)];
            match var_opt with
            | Some var -> return (Some var)
            | None -> ())
        in
        None)
    ;;

    let rec compare fml1 fml2 =
      let fml1' = List.sort fml1 ~compare:Clause.compare in
      let fml2' = List.sort fml2 ~compare:Clause.compare in
      match fml1', fml2' with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | h1 :: t1, h2 :: t2 ->
        let cmp = Clause.compare h1 h2 in
        if cmp = 0 then compare t1 t2 else cmp
    ;;

    let sexp_of_t (fml : t) = List.sexp_of_t Clause.sexp_of_t fml
  end

  include T
  include Comparator.Make (T)
end

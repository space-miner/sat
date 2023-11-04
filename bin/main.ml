open Base
open Stdio
open Assignment
open Clause
open Cnf_formula
open Literal
open Util

module Solver_Result = struct
  type t =
    | Satisfiable of Assignment.t
    | Unsatisfiable
  [@@deriving sexp]
end

module Conflict = struct
  type t =
    { learned_clause : Clause.t
    ; backjump_level : int
    }
  [@@deriving sexp]
end

module type Solver = sig
  type t = Solver_Result.t

  val solve : CNF_Formula.t -> Assignment.t -> t
end

module DPLL : Solver = struct
  type t = Solver_Result.t

  let rec solve fml assignment =
    With_return.with_return (fun { return } ->
      let pure_elim_fml_result = CNF_Formula.pure_literal_elimination fml assignment in
      match pure_elim_fml_result with
      | Ok (assignment', fml') ->
        let unit_prop_fml_result = CNF_Formula.unit_propogation fml' assignment' in
        (match unit_prop_fml_result with
         | Ok (assignment'', fml'') ->
           let var_opt = CNF_Formula.choose_variable_opt fml'' assignment'' in
           (match var_opt with
            | Some var ->
              let true_assignment = Assignment.add assignment'' var true in
              let false_assignment = Assignment.add assignment'' var false in
              let true_result = solve fml true_assignment in
              (match true_result with
               | Solver_Result.Satisfiable _ -> return true_result
               | Solver_Result.Unsatisfiable ->
                 let false_result = solve fml false_assignment in
                 false_result)
            | None -> Solver_Result.Unsatisfiable)
         | _ -> Solver_Result.Unsatisfiable)
      | _ -> Solver_Result.Unsatisfiable)
  ;;
end

let () =
  let dimacs_string = In_channel.read_all "../test/sat-test.cnf" in
  let fml = Util.parse_dimacs dimacs_string in
  let assignment = [] in
  let result = DPLL.solve fml assignment in
  print_s [%sexp (fml : CNF_Formula.t)];
  print_s [%sexp (result : Solver_Result.t)]
;;

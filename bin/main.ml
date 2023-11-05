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
      let fml, assignment = CNF_Formula.pure_literal_elimination fml assignment in
      let fml, assignment = CNF_Formula.unit_propogation fml assignment in
      let var_opt = CNF_Formula.choose_variable_opt fml assignment in
      match var_opt with
      | Some var ->
        (* this feels jank here, assigning and making calls to eliminate *)
        let true_assignment = Assignment.add assignment var true in
        let false_assignment = Assignment.add assignment var false in
        let true_fml = CNF_Formula.eliminate fml [var] in
        let false_fml = CNF_Formula.eliminate fml [Literal.neg var] in
        let true_result = solve true_fml true_assignment in
        (match true_result with
         | Solver_Result.Satisfiable _ -> return true_result
         | Solver_Result.Unsatisfiable ->
           let false_result = solve false_fml false_assignment in
           false_result)
      | None ->
        if CNF_Formula.is_satisfiable fml
        then Solver_Result.Satisfiable assignment
        else Solver_Result.Unsatisfiable)
  ;;
end

let () =
  let dimacs_string = In_channel.read_all "../test/test002.cnf" in
  let fml = Util.parse_dimacs dimacs_string in
  let assignment = [] in
  let result = DPLL.solve fml assignment in
  print_s [%sexp (fml : CNF_Formula.t)];
  print_s [%sexp (result : Solver_Result.t)]
;;

open Base
open Stdio

module Literal = struct
  module T = struct
    type t = int [@@deriving sexp]

    let neg lit = -1 * lit
    let is_positive lit = lit > 0
    let is_negative lit = lit < 0
    let compare = Int.compare
    let sexp_of_t = Int.sexp_of_t
  end

  include T
  include Comparator.Make (T)
end

module Clause = struct
  type t = (Literal.t, Literal.comparator_witness) Set.t [@@deriving sexp]

  let add_literal cls lit : t = Set.add cls lit
  let remove_literal cls lit : t = Set.remove cls lit
  let contains_literal cls lit = Set.mem cls lit
  let is_empty cls : Bool.t = Set.length cls = 0
  let get_literals cls : Literal.t list = Set.to_list cls
  let resolve cls1 cls2 : t option = failwith "todo: apply resolution"
  let compare cls1 cls2 : Int.t = failwith "optional"
  let sexp_of_t cls = failwith "todo"
end

module CNF_Formula = struct
  type t = Clause.t list [@@deriving sexp]

  let contains_clause fml cls : Bool.t = List.mem fml cls ~equal:Set.equal

  let add_clause fml cls : t =
    if contains_clause fml cls then fml else List.append fml [ cls ]
  ;;

  let remove_clause fml cls : t = List.filter fml ~f:(fun c -> Set.equal cls c)
  let is_empty fml : Bool.t = failwith "todo"
  let get_clauses fml : Clause.t list = failwith "todo"
  let merge fml1 fml2 : t = failwith "todo: combine two cnf formulas into one"
  let is_satisfiable fml : Bool.t = failwith "todo"

  let unit_propagate fml : Literal.t option * t =
    failwith "todo: any unit literal * cnf formula after applying unit"
  ;;

  let pure_literal_eliminate fml : Literal.t list * t =
    failwith "todo: list of eliminated literals * updated cnf formula"
  ;;

  let check_satisfiability t assignment : Bool.t = failwith "todo"
  (*
     - Given a CNF formula and an assignment, checks if the assignment satisfies the formula.
  *)
end

module Assignment = struct
  type t = (int * bool) List.t [@@deriving sexp]

  let empty : t = []
  let add t : int -> bool -> t = failwith "todo"
  let remove t : int -> t = failwith "todo"
  let get t : int -> Bool.t option = failwith "get assignment for variable"

  let is_consistent t : Bool.t =
    failwith
      "check if assignment is consistent -- no conflicting values for same variable"
  ;;

  (* util *)
  let variable t : int list = failwith "get list of variables that have been assigned"
  let size t : int = failwith "get total number of variables assigned"
end

module Result = struct
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
  type t = Result.t

  val solve : CNF_Formula.t -> t
end

module DPLL : Solver = struct
  type t = Result.t

  let solve (fml : CNF_Formula.t) : Result.t = failwith "todo"
end

module CDCL : Solver = struct
  type t = Result.t

  let solve (fml : CNF_Formula.t) : Result.t = failwith "todo"
  let propagate : Assignment.t -> CNF_Formula.t -> Conflict.t option = failwith "todo"
  (*
     - Given an assignment and a CNF formula, performs unit propagation.
     - If a conflict is detected, returns the conflict information.
     - Otherwise, returns None.
  *)

  let analyze_conflict : Assignment.t -> Conflict.t -> Clause.t option = failwith "todo"
  (*
     - Given an assignment and a conflict, performs conflict analysis to learn a new clause.
     - Returns the learned clause, or None if no clause can be learned.
  *)
end

let parse_dimacs dimacs_string =
  let lines = dimacs_string |> String.split_lines |> List.map ~f:String.strip in
  print_s [%sexp (lines : string list)]
;;

let () = print_s [%sexp ("Hello, World!" : string)]

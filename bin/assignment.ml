open Base
open Literal

(* might want to turn this into a list with assignments and have clauses be references*)
module Assignment = struct
  type t = (int * bool) List.t [@@deriving sexp]

  let empty = []
  let add assignment var value : t = (var, value) :: assignment

  let add_literal assignment lit : t =
    let var = Literal.to_var lit in
    if Literal.is_positive lit then add assignment var true else add assignment var false
  ;;

  let add_literals assignment literals : t =
    List.fold literals ~init:assignment ~f:(fun acc lit -> add_literal acc lit)
  ;;

  let remove assignment var : t = List.filter assignment ~f:(fun (v, _) -> v <> var)

  let is_consistent assignment : Bool.t =
    let set =
      Set.of_list (module Int) (List.map assignment ~f:(fun assign -> fst assign))
    in
    Set.length set = List.length assignment
  ;;

  let get assignment var : Bool.t option =
    match List.find assignment ~f:(fun (v, _) -> v = var) with
    | Some (_, value) -> Some value
    | None -> None
  ;;

  (* util *)
  let variable t : Literal.t list = List.map t ~f:(fun int_bool -> fst int_bool)
  let size t : Int.t = List.length t
end

open Base
open Cnf_formula
open Literal

(* first line of dimacs is meta nonsense *)
(* clauses are all delimited by 0s *)
let parse_dimacs (dimacs_string : String.t) : CNF_Formula.t =
  let conjoined_clauses =
    dimacs_string
    |> String.split_lines (* turn into lines *)
    |> List.map ~f:String.strip (* clean up lines *)
    |> List.tl_exn (* remove metadata line *)
    |> List.map ~f:(fun s -> String.split_on_chars s ~on:[ ' '; '\n'; '\r' ])
      (* split lines on whitespace *)
    |> List.concat (* join them together *)
    |> List.filter ~f:(fun s -> String.length s > 0) (* remove empty strings *)
    |> List.map ~f:Literal.of_string (* turn into list of integers *)
    |> List.to_array (* convert to array) *)
  in
  let fml = ref [] in
  let clause = ref [] in
  for i = 0 to Array.length conjoined_clauses - 1 do
    let lit = conjoined_clauses.(i) in
    if lit = 0
    then (
      fml := List.append !fml [ !clause ];
      clause := [])
    else clause := lit :: !clause
  done;
  !fml
;;

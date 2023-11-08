open Base
open Stdio
open Cnf_formula
open Literal

(* https://www.domagoj-babic.com/uploads/ResearchProjects/Spear/dimacs-cnf.pdf *)
(* c are comment lines *)
(* p is the problem line with meta info like form, number of variables and number of clauses *)
(* clauses are all delimited by 0s *)
let parse_dimacs (dimacs_string : String.t) : CNF_Formula.t =
  let conjoined_clauses =
    dimacs_string
    |> String.split_lines (* turn into lines *)
    |> List.map ~f:String.strip (* strip lines of whitespace on left and right *)
    |> List.filter ~f:(fun ln ->
      (not (String.is_empty ln))
      && String.to_list ln
         |> List.hd_exn
         |> fun ch -> Char.is_digit ch || Char.( = ) ch '-')
      (* remove comment and problem lines -- those starting with c and p *)
    |> List.map ~f:(fun ln -> String.split_on_chars ln ~on:[ ' '; '\n'; '\r' ])
      (* split each lines on whitespace *)
    |> List.concat
       (* join all the lines together -- clauses are not line based, 0 denotes end of clause *)
    |> List.filter ~f:(fun ln -> not (String.is_empty ln)) (* take non-empty strings *)
    |> List.map ~f:Literal.of_string (* turn into list of integers *)
    |> List.to_array (* convert to array *)
  in
  let fml = ref [] in
  let clause = ref [] in
  for i = 0 to Array.length conjoined_clauses - 1 do
    let lit = conjoined_clauses.(i) in
    if lit = 0 && List.length !clause > 0
    then (
      fml := List.append !fml [ !clause ];
      clause := [])
    else clause := lit :: !clause
  done;
  !fml
;;

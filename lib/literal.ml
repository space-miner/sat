open Base

(* does this encoding make sense? -l = 2k + 1 and l as 2k => |l| = x >> 1 *)
module T = struct
  include Comparable.Make (Int)

  type t = int [@@deriving sexp]

  let neg lit = -1 * lit
  let is_positive lit = lit > 0
  let is_negative lit = lit < 0
  let of_string lit = Int.of_string lit
  let to_var lit : Int.t = Int.abs lit
end

include T
include Comparator.Make (T)

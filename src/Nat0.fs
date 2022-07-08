namespace Bin_prot

module Nat0 =
  (* Nat0: natural numbers (including zero) *)

  type t = int

  let of_int n =
    if n < 0 then
      failwith "Bin_prot.Nat0.of_int: n < 0"

    n

  let unsafe_of_int (n : t) = n

namespace Bin_prot

module Nat0 =
  (** Nat0: natural numbers (including zero) *)

  type t = int

  (** [of_int n] converts integer [n] to a natural number.  @raise Failure
      if [n] is negative. *)
  val of_int : int -> t

  val unsafe_of_int : int -> t

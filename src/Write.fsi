namespace Bin_prot

module Write =
  (** Writing values to the binary protocol using (mostly) OCaml. *)

  open Common

  (** Type of writer functions for the binary protocol. They take a buffer,
      a write position and a value, and return the next position after
      writing out the value. *)
  type 'a writer = buf -> pos -> 'a -> pos

  type ('a, 'b) writer1 = 'a writer -> 'b writer
  type ('a, 'b, 'c) writer2 = 'a writer -> writer1<'b, 'c>
  type ('a, 'b, 'c, 'd) writer3 = 'a writer -> writer2<'b, 'c, 'd>

  val bin_write_unit : unit writer
  val bin_write_bool : bool writer
  val bin_write_string : string writer
  val bin_write_char : char writer
  val bin_write_int64 : int64 writer
  val bin_write_nat0 : Nat0.t writer
  val bin_write_float : float writer
  val bin_write_int32 : int32 writer
  val bin_write_ref : writer1<'a, 'a ref>
  val bin_write_lazy : writer1<'a, 'a Lazy>
  val bin_write_option : writer1<'a, 'a option>
  val bin_write_pair : writer2<'a, 'b, 'a * 'b>
  val bin_write_triple : writer3<'a, 'b, 'c, 'a * 'b * 'c>
  val bin_write_list : writer1<'a, 'a list>
  val bin_write_array : writer1<'a, 'a array>

  (** [bin_write_variant_int] writes out the exact little-endian bit
      representation of the variant tag of the given value (= 32 bits). *)
  val bin_write_variant_int : int writer

  (** 8 and 16 bit use int32 as they're used for variant constructor id in OCaml
      and if we use int64 we need the 'L' suffix *)

  (** [bin_write_int_8bit] writes out the exact little-endian bit representation
      of the given [int] value using the lower 8 bits. *)
  val bin_write_int_8bit : int writer

  (** [bin_write_int_16bit] writes out the exact little-endian bit representation
      of the given [int] value using the lower 16 bits. *)
  val bin_write_int_16bit : int writer

  (** [bin_write_int_32bit] writes out the exact little-endian bit representation
      of the given [int] value using the lower 32 bits. *)
  val bin_write_int_32bit : int64 writer

  (** [bin_write_int_64bit] writes out the exact little-endian bit representation
      of the given [int] value using all 64 bits. *)
  val bin_write_int_64bit : int64 writer

  (** [bin_write_network16_int] writes out an integer in 16bit network
      byte order (= big-endian). *)
  val bin_write_network16_int : int64 writer

  (** [bin_write_network32_int] writes out an integer in 32bit network
      byte order (= big-endian). *)
  val bin_write_network32_int : int64 writer

  (** [bin_write_network64_int] writes out an integer in 64bit network
      byte order (= big-endian). *)
  val bin_write_network64_int : int64 writer

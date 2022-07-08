namespace Bin_prot

module Read =
  (** Reading values from the binary protocol using F#. *)

  open Common

  (** Type of reader functions for the binary protocol.  They take a
      buffer and a reference to a read position, and return the unmarshalled
      value.  The next buffer position after reading in the value will be
      stored in the position reference. *)
  type 'a reader = buf -> pos_ref -> 'a

  type ('a, 'b) reader1 = 'a reader -> 'b reader
  type ('a, 'b, 'c) reader2 = 'a reader -> ('b, 'c)reader1
  type ('a, 'b, 'c, 'd) reader3 = 'a reader -> ('b, 'c, 'd)reader2

  val bin_read_unit : unit reader
  val bin_read_bool : bool reader
  val bin_read_string : string reader
  val bin_read_char : char reader
  val bin_read_int64 : int64 reader
  val bin_read_nat0 : Nat0.t reader
  val bin_read_float : float reader
  val bin_read_int32 : int32 reader
  val bin_read_ref : ('a, 'a ref)reader1
  val bin_read_lazy : ('a, 'a Lazy)reader1
  val bin_read_option : ('a, 'a option)reader1
  val bin_read_pair : ('a, 'b, 'a * 'b)reader2
  val bin_read_triple : ('a, 'b, 'c, 'a * 'b * 'c)reader3
  val bin_read_list : ('a, 'a list)reader1
  val bin_read_array : ('a, 'a array)reader1
  val bin_read_variant_int : int reader
  (** 8 and 16 bit use int32 as they're used for variant constructor id in OCaml
      and if we use int64 we need the 'L' suffix *)
  val bin_read_int_8bit : int reader
  val bin_read_int_16bit : int reader
  val bin_read_int_32bit : int64 reader
  val bin_read_int_64bit : int64 reader
  val bin_read_network16_int : int64 reader
  val bin_read_network32_int : int64 reader
  val bin_read_network64_int : int64 reader

  (** Fail early if the list is larger than [max_len]. *)
  val bin_read_list_with_max_len : max_len : int -> ('a, 'a list)reader1

  (** These functions all raise as they should only be used to deserialize
      variants. [ppx_bin_prot] will sometimes generate references to them so we
      need to provide them for basic types. *)
  val __bin_read_unit__ : (int -> unit) reader
  val __bin_read_bool__ : (int -> bool) reader
  val __bin_read_string__ : (int -> string) reader
  val __bin_read_char__ : (int -> char) reader
  val __bin_read_float__ : (int -> float) reader
  val __bin_read_float_array__ : (int -> float array) reader
  val __bin_read_int32__ : (int -> int32) reader
  val __bin_read_int64__ : (int -> int64) reader
  val __bin_read_ref__ : ('a, int -> 'a ref)reader1
  val __bin_read_lazy__ : ('a, int -> 'a Lazy)reader1
  val __bin_read_option__ : ('a, int -> 'a option)reader1
  val __bin_read_list__ : ('a, int -> 'a list)reader1
  val __bin_read_array__ : ('a, int -> 'a array)reader1

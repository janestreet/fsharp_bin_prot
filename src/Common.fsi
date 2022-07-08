namespace Bin_prot

module Common =
  (** Common definitions used by binary protocol converters *)

  open Buffer

  (** {2 Buffers} *)

  (** Position within buffers *)
  type pos = int

  (** Reference to a position within buffers *)
  type pos_ref = pos ref

  (** Buffers *)
  type buf = Buffer<byte>

  (** [create_buf n] creates a buffer of size [n]. *)
  val create_buf : int -> buf

  (** [assert_pos pos] @raise Invalid_argument if position [pos] is negative. *)
  val assert_pos : pos -> unit

  (** [check_pos buf pos] @raise Buffer_short if position [pos] exceeds
      the length of buffer [buf]. *)
  val check_pos : buf -> pos -> unit

  (** [check_next buf pos] @raise Buffer_short if the next position after
      [pos] exceeds the length of buffer [buf]. *)
  val check_next : buf -> pos -> unit

  (** [safe_get_pos buf pos_ref] @return the position referenced by
      [pos_ref] within buffer [buf].  @raise Buffer_short if the position
      exceeds the length of the buffer. *)
  val safe_get_pos : buf -> pos_ref -> pos

  (** {2 Errors and exceptions} *)

  (** Buffer too short for read/write operation *)
  exception Buffer_short

  (** Used internally for backtracking *)
  exception No_variant_match

  module ReadError =
    type t =
      | Neg_int8 (** Negative integer was positive or zero *)
      | Int_code (** Unknown integer code while reading integer *)
      | Int_overflow (** Overflow reading integer *)
      | Nat0_code (** Unknown integer code while reading natural number *)
      | Nat0_overflow (** Overflow reading natural number *)
      | Int32_code (** Unknown integer code while reading 32bit integer *)
      | Int64_code (** Unknown integer code while reading 64bit integer *)
      | Unit_code (** Illegal unit value *)
      | Bool_code (** Illegal boolean value *)
      | Option_code (** Illegal option code *)
      | String_too_long (** String too long *)
      | Variant_tag (** Untagged integer encoding for variant tag *)
      | Array_too_long (** Array too long *)
      | List_too_long of {| len : int; max_len : int |}
      | Sum_tag of string (** Illegal sum tag for given type *)
      | Variant of string (** Illegal variant for given type *)
      | Variant_wrong_type of string
      (** Unexpected attempt to read variant with given non-variant type *)
      | Silly_type of string
      (** [Silly_type type_name] indicates unhandled but silly case
          where a type of the sort [type 'a type_name = 'a] is used
          with a polymorphic variant as type parameter and included
          in another polymorphic variant type. *)
      | Empty_type of
        string (** Attempt to read data that corresponds to an empty type. *)

  (** [ReadError (err, err_pos)] *)
  exception Read_error of ReadError.t * pos

  (** [EmptyType] gets raised when the user attempts to write or estimate
      the size of a value of an empty type, which would not make sense. *)
  exception Empty_type of string

  (** [raise_read_error err pos] *)
  val raise_read_error : ReadError.t -> pos -> 'a

  (** [raise_variant_wrong_type name pos] *)
  val raise_variant_wrong_type : string -> pos -> 'a

  (** [array_bound_error ()] *)
  val array_bound_error : unit -> 'a

  val (+) : int -> int -> int

namespace Bin_prot

module Type_class =
  (** Sizers, writers, and readers in records *)

  open Common

  type 'a writer =
    { size : 'a Size.sizer
      write : 'a Write.writer }

  type 'a reader =
    { read : 'a Read.reader
      vtag_read : (int -> 'a) Read.reader }

  type 'a t =
    { writer : 'a writer
      reader : 'a reader }

  type 'a writer0 = 'a writer
  type 'a reader0 = 'a reader
  type 'a t0 = 'a t

  module S1 =
    type ('a, 'b) writer = 'a writer0 -> 'b writer0
    type ('a, 'b) reader = 'a reader0 -> 'b reader0
    type ('a, 'b) t = 'a t0 -> 'b t0

  module S2 =
    type ('a, 'b, 'c) writer = 'a writer0 -> S1.writer<'b, 'c>
    type ('a, 'b, 'c) reader = 'a reader0 -> S1.reader<'b, 'c>
    type ('a, 'b, 'c) t = 'a t0 -> S1.t<'b, 'c>

  module S3 =
    type ('a, 'b, 'c, 'd) writer = 'a writer0 -> S2.writer<'b, 'c, 'd>
    type ('a, 'b, 'c, 'd) reader = 'a reader0 -> S2.reader<'b, 'c, 'd>
    type ('a, 'b, 'c, 'd) t = 'a t0 -> S2.t<'b, 'c, 'd>

  (*$ open Bin_prot_cinaps.Sig *)
  (*$ mk_base "unit" *)
  val bin_writer_unit : unit writer
  val bin_reader_unit : unit reader
  val bin_unit : unit t

  (*$ mk_base "bool" *)
  val bin_writer_bool : bool writer
  val bin_reader_bool : bool reader
  val bin_bool : bool t

  (*$ mk_base "string" *)
  val bin_writer_string : string writer
  val bin_reader_string : string reader
  val bin_string : string t

  (*$ mk_base "char" *)
  val bin_writer_char : char writer
  val bin_reader_char : char reader
  val bin_char : char t

  (*$ mk_base "int64" *)
  val bin_writer_int64 : int64 writer
  val bin_reader_int64 : int64 reader
  val bin_int64 : int64 t

  (*$ mk_base "float" *)
  val bin_writer_float : float writer
  val bin_reader_float : float reader
  val bin_float : float t

  (*$ mk_base "int32" *)
  val bin_writer_int32 : int32 writer
  val bin_reader_int32 : int32 reader
  val bin_int32 : int32 t

  (*$ mk_base_tp "nat0" "Nat0.t" *)
  val bin_writer_nat0 : Nat0.t writer
  val bin_reader_nat0 : Nat0.t reader
  val bin_nat0 : Nat0.t t

  (*$ mk_base1 "ref" *)
  val bin_writer_ref : S1.writer<'a, 'a ref>
  val bin_reader_ref : S1.reader<'a, 'a ref>
  val bin_ref : S1.t<'a, 'a ref>

  (*$ mk_base1_tp "lazy" "lazy_t" *)
  val bin_writer_lazy : S1.writer<'a, 'a Lazy>
  val bin_reader_lazy : S1.reader<'a, 'a Lazy>
  val bin_lazy : S1.t<'a, 'a Lazy>

  (*$ mk_base1 "option" *)
  val bin_writer_option : S1.writer<'a, 'a option>
  val bin_reader_option : S1.reader<'a, 'a option>
  val bin_option : S1.t<'a, 'a option>

  (*$*)

  val bin_writer_pair : S2.writer<'a, 'b, 'a * 'b>
  val bin_reader_pair : S2.reader<'a, 'b, 'a * 'b>
  val bin_pair : S2.t<'a, 'b, 'a * 'b>
  val bin_writer_triple : S3.writer<'a, 'b, 'c, 'a * 'b * 'c>
  val bin_reader_triple : S3.reader<'a, 'b, 'c, 'a * 'b * 'c>
  val bin_triple : S3.t<'a, 'b, 'c, 'a * 'b * 'c>

  (*$ mk_base1 "list" *)
  val bin_writer_list : S1.writer<'a, 'a list>
  val bin_reader_list : S1.reader<'a, 'a list>
  val bin_list : S1.t<'a, 'a list>

  (*$ mk_base1 "array" *)
  val bin_writer_array : S1.writer<'a, 'a array>
  val bin_reader_array : S1.reader<'a, 'a array>
  val bin_array : S1.t<'a, 'a array>

  (*$*)

  val bin_writer_variant_int : int writer
  val bin_reader_variant_int : int reader
  val bin_variant_int : int t

  (*$ mk_base_tp "int_8bit" "int" *)
  val bin_writer_int_8bit : int writer
  val bin_reader_int_8bit : int reader
  val bin_int_8bit : int t

  (*$ mk_base_tp "int_16bit" "int" *)
  val bin_writer_int_16bit : int writer
  val bin_reader_int_16bit : int reader
  val bin_int_16bit : int t

  (*$ mk_base_tp "int_32bit" "int" *)
  val bin_writer_int_32bit : int64 writer
  val bin_reader_int_32bit : int64 reader
  val bin_int_32bit : int64 t

  (*$ mk_base_tp "int_64bit" "int" *)
  val bin_writer_int_64bit : int64 writer
  val bin_reader_int_64bit : int64 reader
  val bin_int_64bit : int64 t

  (*$ mk_base_tp "network16_int" "int" *)
  val bin_writer_network16_int : int64 writer
  val bin_reader_network16_int : int64 reader
  val bin_network16_int : int64 t

  (*$ mk_base_tp "network32_int" "int" *)
  val bin_writer_network32_int : int64 writer
  val bin_reader_network32_int : int64 reader
  val bin_network32_int : int64 t

  (*$ mk_base_tp "network64_int" "int" *)
  val bin_writer_network64_int : int64 writer
  val bin_reader_network64_int : int64 reader
  val bin_network64_int : int64 t

  (** Conversion of binable types *)

  val cnv_writer : ('a -> 'b) -> 'b writer -> 'a writer
  val cnv_reader : ('b -> 'a) -> 'b reader -> 'a reader
  val cnv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

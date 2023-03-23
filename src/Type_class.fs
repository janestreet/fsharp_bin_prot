namespace Bin_prot

module Type_class =

  (* Tp_class: sizers, writers, and readers in records *)

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

  let variant_wrong_type name _buf pos_ref _x =
    Common.raise_variant_wrong_type name !pos_ref

  (*$ open Bin_prot_cinaps.Str *)
  (*$ mk_base "unit" *)
  let bin_writer_unit =
    { size = Size.bin_size_unit
      write = Write.bin_write_unit }

  let bin_reader_unit =
    { read = Read.bin_read_unit
      vtag_read = variant_wrong_type "unit" }

  let bin_unit =
    { writer = bin_writer_unit
      reader = bin_reader_unit }

  (*$ mk_base "bool" *)
  let bin_writer_bool =
    { size = Size.bin_size_bool
      write = Write.bin_write_bool }

  let bin_reader_bool =
    { read = Read.bin_read_bool
      vtag_read = variant_wrong_type "bool" }

  let bin_bool =
    { writer = bin_writer_bool
      reader = bin_reader_bool }

  (*$ mk_base "string" *)
  let bin_writer_string =
    { size = Size.bin_size_string
      write = Write.bin_write_string }

  let bin_reader_string =
    { read = Read.bin_read_string
      vtag_read = variant_wrong_type "string" }

  let bin_string =
    { writer = bin_writer_string
      reader = bin_reader_string }

  (*$ mk_base "char" *)
  let bin_writer_char =
    { size = Size.bin_size_char
      write = Write.bin_write_char }

  let bin_reader_char =
    { read = Read.bin_read_char
      vtag_read = variant_wrong_type "char" }

  let bin_char =
    { writer = bin_writer_char
      reader = bin_reader_char }

  (*$ mk_base "int64" *)
  let bin_writer_int64 =
    { size = Size.bin_size_int64
      write = Write.bin_write_int64 }

  let bin_reader_int64 =
    { read = Read.bin_read_int64
      vtag_read = variant_wrong_type "int64" }

  let bin_int64 =
    { writer = bin_writer_int64
      reader = bin_reader_int64 }

  (*$ mk_base "float" *)
  let bin_writer_float =
    { size = Size.bin_size_float
      write = Write.bin_write_float }

  let bin_reader_float =
    { read = Read.bin_read_float
      vtag_read = variant_wrong_type "float" }

  let bin_float =
    { writer = bin_writer_float
      reader = bin_reader_float }

  (*$ mk_base "int32" *)
  let bin_writer_int32 =
    { size = Size.bin_size_int32
      write = Write.bin_write_int32 }

  let bin_reader_int32 =
    { read = Read.bin_read_int32
      vtag_read = variant_wrong_type "int32" }

  let bin_int32 =
    { writer = bin_writer_int32
      reader = bin_reader_int32 }

  (*$ mk_base "nat0" *)
  let bin_writer_nat0 =
    { size = Size.bin_size_nat0
      write = Write.bin_write_nat0 }

  let bin_reader_nat0 =
    { read = Read.bin_read_nat0
      vtag_read = variant_wrong_type "nat0" }

  let bin_nat0 =
    { writer = bin_writer_nat0
      reader = bin_reader_nat0 }

  (*$ mk_base1 "ref" *)
  let bin_writer_ref bin_writer_el =
    { size = (fun v -> Size.bin_size_ref bin_writer_el.size v)
      write = (fun buf pos v -> Write.bin_write_ref bin_writer_el.write buf pos v) }

  let bin_reader_ref bin_reader_el =
    { read = (fun buf pos_ref -> Read.bin_read_ref bin_reader_el.read buf pos_ref)
      vtag_read = variant_wrong_type "ref" }

  let bin_ref bin_el =
    { writer = bin_writer_ref bin_el.writer
      reader = bin_reader_ref bin_el.reader }

  (*$ mk_base1 "Lazy" *)
  let bin_writer_Lazy (bin_writer_el : 'a writer) =
    { size = (fun (v : 'a Lazy) -> Size.bin_size_Lazy bin_writer_el.size v)
      write = (fun buf pos v -> Write.bin_write_Lazy bin_writer_el.write buf pos v) }

  let bin_reader_Lazy bin_reader_el =
    { read = (fun buf pos_ref -> Read.bin_read_Lazy bin_reader_el.read buf pos_ref)
      vtag_read = variant_wrong_type "Lazy" }

  let bin_Lazy bin_el =
    { writer = bin_writer_Lazy bin_el.writer
      reader = bin_reader_Lazy bin_el.reader }

  (*$ mk_base1 "option" *)
  let bin_writer_option bin_writer_el =
    { size = (fun v -> Size.bin_size_option bin_writer_el.size v)
      write = (fun buf pos v -> Write.bin_write_option bin_writer_el.write buf pos v) }

  let bin_reader_option bin_reader_el =
    { read = (fun buf pos_ref -> Read.bin_read_option bin_reader_el.read buf pos_ref)
      vtag_read = variant_wrong_type "option" }

  let bin_option bin_el =
    { writer = bin_writer_option bin_el.writer
      reader = bin_reader_option bin_el.reader }

  (*$ mk_base2 "pair" *)
  let bin_writer_pair bin_writer_el1 bin_writer_el2 =
    { size = (fun v -> Size.bin_size_pair bin_writer_el1.size bin_writer_el2.size v)
      write =
        (fun buf pos v ->
          Write.bin_write_pair bin_writer_el1.write bin_writer_el2.write buf pos v) }

  let bin_reader_pair bin_reader_el1 bin_reader_el2 =
    { read =
        (fun buf pos_ref ->
          Read.bin_read_pair bin_reader_el1.read bin_reader_el2.read buf pos_ref)
      vtag_read = variant_wrong_type "pair" }

  let bin_pair bin_el1 bin_el2 =
    { writer = bin_writer_pair bin_el1.writer bin_el2.writer
      reader = bin_reader_pair bin_el1.reader bin_el2.reader }

  (*$ mk_base3 "triple" *)
  let bin_writer_triple bin_writer_el1 bin_writer_el2 bin_writer_el3 =
    { size =
        (fun v ->
          Size.bin_size_triple
            bin_writer_el1.size
            bin_writer_el2.size
            bin_writer_el3.size
            v)
      write =
        (fun buf pos v ->
          Write.bin_write_triple
            bin_writer_el1.write
            bin_writer_el2.write
            bin_writer_el3.write
            buf
            pos
            v) }

  let bin_reader_triple bin_reader_el1 bin_reader_el2 bin_reader_el3 =
    { read =
        (fun buf pos_ref ->
          Read.bin_read_triple
            bin_reader_el1.read
            bin_reader_el2.read
            bin_reader_el3.read
            buf
            pos_ref)
      vtag_read = variant_wrong_type "triple" }

  let bin_triple bin_el1 bin_el2 bin_el3 =
    { writer = bin_writer_triple bin_el1.writer bin_el2.writer bin_el3.writer
      reader = bin_reader_triple bin_el1.reader bin_el2.reader bin_el3.reader }

  (*$ mk_base1 "list" *)
  let bin_writer_list bin_writer_el =
    { size = (fun v -> Size.bin_size_list bin_writer_el.size v)
      write = (fun buf pos v -> Write.bin_write_list bin_writer_el.write buf pos v) }

  let bin_reader_list bin_reader_el =
    { read = (fun buf pos_ref -> Read.bin_read_list bin_reader_el.read buf pos_ref)
      vtag_read = variant_wrong_type "list" }

  let bin_list bin_el =
    { writer = bin_writer_list bin_el.writer
      reader = bin_reader_list bin_el.reader }

  (*$ mk_base1 "array" *)
  let bin_writer_array bin_writer_el =
    { size = (fun v -> Size.bin_size_array bin_writer_el.size v)
      write = (fun buf pos v -> Write.bin_write_array bin_writer_el.write buf pos v) }

  let bin_reader_array bin_reader_el =
    { read = (fun buf pos_ref -> Read.bin_read_array bin_reader_el.read buf pos_ref)
      vtag_read = variant_wrong_type "array" }

  let bin_array bin_el =
    { writer = bin_writer_array bin_el.writer
      reader = bin_reader_array bin_el.reader }

  (*$ mk_base "variant_int" *)
  let bin_writer_variant_int =
    { size = Size.bin_size_variant_int
      write = Write.bin_write_variant_int }

  let bin_reader_variant_int =
    { read = Read.bin_read_variant_int
      vtag_read = variant_wrong_type "variant_int" }

  let bin_variant_int =
    { writer = bin_writer_variant_int
      reader = bin_reader_variant_int }

  (*$ mk_base "int_8bit" *)
  let bin_writer_int_8bit =
    { size = Size.bin_size_int_8bit
      write = Write.bin_write_int_8bit }

  let bin_reader_int_8bit =
    { read = Read.bin_read_int_8bit
      vtag_read = variant_wrong_type "int_8bit" }

  let bin_int_8bit =
    { writer = bin_writer_int_8bit
      reader = bin_reader_int_8bit }

  (*$ mk_base "int_16bit" *)
  let bin_writer_int_16bit =
    { size = Size.bin_size_int_16bit
      write = Write.bin_write_int_16bit }

  let bin_reader_int_16bit =
    { read = Read.bin_read_int_16bit
      vtag_read = variant_wrong_type "int_16bit" }

  let bin_int_16bit =
    { writer = bin_writer_int_16bit
      reader = bin_reader_int_16bit }

  (*$ mk_base "int_32bit" *)
  let bin_writer_int_32bit =
    { size = Size.bin_size_int_32bit
      write = Write.bin_write_int_32bit }

  let bin_reader_int_32bit =
    { read = Read.bin_read_int_32bit
      vtag_read = variant_wrong_type "int_32bit" }

  let bin_int_32bit =
    { writer = bin_writer_int_32bit
      reader = bin_reader_int_32bit }

  (*$ mk_base "int_64bit" *)
  let bin_writer_int_64bit =
    { size = Size.bin_size_int_64bit
      write = Write.bin_write_int_64bit }

  let bin_reader_int_64bit =
    { read = Read.bin_read_int_64bit
      vtag_read = variant_wrong_type "int_64bit" }

  let bin_int_64bit =
    { writer = bin_writer_int_64bit
      reader = bin_reader_int_64bit }

  (*$ mk_base "network16_int" *)
  let bin_writer_network16_int =
    { size = Size.bin_size_network16_int
      write = Write.bin_write_network16_int }

  let bin_reader_network16_int =
    { read = Read.bin_read_network16_int
      vtag_read = variant_wrong_type "network16_int" }

  let bin_network16_int =
    { writer = bin_writer_network16_int
      reader = bin_reader_network16_int }

  (*$ mk_base "network32_int" *)
  let bin_writer_network32_int =
    { size = Size.bin_size_network32_int
      write = Write.bin_write_network32_int }

  let bin_reader_network32_int =
    { read = Read.bin_read_network32_int
      vtag_read = variant_wrong_type "network32_int" }

  let bin_network32_int =
    { writer = bin_writer_network32_int
      reader = bin_reader_network32_int }

  (*$ mk_base "network64_int" *)
  let bin_writer_network64_int =
    { size = Size.bin_size_network64_int
      write = Write.bin_write_network64_int }

  let bin_reader_network64_int =
    { read = Read.bin_read_network64_int
      vtag_read = variant_wrong_type "network64_int" }

  let bin_network64_int =
    { writer = bin_writer_network64_int
      reader = bin_reader_network64_int }

  (* Conversion of binable types *)

  let cnv_writer cnv tp_class =
    { size = (fun v -> tp_class.size (cnv v))
      write = (fun buf pos v -> tp_class.write buf pos (cnv v)) }

  let cnv_reader cnv tp_class =
    { read = (fun buf pos_ref -> cnv (tp_class.read buf pos_ref))
      vtag_read = (fun buf pos_ref vtag -> cnv (tp_class.vtag_read buf pos_ref vtag)) }

  let cnv for_writer for_reader tp_class =
    { writer = cnv_writer for_writer tp_class.writer
      reader = cnv_reader for_reader tp_class.reader }

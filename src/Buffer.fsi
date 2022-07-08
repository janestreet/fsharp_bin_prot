namespace Bin_prot

module Buffer =

  open System

  type Buffer<'a> =
    new : 'a [] -> Buffer<'a>
    new : int -> Buffer<'a>

    member Length : int

    member Item : pos : int -> 'a

    member Set : pos : int -> 'a -> unit

    member Buffer : 'a []

    member Slice : pos : int * len : int -> ReadOnlySpan<'a>

    member WritableSlice : pos : int * len : int -> Span<'a>

    member CopyMemory : src : 'a [] * pos : int * len : int -> unit

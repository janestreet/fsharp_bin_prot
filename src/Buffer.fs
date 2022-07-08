namespace Bin_prot

module Buffer =

  open System

  type Buffer<'a> (buffer : 'a []) =

    new (len : int) = Buffer<'a>(Array.zeroCreate len)

    member val Length = buffer.Length

    member this.Item pos = buffer.[pos]
    member this.Set pos byte = buffer.[pos] <- byte

    member this.Slice(pos, len) = ReadOnlySpan<'a>(buffer, pos, len)

    member this.WritableSlice(pos, len) = Span<'a>(buffer, pos, len)

    member this.CopyMemory((src : 'a []), pos, len) = Array.Copy(src, 0, buffer, pos, len)

    member this.Buffer = buffer

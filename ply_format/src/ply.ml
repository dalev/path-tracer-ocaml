open! Base
open Stdio

module Header = struct
  type t = string list
end

let of_in_channel (_ic : In_channel.t) = failwith "to do"

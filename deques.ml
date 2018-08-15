module Buffers = struct
  type ('a, _) buffer =
    | Buffer0 :                           ('a,    [`Red]) buffer
    | Buffer1 : 'a                     -> ('a, [`Yellow]) buffer
    | Buffer2 : 'a * 'a                -> ('a,  [`Green]) buffer
    | Buffer3 : 'a * 'a * 'a           -> ('a,  [`Green]) buffer
    | Buffer4 : 'a * 'a * 'a * 'a      -> ('a, [`Yellow]) buffer
    | Buffer5 : 'a * 'a * 'a * 'a * 'a -> ('a,    [`Red]) buffer
end
open Buffers

type 'a somebuffer =
  | Buffer : ('a, 'x) buffer -> 'a somebuffer
type z = int somebuffer
(* let f b = match b with
 *     Buffer0 -> 42
 *   | Buffer1 x -> 42
 *   | Buffer2 (x, y) -> 42
 *   | Buffer3 (x, y, z) -> 42
 *   | Buffer4 (x, y, z, t) -> 42
 *   | Buffer5 (x, y, z, t, u) -> 42 *)

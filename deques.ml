(** An implementation of purely functional deques as described by Kaplan and
   Tarjan. *)

(** jacm-final.pdf p.8 (584) §4.1 media 60 380 368 40 *)
module Buffers = struct
  type 'a buffer0 = unit
  type 'a buffer1 = 'a
  type 'a buffer2 = 'a * 'a
  type 'a buffer3 = 'a * 'a * 'a
  type 'a buffer4 = 'a * 'a * 'a * 'a
  type 'a buffer5 = 'a * 'a * 'a * 'a * 'a
end
open Buffers

(** jacm-final.pdf p.9 (585) §4.1 media 60 158 368 24 *)
module Colors = struct
  type 'a green  = Buffer2 of 'a buffer2 | Buffer3 of 'a buffer3
  type 'a yellow = Buffer1 of 'a buffer1 | Buffer4 of 'a buffer4
  type 'a red    = Buffer0 of 'a buffer0 | Buffer5 of 'a buffer5
end
open Colors

(** jacm-final.pdf p.9 (585) §4.1 media 60 134 368 36 *)
type 'a buffer =
  Green  of 'a green
| Yellow of 'a yellow
| Red    of 'a red

(** jacm-final.pdf p.8 / 584 §4.1 60 408 368 48 *)
type 'a deque = {
  prefix: 'a buffer;
  child:  ('a * 'a) deque option;
  suffix: 'a buffer;
}

(* jacm-final.pdf p.8 / 584 §4.1 60 480 368 36 *)

(* let rec child i d =
     if i = 0 then d
     else child (i - 1) d.child *)

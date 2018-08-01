(** An implementation of purely functional deques as described by Kaplan and
   Tarjan. *)

(** jacm-final.pdf p.8 (584) §4.1 media 60 380 368 40 *)
module Buffers = struct
  type 'a buffer0 = [`Buffer0]
  type 'a buffer1 = [`Buffer1 of 'a]
  type 'a buffer2 = [`Buffer2 of 'a * 'a]
  type 'a buffer3 = [`Buffer3 of 'a * 'a * 'a]
  type 'a buffer4 = [`Buffer4 of 'a * 'a * 'a * 'a]
  type 'a buffer5 = [`Buffer5 of 'a * 'a * 'a * 'a * 'a]
end
open Buffers

(** jacm-final.pdf p.9 (585) §4.1 media 60 158 368 24 *)
module Colors = struct
  type 'a green  = ['a buffer2 | 'a buffer3]
  type 'a yellow = ['a buffer1 | 'a buffer4]
  type 'a red    = ['a buffer0 | 'a buffer5]
end
open Colors

(** jacm-final.pdf p.9 (585) §4.1 media 60 134 368 36 *)
type 'a buffer = [
    `Green  of 'a green
  | `Yellow of 'a yellow
  | `Red    of 'a red
]

(** jacm-final.pdf p.8 / 584 §4.1 60 408 368 48 *)
type 'a deque = {
  prefix: 'a buffer;
  child:  ('a * 'a) deque option;
  suffix: 'a buffer;
}

(** jacm-final.pdf p.9 (585) §4.1 media 60 206 368 12 *)
module ColorOrder = struct
  type nonrec 'a green  = [`Green  of 'a green]
  type nonrec 'a yellow = [`Yellow of 'a yellow]
  type nonrec 'a red    = [`Red    of 'a red]
  
  type 'a le_green  = [ | 'a green | 'a yellow | 'a red]
  type 'a le_yellow = [ |            'a yellow | 'a red]
  type 'a le_red    = [ |                        'a red]
  
  type 'a ge_green  = [ | 'a green                     ]
  type 'a ge_yellow = [ | 'a green | 'a yellow         ]
  type 'a ge_red    = [ | 'a green | 'a yellow | 'a red]
end
open ColorOrder

type 'a nested_deque = [`Some of ('a * 'a) deque]
type ('prefix, 'nested, 'suffix) deque2 = {
  prefix: 'prefix;
  child:  'nested;
  suffix: 'suffix;
}

(** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *)
type ('a, 'color, 'le_color) colored_buffer =
  [  `Full of ('color,     'a nested_deque, 'le_color)  deque2
   | `L    of ('a buffer0, [`None],         'le_color)  deque2
   | `R    of ('color,     [`None],         'a buffer0) deque2]
type 'a greenDeque  = [`Green  of ('a, 'a green,  'a le_green)  colored_buffer]
type 'a yellowDeque = [`Yellow of ('a, 'a yellow, 'a le_yellow) colored_buffer]
type 'a redDeque    = [`Red    of ('a, 'a red,    'a le_red)    colored_buffer]

(* TODO: color constraints for the root and collapsing of intermediate sequences of yellow *)
type 'a deque3 = ['a greenDeque | 'a yellowDeque | 'a redDeque]

(* jacm-final.pdf p.8 / 584 §4.1 60 480 368 36 *)

(* let rec child i d =
     if i = 0 then d
     else child (i - 1) d.child *)

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
  
  type 'a leGreen  = [ | 'a green | 'a yellow | 'a red]
  type 'a leYellow = [ |            'a yellow | 'a red]
  type 'a leRed    = [ |                        'a red]
  
  type 'a geGreen  = [ | 'a green                     ]
  type 'a geYellow = [ | 'a green | 'a yellow         ]
  type 'a geRed    = [ | 'a green | 'a yellow | 'a red]
end
open ColorOrder

type 'a nestedDeque = [`Some of ('a * 'a) deque]
type ('prefix, 'substack, 'stack, 'suffix) deque2 = {
  prefix:   'prefix;
  substack: 'substack;
  stack:    'stack;
  suffix:   'suffix;
}






(** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *)
module ColoredDeques = struct
  type ('a, 'color, 'leColor) coloredDeque =
    [  `Full of ('color,     'a nestedDeque, 'leColor)  deque2 (* TODO: the nested deque should be a recursive type *)
     (* TODO: `L and `R should be merged into a single case + constructor inside, and the empty parts the info deleted *)
     | `L    of ('a buffer0, [`None],         'leColor)  deque2
     | `R    of ('color,     [`None],         'a buffer0) deque2] constraint 'xyz = 'xyz (* TODO: use constraint to allow or forbid the empty child (L/R) case for yellow. *)
  type 'a greenDeque  = [`Green  of ('a, 'a green,  'a leGreen)  coloredDeque]
  type 'a yellowDeque = [`Yellow of ('a, 'a yellow, 'a leYellow) coloredDeque]
  type 'a redDeque    = [`Red    of ('a, 'a red,    'a leRed)    coloredDeque]
end
open ColoredDeques







(** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *)
module ColoredDeques2 = struct
  type ('a, 'color, 'leColor, 'substack, 'stack) coloredDeque =
    [  `Full of ('color,     'substack, 'stack,    'leColor) deque2]
  (** The astute reader will notice that if the child of a deque is empty,
     then it is the first (deepest) element on the stack. Therefore, the cases
     considering that the colour of a deque is that of its only non-empty
     buffer, if its child is also empty, apply only to the first element of the
     stack. *)
  type ('a, 'color, 'leColor, 'substack, 'stack) coloredLRDeque = [
       ('a, 'color, 'leColor, 'substack, 'stack) coloredDeque
     | `L    of ('a buffer0, [`None],   [`None],   'leColor) deque2
     | `R    of ('color,     [`None],   [`None], 'a buffer0) deque2]

  (* "Error: The type constructor yellowDeque is not yet completely defined"
     if we don't use a fixup: all expanded cases in […|…] have to be
     completely defined (or wrapped with a `X so that they need not be
     substituted) *)
  type 'a greenDeque  = ('a, 'a green,  'a leGreen,  [`None|`Yellow of 'a yellowDeque], [`None | `Green of 'a greenDeque |   `Red of 'a   redDeque]) coloredDeque
  and  'a yellowDeque = ('a, 'a yellow, 'a leYellow, [`None|`Yellow of 'a yellowDeque], [`None]) coloredDeque
  and  'a redDeque    = ('a, 'a red,    'a leRed,    [`None|`Yellow of 'a yellowDeque], [`None | `Green of 'a greenDeque]) coloredDeque
end
open ColoredDeques2

module ColoredDequesFixup = struct
  type nonrec 'a greenDeque  = [`Green  of 'a greenDeque ]
  and         'a yellowDeque = [`Yellow of 'a yellowDeque]
  and         'a redDeque    = [`Red    of 'a redDeque   ]
end
open ColoredDequesFixup


(* jacm-final.pdf p.8 / 584 §4.1 60 480 368 36 *)

(* let rec child i d =
     if i = 0 then d
     else child (i - 1) d.child *)

(* TODO: what is the state of the `None once there are two pointers? *)
(* TODO: Problem: a yellow at the bottom of its substack needs to know what's beneath to know
   if the "no child + one empty buffer" case is allowed. *)
(* TODO: show the naïve (wrong) version here. *)

(* type ('a, 'color, 'leColor, 'nested) colored9 =
 *   [  `Full of ('color,     'nestedSubstack, 'nestedStack, 'leColor)  deque2
 *    | `L    of ('a buffer0, [`None],          'nestedStack, 'leColor)  deque2
 *    | `R    of ('color,     [`None],          'nestedStack, 'a buffer0) deque2]
 * type 'a greenDeque  = [`Green  of ('a, 'a green,  'a leGreen)  colored9]
 * type 'a yellowDeque = [`Yellow of ('a, 'a yellow, 'a leYellow) colored9]
 * type 'a redDeque    = [`Red    of ('a, 'a red,    'a leRed)    colored9] *)

(* type 'a yellowDeque    = [`Yellow of ('a, 'a yellow, 'a leYellow, [`Some of ('a * 'a) nonYellowDeque])   coloredDeque]
 * and  'a greenDeque     = [`Green  of ('a, 'a green,  'a leGreen,  [`Some of ('a * 'a) yellowDeque list]) coloredDeque]
 * and  'a redDeque       = [`Red    of ('a, 'a red,    'a leRed,    [`Some of ('a * 'a) yellowDeque list]) coloredDeque]
 * and  'a nonYellowDeque = ['a greenDeque | 'a redDeque] *)

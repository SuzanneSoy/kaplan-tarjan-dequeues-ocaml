(** An implementation of purely functional deques as described by Kaplan and
   Tarjan. *)

(** jacm-final.pdf p.8 (584) §4.1 media 60 380 368 40 *)
module Buffers = struct
  type    buffer0 = [`Buffer0]
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
  type 'a red    = [   buffer0 | 'a buffer5]
  (* In addition to the colors defined by Kaplan and Tarjan, we define the
     shorhand "empty" for buffers of size zero (which are used later on). *)
  type    empty  = [           |    buffer0]
end
open Colors

(** jacm-final.pdf p.9 (585) §4.1 media 60 206 368 12 *)
module ColorOrder = struct
  type nonrec 'a green  = [`Green  of 'a green]
  type nonrec 'a yellow = [`Yellow of 'a yellow]
  type nonrec 'a red    = [`Red    of 'a red]
  type nonrec    empty  = [`Red    of    empty]
  
  type 'a leGreen  = [ | 'a green | 'a yellow | 'a red]
  type 'a leYellow = [ |            'a yellow | 'a red]
  type 'a leRed    = [ |                        'a red]
  
  type 'a geGreen  = [ | 'a green                     ]
  type 'a geYellow = [ | 'a green | 'a yellow         ]
  type 'a geRed    = [ | 'a green | 'a yellow | 'a red]
end
open ColorOrder

(** jacm-final.pdf p.9 (585) §4.1 media 60 134 368 36 *)
module BufferColors = struct
  type 'a buffer = ['a green | 'a yellow | 'a red]
end
open BufferColors

(** jacm-final.pdf p.8 / 584 §4.1 60 408 368 48 *)
module Deques = struct
  (* We use polymorphic variants instead of the usual 't option so that it is
     possible later to indicate in which contexts a deque must or must not
     have a non-empty child. *)
  type 'a deque = {
    prefix: 'a buffer;
    child:  [`Child of ('a * 'a) deque | `NoChild];
    suffix: 'a buffer;
    }
  (* We can't easily factor this, as OCaml cannot inline polymorphic variants
     before they are completely defined. *)
  type 'a child   = [`Child of ('a * 'a) deque]
  type    noChild = [`NoChild]
  let prefix d = d.prefix
  let child  d = d.child
  let suffix d = d.suffix
end
open Deques

module DequesColors = struct
  type ('prefix, 'child, 'suffix) deque = {
    prefix: 'prefix;
    child:  'child;
    suffix: 'suffix;
  }
  (** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *)
  type ('color, 'geColor, 'child) min = [
      `LeftMin   of ('color,   [`Child of 'child | `NoChild],          'geColor) deque
    | `RightMin  of ('geColor, [`Child of 'child | `NoChild],            'color) deque
    (* When the 'color is red, the two cases below are partially redundant
       with the ones above. For example, a buffer0,`NoChild,buffer5 dequeue
       could be labeled as `RightOnly or `LeftMin or `RightMin. This does not
       affect the safety properties, but it would be nicer if we had an
       unambiguous labeling. *)
    | `LeftOnly  of ('color,   [                   `NoChild],             empty) deque
    | `RightOnly of ( empty,   [                   `NoChild],            'color) deque
    ] 

  (** jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  (* If we try to inline and merge together polymorphic variants (e.g. ['a
     greenDeque|'yellowGDeque]) which are defined as mutually-recursive types,
     OCamle gives the following error: *)
  (* "Error: The type constructor yellowDeque is not yet completely defined"
     We therefore first define the types by moving the [`X of …] at the
     use-site, and stripping it away from the definition site. Then, we wrap
     the types defined that way in order to put back the [`X of …] around each
     type. *)
  (* Since OCaml does not allow types to be shadowed, we define the first
     unwrapped version of the types in a submodule. Types which are made
     available in the current scope by using "open" can be shadowed by later
     uses of "open" and later definitions. *)
  module ToFixup = struct
    type 'a greenDeque    = ('a green,  'a geGreen,  [ `Green  of 'a greenDeque
                                                     | `Yellow of 'a yellowGRDeque
                                                     | `Red    of 'a redDeque     ]) min
    and  'a yellowGDeque  = ('a yellow, 'a geYellow, [ `Green  of 'a greenDeque
                                                     | `Yellow of 'a yellowGDeque ]) min
    and  'a yellowGRDeque = ('a yellow, 'a geYellow, [ `Green  of 'a greenDeque
                                                     | `Yellow of 'a yellowGRDeque
                                                     | `Red    of 'a redDeque     ]) min
    and  'a redDeque      = ('a red,    'a geRed,    [ `Green  of 'a greenDeque
                                                     | `Yellow of 'a yellowGDeque ]) min
  end
  open ToFixup
  type nonrec 'a greenDeque    = [`Green  of 'a    greenDeque]
  type nonrec 'a yellowGDeque  = [`Yellow of 'a  yellowGDeque]
  type nonrec 'a yellowGRDeque = [`Yellow of 'a yellowGRDeque]
  type nonrec 'a redDeque      = [`Red    of 'a      redDeque]

  (** * jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  type 'a semiregular = ['a greenDeque | 'a yellowGRDeque | 'a redDeque]
  (** * jacm-final.pdf p.9 (585) §4.1 media 60 338 368 24 *)
  type 'a regular     = ['a greenDeque | 'a yellowGDeque  | 'a redDeque]
end
open DequesColors














(* module DequeWithSubstack = struct
 *   type 'a nestedDeque = [`Some of ('a * 'a) deque]
 *   type ('prefix, 'substack, 'stack, 'suffix) deque = {
 *       prefix:   'prefix;
 *       substack: 'substack;
 *       stack:    'stack;
 *       suffix:   'suffix;
 *     }
 * end
 * open DequeWithSubstack *)




(* module ColoredDeques = struct
 *   type ('a, 'color, 'leColor) coloredDeque =
 *     [  `Full of ('color,     'a nestedDeque, 'leColor)  deque (\* TODO: the nested deque should be a recursive type *\)
 *      (\* TODO: `L and `R should be merged into a single case + constructor inside, and the empty parts the info deleted *\)
 *      | `L    of ('a buffer0, [`None],         'leColor)  deque
 *      | `R    of ('color,     [`None],         'a buffer0) deque] constraint 'xyz = 'xyz (\* TODO: use constraint to allow or forbid the empty child (L/R) case for yellow. *\)
 *   type 'a greenDeque  = [`Green  of ('a, 'a green,  'a leGreen)  coloredDeque]
 *   type 'a yellowDeque = [`Yellow of ('a, 'a yellow, 'a leYellow) coloredDeque]
 *   type 'a redDeque    = [`Red    of ('a, 'a red,    'a leRed)    coloredDeque]
 * end
 * open ColoredDeques *)







(* (\** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *\)
 * module ColoredDeques2 = struct
 *   type ('a, 'color, 'leColor, 'substack, 'stack) coloredDeque =
 *     [  `Full of ('color,     'substack, 'stack,    'leColor) deque]
 *   (\** The astute reader will notice that if the child of a deque is empty,
 *      then it is the first (deepest) element on the stack. Therefore, the cases
 *      considering that the colour of a deque is that of its only non-empty
 *      buffer, if its child is also empty, apply only to the first element of the
 *      stack. *\)
 *   type ('a, 'color, 'leColor, 'substack, 'stack) coloredLRDeque = [
 *        ('a, 'color, 'leColor, 'substack, 'stack) coloredDeque
 *      | `L    of ('a buffer0, [`None],   [`None],   'leColor) deque
 *      | `R    of ('color,     [`None],   [`None], 'a buffer0) deque]
 * 
 *   (\* "Error: The type constructor yellowDeque is not yet completely defined"
 *      if we don't use a fixup: all expanded cases in […|…] have to be
 *      completely defined (or wrapped with a `X so that they need not be
 *      substituted) *\)
 *   type 'a greenDeque  = ('a, 'a green,  'a leGreen,  [`Yellow of 'a yellowDeque], [`Green of 'a greenDeque|`Red of 'a redDeque]) coloredDeque
 *   and  'a yellowDeque = ('a, 'a yellow, 'a leYellow, [`Yellow of 'a yellowDeque], [`None]) coloredDeque (\*  The first deque, if it is yellow, may have a stack pointer which is not `None. *\)
 *   and  'a redDeque    = ('a, 'a red,    'a leRed,    [`Yellow of 'a yellowDeque], [`Green of 'a greenDeque]) coloredDeque
 * end
 * open ColoredDeques2
 * 
 * module ColoredDequesFixup = struct
 *   type nonrec 'a greenDeque  = [`Green  of 'a greenDeque ]
 *   and         'a yellowDeque = [`Yellow of 'a yellowDeque]
 *   and         'a redDeque    = [`Red    of 'a redDeque   ]
 * end
 * open ColoredDequesFixup *)


(* jacm-final.pdf p.8 / 584 §4.1 60 480 368 36 *)

(* let rec child i d =
     if i = 0 then d
     else child (i - 1) d.child *)

(* TODO: what is the state of the `None once there are two pointers? *)
(* TODO: Problem: a yellow at the bottom of its substack needs to know what's beneath to know
   if the "no child + one empty buffer" case is allowed. *)
(* TODO: show the naïve (wrong) version here. *)

(* type ('a, 'color, 'leColor, 'nested) colored9 =
 *   [  `Full of ('color,     'nestedSubstack, 'nestedStack, 'leColor)  deque
 *    | `L    of ('a buffer0, [`None],          'nestedStack, 'leColor)  deque
 *    | `R    of ('color,     [`None],          'nestedStack, 'a buffer0) deque]
 * type 'a greenDeque  = [`Green  of ('a, 'a green,  'a leGreen)  colored9]
 * type 'a yellowDeque = [`Yellow of ('a, 'a yellow, 'a leYellow) colored9]
 * type 'a redDeque    = [`Red    of ('a, 'a red,    'a leRed)    colored9] *)

(* type 'a yellowDeque    = [`Yellow of ('a, 'a yellow, 'a leYellow, [`Some of ('a * 'a) nonYellowDeque])   coloredDeque]
 * and  'a greenDeque     = [`Green  of ('a, 'a green,  'a leGreen,  [`Some of ('a * 'a) yellowDeque list]) coloredDeque]
 * and  'a redDeque       = [`Red    of ('a, 'a red,    'a leRed,    [`Some of ('a * 'a) yellowDeque list]) coloredDeque]
 * and  'a nonYellowDeque = ['a greenDeque | 'a redDeque] *)

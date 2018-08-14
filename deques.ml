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
  type    empty  =     buffer0
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

(** jacm-final.pdf p.9 (585) §4.1 media 60 146 368 24 *)
module BufferColors = struct
  type 'a buffer = ['a green | 'a yellow | 'a red]
end
open BufferColors

(** jacm-final.pdf p.8 (584) §4.1 media 60 408 368 48 *)
module Deques = struct
  (* We use polymorphic variants instead of the usual 't option so that it is
     possible later to indicate in which contexts a deque must or must not
     have a non-empty child. *)
  type ('prefix, 'substack, 'suffix) deque = {
      prefix:   'prefix;
      substack: 'substack;
      suffix:   'suffix}
  type 'a child = 'a * 'a
end
open Deques

module DequesColorsStack = struct

  (** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *)
  (* TODO: this is ambiguous: when the colors are the same, or for the empty
     cases, several constructors could be used for the same pair of
     buffers. *)
  type ('color, 'geColor, 'substack) min = [
      `LeftMin   of ('color,   'substack, 'geColor) deque
    | `RightMin  of ('geColor, 'substack,   'color) deque]
  type ('color, 'geColor, 'substack) lastMin = [
      ('color, 'geColor, 'substack) min
    | `LeftOnly  of ('color,   [`Nil],       empty) deque
    | `RightOnly of ( empty,   [`Nil],      'color) deque]

  type 'a minTail     = Deque1 of ('a yellow, 'a geYellow, 'a child minTail) min
  type 'a lastMinTail = Deque2 of ('a yellow, 'a geYellow, 'a child lastMinTail) lastMin
  type ('a, 'color, 'geColor) min'     = ('color, 'geColor, 'a child minTail) min
  type ('a, 'color, 'geColor) lastMin' = ('color, 'geColor, 'a child lastMinTail) lastMin

  (** jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  type ('a, 'color, 'geColor, 'b_last) substack = 'result
  constraint 'b_last = (  'a, 'color,  'geColor) min'
                       * ('a, 'color,  'geColor) lastMin'
                       * 'result
  
  type ('a, 'b_last) greenSubstack  = ('a, 'a green,  'a geGreen,  'b_last) substack
  (* A yellow substack can only appear as the topmost substack in a stack. *)
  type ('a, 'b_last) yellowSubstack = ('a, 'a yellow, 'a geYellow, 'b_last) substack
  type ('a, 'b_last) redSubstack    = ('a, 'a red,    'a geRed,    'b_last) substack

  (** jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  (* In order to simplify the description of the types, we have separated the
     outer stack from the substacks. *)
  type 'a gStackLast   = [ `GreenStackLast  of ('a, 't * 'f * 't) greenSubstack  * [`Null]]
  type 'a yStackLast   = [ `YellowStackLast of ('a, 't * 'f * 't) yellowSubstack * [`Null]]
  type 'a rStackLast   = [ `RedStackLast    of ('a, 't * 'f * 't) redSubstack    * [`Null]]
  type ('a, 'b) _gStack  = [ 'a gStackLast
                           | `GreenStack  of ('a, 't * 'f * 'f)  greenSubstack * 'b]
  type ('a, 'b) _yStack  = [ 'a yStackLast
                           | `YellowStack of ('a, 't * 'f * 'f) yellowSubstack * 'b]
  type ('a, 'b) _rStack  = [ 'a rStackLast
                           | `RedStack    of ('a, 't * 'f * 'f)    redSubstack * 'b]
  type ('a, 'b, 'c) _grStack = [('a, 'b) _gStack | ('a, 'c) _rStack]

  type 'a      greenStack = ('a, 'a greenOrRedStack) _gStack
  and  'a    yellowGStack = ('a, 'a      greenStack) _yStack
  and  'a   yellowGRStack = ('a, 'a greenOrRedStack) _yStack
  and  'a        redStack = ('a, 'a        redStack) _rStack
  and  'a greenOrRedStack = ('a, 'a greenOrRedStack, 'a redStack) _grStack

  (* Contrarily to substacks, the tail of a stack may start with different
     colors: green is allways possible, but some stack elements may be
     followed by a red stack element. We therefore need to introduce
     polymorphic constructors in order to make a union of the two cases. In
     practice, these cases are already disjoint types, but performing their
     union is more readily done by defining a variant at this level. *)

  type 'a semiregular = ['a greenStack | 'a yellowGRStack | 'a redStack]
  (** jacm-final.pdf p.9 (585) §4.1 media 60 338 368 24 *)
  type 'a regular     = ['a greenStack | 'a yellowGStack  | 'a redStack]
end
open DequesColorsStack

(** jacm-final.pdf p.8 (584) §4.1 media 60 480 368 36 *)
(* let rec child i d =
     if i = 0 then d
     else child (i - 1) d.child *)

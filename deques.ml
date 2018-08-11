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

(* If we naively translate Kaplan and Tarjan's constraints into a set of
   polymorphic variants, we might end up with some variation of the following,
   which describes the type of a (semi-)regular stack of deques, where the
   stack's pointers are one of the fields of its elements. This first attempt
   only implements a single stack of deques, not a stack of substacks (where
   the elements of the stack are linked by one of two pointers). *)
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

(* As we can see, the above translation defines the semiregular and regular
   types as a variant of variants, with 10 cases in total in the
   definition. Substacks will introduce a number of corner cases, and the
   above definition could become a long list of those. *)
(* Instead, we choose to separate the concerns related to pairs of buffers,
   substacks and stacks. Each level of the construction has its own corner
   cases, which will be to some extent kept separate. We first start by
   updating the definition of deques so that the stack becomes a separate,
   apparent structure. *)
module DequesColorsStack = struct
  type ('prefix, 'substack, 'suffix) deque = {
      prefix:   'prefix;
      substack: 'substack;
      suffix:   'suffix}

  (** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *)
  type ('color, 'geColor, 'substack) min = [
      `LeftMin   of ('color,   'substack, 'geColor) deque
    | `RightMin  of ('geColor, 'substack,   'color) deque]
  type ('color, 'geColor, 'substack) lastMin = [
      ('color, 'geColor, 'substack) min
    | `LeftOnly  of ('color,   [`Nil],       empty) deque
    | `RightOnly of ( empty,   [`Nil],      'color) deque]

  type 'a minTail = ('a yellow, 'a geYellow, 'a minTail) min
  type ('a, 'color, 'geColor) min'     = ('color, 'geColor, 'a minTail) min
  type ('a, 'color, 'geColor) lastMin' = ('color, 'geColor, 'a minTail) lastMin

  (** jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  (* TODO: move the decision just around min and lastMin, and merge min' with lastMin' *)
  type ('a, 'color, 'geColor, 'b_last) substack = 'result
  constraint 'b_last = (  'a, 'color,  'geColor) min'
                       * ('a, 'color,  'geColor) lastMin'
                       * 'result
  
  type ('a, 'b_last) greenSubstack  = ('a, 'a green,  'a geGreen,  'b_last) substack
  (* A yellow substack can only appear as the topmost substack in a stack. *)
  type ('a, 'b_last) yellowSubstack = ('a, 'a yellow, 'a geYellow, 'b_last) substack
  type ('a, 'b_last) redSubstack    = ('a, 'a red,    'a geRed,    'b_last) substack

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

  (** * jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  type 'a semiregular = ['a greenStack | 'a yellowGRStack | 'a redStack]
  (** * jacm-final.pdf p.9 (585) §4.1 media 60 338 368 24 *)
  type 'a regular     = ['a greenStack | 'a yellowGStack  | 'a redStack]
end
open DequesColorsStack

(** jacm-final.pdf p.8 (584) §4.1 media 60 480 368 36 *)
(* let rec child i d =
     if i = 0 then d
     else child (i - 1) d.child *)

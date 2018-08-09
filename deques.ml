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
  type ('prefix, 'suffix) deque = {
    prefix: 'prefix;
    suffix: 'suffix;
  }
  (** jacm-final.pdf p.9 (585) §4.1 media 60 230 368 48 *)
  type ('color, 'geColor) min = [
      `LeftMin   of ('color,   'geColor) deque
    | `RightMin  of ('geColor,   'color) deque
  ] 
  type ('color, 'geColor) lastMin = [
      ('color, 'geColor) min
    | `LeftOnly  of ('color,      empty) deque
    | `RightOnly of ( empty,     'color) deque
  ] 

  (* TODO: use a functor to fill in the <min> part which should be min or lastMin *)
  (** jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  type 'a greenDeque  = ('a green,  'a geGreen)  min
  type 'a yellowDeque = ('a yellow, 'a geYellow) min
  type 'a redDeque    = ('a red,    'a geRed)    min

  type 'a tailSubStack    = [`Cons of 'a   yellowDeque * 'a tailSubStack]
  type ('hd, 'a) subStack = [`Cons of 'hd              * 'a tailSubStack]
  type 'a greenSubStack   = ('a  greenDeque, 'a) subStack
  (* A yellow substack can only appear as the topmost substack in a stack. *)
  type 'a yellowSubStack  = ('a yellowDeque, 'a) subStack
  type 'a redSubStack     = ('a    redDeque, 'a) subStack

  (* Contrarily to substacks, the tail of a stack may start with different
     colors: green is allways possible, but some stack elements may be
     followed by a red stack element. We therefore need to introduce
     polymorphic constructors in order to make a union of the two cases. In
     practice, these cases are already disjoint types, but performing their
     union is more readily done by defining a variant at this level. *)
  module ToFixup = struct
    type 'a gStackTail    = [`GreenStack of 'a greenStack                            | `Null]
    and  'a gRStackTail   = [`GreenStack of 'a greenStack | `RedStack of 'a redStack | `Null]
    and  'a greenStack    = 'a greenSubStack  * 'a gRStackTail
    and  'a yellowGStack  = 'a yellowSubStack * 'a gStackTail
    and  'a yellowGRStack = 'a yellowSubStack * 'a gRStackTail
    and  'a redStack      = 'a redSubStack    * 'a gStackTail
  end
  open ToFixup
  type nonrec 'a greenStack    = [`GreenStack  of 'a    greenStack]
  type nonrec 'a yellowGStack  = [`YellowStack of 'a  yellowGStack]
  type nonrec 'a yellowGRStack = [`YellowStack of 'a yellowGRStack]
  type nonrec 'a redStack      = [`RedStack    of 'a      redStack]

  (** * jacm-final.pdf p.9 (585) §4.1 media 60 290 368 60 *)
  type 'a semiregular = ['a greenStack | 'a yellowGRStack | 'a redStack]
  (** * jacm-final.pdf p.9 (585) §4.1 media 60 338 368 24 *)
  type 'a regular     = ['a greenStack | 'a yellowGStack  | 'a redStack]
end
open DequesColorsStack

module TypeLevelFunctions1 = struct
  (* TODO: bundle together the stack and an on-demand infinite stack of free variables *)
  (* This should not be exported in the sig. *)
  module Private = struct
    (* stack of type-level operands *)
    type start = ()
    type ('head, 'tail) stk    = Stk of 'head * 'tail (* constraint 'tail = ('a,'b) stk *)

    (* internal: quote a type and place it on the stack *)
    type 't _typ = Typ of 't
  end
  open Private

  (* unwrap the single element on the stack *)
  type 'stk return = 'returned constraint 'stk = (start, 'returned _typ) stk

  (* quote a type and place it on the stack *)
  type ('stk, 't) typ  = ('stk, 't _typ) stk

  (* type-level booleans *)
  type ('stk, 'freevar) tru  = ('stk, ('a * 'b * 'b)) stk constraint 'freevar = 'a * 'b
  type ('stk, 'freevar) fals = ('stk, ('a * 'b * 'b)) stk constraint 'freevar = 'a * 'b

  (* type-level conditional *)
  type 'stk ifelse = ('tail, 'tresult) stk
  constraint 'tcondition = 'tthen * 'telse * 'tresult
  constraint 'stk = ((('tail, 'tcondition) stk, 'tthen) stk, 'telse) stk

  (* type-level duplication of a boolean

     We prefer not to allow duplication of a quoted type, as there would be no
     way to avoid using the same polymorphic variables in both occurrences. *)
  (* TODO: use if to duplicate! *)
  type 'stk dup = ('stk, 'head) stk constraint 'stk = ('tail, 'head) stk

  (* type 'x push = 'a * 'b constraint 'x = 'a * 'b *)
  (* type ('tcondition, 'tthen, 'telse) ifelse = 'tresult constraint 'tcondition = 'tthen * 'telse * 'tresult *)
  
  type s = ((((start, 't) tru, string) typ), int) typ ifelse return
end








(* Unification in the HM type system, along with OCaml's constraint keyword,
   allows us to describe relations between types. The type-level functions
   below are using this in a way that is reminiscent of Prolog's
   predicates. We try to put the results on the left-hand side of the equals
   sign, even though a use of the constraint keyword establishes an unordered
   relation between two types. This explains why it is possible to write
   constraint 'x = ('hd, 'tl) push to indicate that 'x is the result, and the
   equivalent constraint ('hd, 'tl) push = 'x to indicate that 'hd and 'tl are
   expected to be extracted from a known 'x. *)
module T = struct
  (* TODO: bundle together the stack and an on-demand infinite stack of free variables *)
  (* This should not be exported in the sig. *)
  module Private = struct
    (* stack of type-level operands *)
    type ('tl, 'hd, 'fv) _stack = Stack of 'tl * 'hd * 'fv
    type ('stack, 'freevars) _state = State of 'stack * 'freevars (* constraint 'tail = ('a,'b) state *)

    type __start = ()
    type 'freevars _start = (__start, 'freevars) _state
    (* internal: quote a type and place it on the stack *)
    type 't     _typ = Typ of 't
    type 't _polytyp = PolyTyp of 't

    (* Push a new type-level value onto the stack *)
    (* 'fv denotes the free variables in 'v, which will be bound to a dummy
       type if the value 'v is dropped from the stack. *)
    type ('state, 'v, 'fv) _push = (('stack, 'v, 'fv) _stack, 'freevars) _state
    constraint ('stack, 'freevars) _state = 'state

    (* fetch the value on top of the the stack and its free variables *)
    type 'state _peekv = 'v
    constraint (('stack, 'v, 'fv) _stack, 'freevars) _state = 'state
    type 'state _peekfv = 'fv
    constraint (('stack, 'v, 'fv) _stack, 'freevars) _state = 'state

    (* drop the value on top of the the stack *)
    (* type closed_freevar = [`FreeVar of (closed_freevar * closed_freevar)] *)
    type closed_freevar1 = [`FreeVar of unit]
    type closed_freevar2 = [`FreeVar of (closed_freevar1 * closed_freevar1)]
    type closed_freevar3 = [`FreeVar of (closed_freevar2 * closed_freevar2)]
    type closed_freevar4 = [`FreeVar of (closed_freevar3 * closed_freevar3)]
    type closed_freevar5 = [`FreeVar of (closed_freevar4 * closed_freevar4)]
    type closed_freevar6 = [`FreeVar of (closed_freevar5 * closed_freevar5)]
    type closed_freevar7 = [`FreeVar of (closed_freevar6 * closed_freevar6)]
    type closed_freevar  = [`FreeVar of (closed_freevar7 * closed_freevar7)]
    type 'a _fv = [`FreeVar of 'a]
    type ('a,'b) _fvs = [`FreeVar of 'a * 'b]
    type no_freevar = closed_freevar
    type 'state _pop = ('tail, 'freevars) _state
    constraint (('tail, 'v, 'fv) _stack, 'freevars) _state = 'state
    type 'state _discard = ('tail, 'freevars) _state
    constraint (('tail, 'v, 'fv) _stack, 'freevars) _state = 'state
    constraint 'fv = closed_freevar
  end
  open Private

  (* Instantiate a new free variable. *)
  type ('state, 'freevar) _freevar = ('stack, 'new_freevars) _state
  constraint ('stack, 'freevars) _state = 'state
  constraint 'new_freevars * 'freevar = 'freevars

  (* Unwrap the single element on the stack. *)
  type 'state return = 'returned
  constraint 'state = ((__start, 'returned _typ, closed_freevar) _stack, closed_freevar) _state

  (* Quote a type and place it on the stack. *)
  type ('state, 't) typ  = (('stack, 't _typ, no_freevar) _stack, 'freevars) _state
  constraint ('stack, 'freevars) _state = 'state

  (* Quote a polymorphic type and place it on the stack. *)
  (* 'x should be a free variable on the caller's site,
     'xt should be 'x t where t is the polymorphic type to quote. *)
  type ('state, 'x, 'xt) polytyp = (('stack, ('xt * 'x) _polytyp, 'x _fv) _stack, 'freevars) _state
  constraint ('stack, 'freevars) _state = ('state, 'x) _freevar

  (* type-level booleans *)
  type 'state tru  = ('state2, ('a * 'b * 'a * 'b), ('a _fv, 'b _fv) _fvs) _push
  constraint 'state2 = ('state, 'a * 'b * [`tru]) _freevar
  type 'state fals = ('state2, ('a * 'b * 'b * 'a), ('a _fv, 'b _fv) _fvs) _push
  constraint 'state2 = ('state, 'a * 'b * [`fals]) _freevar

  type ('x, 'y) _cons = Cons of 'x * 'y
  type 'state cons = ('state _pop _pop,
                      ('state _pop _peekv,  'state _peekv)  _cons,
                      ('state _pop _peekfv, 'state _peekfv) _fvs) _push
  type 'state uncons = (('state _pop, 'x, 'fvx) _push, 'y, 'fvy) _push
  constraint ('x,     'y) _cons = 'state _peekv
  constraint ('fvx, 'fvy) _fvs  = 'state _peekfv

  (* type-level conditional *)
  type 'state ifelse = (('tail, 'tresult, 'fvr) _stack, 'freevars) _state
  constraint 'state = (((('tail, 'tcondition, 'fvc) _stack, 'tthen, 'fvt) _stack, 'telse, 'fve) _stack, 'freevars) _state
  constraint 'tcondition = ([`whut of 'tthen * 'fvt]) * ([`whut of 'telse * 'fve]) * ([`whut of 'tresult * 'fvr]) * ([`whut of 'tdiscard * 'fvd])
  (* (\* DEBUG *\)
   * constraint 'fvr = 'fvt
   * constraint 'fvd = 'fve *)
  (* Since the discarded value may contain some free variables, we need to
     bind them to some dummy value. *)
  constraint 'fvd = closed_freevar

  (* type-level duplication of a boolean

     We prefer not to allow duplication of a quoted type, as there would be no
     way to avoid using the same polymorphic variables in both occurrences. *)
  type 'state dup = 'state tru tru cons fals fals cons ifelse uncons
  type 'state exch = (('state _pop _pop, 'state _peekv, 'state _peekfv) _push, 'state _pop _peekv, 'state _pop _peekfv) _push
  type 'state pop = 'state _discard

  (* apply a single-argument polymorphic type to an argument *)
  type 'state polyapp = 'result
  (* left-hand-side of product type is output, right-hand-side is input *)
  constraint ('result * ('state _pop)) _polytyp = 'state _peekv

  (* type 'x push = 'a * 'b constraint 'x = 'a * 'b *)
  (* type ('tcondition, 'tthen, 'telse) ifelse = 'tresult constraint 'tcondition = 'tthen * 'telse * 'tresult *)

  type 'state typ_string = ('state, string) typ
  type 'state typ_int    = ('state,    int) typ
  type 'state typ_unit   = ('state, unit)   typ
end

module TypeLevelFunctionsExamples = struct
  open T
  
  type 'state s = 'state typ_string typ_int ifelse return
  type s1 = 't tru s            (* push true  on the stack, call s => string *)
  type s2 = 't fals s           (* push false on the stack, call s => int *)
  
  type 'state f = 'state tru typ_string
  type 'state t = 'state polyapp typ_int ifelse return
  type 'state push_f = ('state, 'x, 'x f) polytyp
  type t1 = 't push_f t         (* => string *)

  type 'state u = 'state dup typ_int typ_string ifelse typ_unit exch ifelse return
  (* type 'state u = 'state tru tru tru cons fals fals cons ifelse uncons pop pop typ_int return *)
  (* type 'state u = 'state tru w typ_int typ_string ifelse return *)

     
  (* type 'state w = 'state tru fals cons (\* the problem occurs when the cons is present. *\)
   *                   fals fals cons ifelse (\* and the second cons and ifelse are present too *\)
   * type 'state u = 'state tru w pop typ_int return *)
  
  type u1 = 't tru u
  type u2 = 't fals u
end










(* The code below gives the error:

   In this definition, a type variable cannot be deduced from the type
   parameters. *)
(* module TypeLevelFunctions = struct
 *   (\* This should not be exported in the sig. *\)
 *   module Private = struct
 *     (\* stack of type-level operands *\)
 *     type ('head, 'tail)     __stk = 'head * 'tail
 *     (\* TODO: use a low-level boolean to make an "or" constraint on the tail: "State" OR "start" and fuse with line above *\)
 *     type ('head, 'tail)      _stk = ('head, 'tail) __stk      (\* constraint 'tail = ('a,'b) __stk *\)
 *     type ('stk, 'freevars) _state = [`State of 'stk * 'freevars] (\* constraint 'stk = ('head, 'tail) _stk *\)
 * 
 *     type 'freevars start = (unit * (unit * unit), 'freevars) _state
 * 
 *     (\* type ('state, 'elt)      push = (('elt, 'stk) _stk, 'freevars) _state
 *      * constraint ('stk, 'freevars) _state = 'state *\)
 *     type ('state, 'elt)      push = [`State of ('elt * 'stk) * 'freevars]
 *     constraint [`State of 'stk * 'freevars] = 'state
 * 
 *     type ('state, 'fresh_freevar) freevar = ('stk, 'next_freevars) _state
 *     constraint ('stk, 'freevars) _state = 'state
 *     (\* The following line connects two polymorphic type variables (as the
 *        element types of a 2-tuple) to what was previously a single spot in the
 *        type. This has the effect of conceptually splitting that chunk of
 *        unknown into a fresh free variable (which may get unified to anything),
 *        and a new chunk of unknown. *\)
 *     constraint ('fresh_freevar * 'next_freevars) = 'freevars
 * 
 *     (\* internal: quote a type and place it on the stack *\)
 *     type 't _typ = Typ of 't
 *   end
 *   open Private
 * 
 *   (\* unwrap the single element on the stack *\)
 *   type 'state return = 'returned constraint 'state = ('freevars start, 'returned _typ) push
 * 
 *   (\* quote a type and place it on the stack *\)
 *   type ('state, 't) typ  = ('state, 't _typ) push
 * 
 *   (\* type-level booleans *\)
 *   type ('state, 'freevar) tru  = ('state, ('a * 'b * 'b)) push constraint 'freevar = 'a * 'b
 *   type ('state, 'freevar) fals = ('state, ('a * 'b * 'b)) push constraint 'freevar = 'a * 'b
 * 
 *   (\* type-level conditional *\)
 *   type 'arg psh = [`State of ('tcondition * 'stk) * 'freevars]
 *   constraint [`State of 'stk * 'freevars] = 'tail
 *   constraint 'tail * 'tcondition * 'stk * 'freevars = 'arg
 *   
 *   type 'state ifelse = ('tail * 'tresult * int * string) psh
 *   (\* constraint ('tail, 'tcondition) push = 'state *\)
 *     (\* type ('state, 'elt)      push = [`State of ('elt * 'stk) * 'freevars] *\)
 *   constraint 'state = ('tail * 'tcondition * int * string) psh
 *   constraint 'tcondition = 'tresult * int
 *   (\* constraint 'tthen * 'telse * 'tresult = 'tcondition
 *    * constraint ((('tail, 'tcondition) push, 'tthen) push, 'telse) push = 'state *\)
 * 
 *   (\* type-level duplication of a boolean
 * 
 *      We prefer not to allow duplication of a quoted type, as there would be no
 *      way to avoid using the same polymorphic variables in both occurrences. *\)
 *   (\* TODO: use if to duplicate! *\)
 *   (\* TODO: use if to duplicate! *\)
 *   (\* TODO: use if to duplicate! *\)
 *   (\* TODO: use if to duplicate! *\)
 *   type 'state dup = ('state, 'head) push constraint 'state = ('tail, 'head) push
 * 
 *   (\* type 'x push = 'a * 'b constraint 'x = 'a * 'b *\)
 *   (\* type ('tcondition, 'tthen, 'telse) ifelse = 'tresult constraint 'tcondition = 'tthen * 'telse * 'tresult *\)
 *   
 *   (\* type s = ((((start, 't) tru, string) typ), int) typ ifelse return *\)
 * end *)







   
(** jacm-final.pdf p.8 (584) §4.1 media 60 480 368 36 *)
(* let rec child i d =
     if i = 0 then d
     else child (i - 1) d.child *)

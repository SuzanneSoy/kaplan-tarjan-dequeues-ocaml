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
    type closed_freevar1 = [`FreeVar of unit * unit]
    (* 'x serves as a handled. If is is not unified with anything, it does not
       cause any harm. If it is unified with ('u * 'u * 'v * 'v * 'w * 'w), it
       causes three pairs of types to unify, thereby closing a local type
       variable and propagating that close order down to the two children. *)
    type 'dummy close_freevars = ('u * 'u * 'v * 'v * 'w * 'w) constraint ('u * 'v * 'w) = 'dummy
    type ('a, 'x) _fv = [`FreeVar of 'a * 'x]
    constraint (  'a   * closed_freevar1
                * unit * unit (* ignored *)
                * unit * unit (* ignored *)) = 'x
    type ('a, 'b, 'x) _fvs = [`FreeVar of ('a * 'b) * 'x]
    constraint (  unit * unit (* ignored *)
                * 'a   * [`FreeVar of 'one * 'dummy1 close_freevars]
                * 'b   * [`FreeVar of 'two * 'dummy2 close_freevars]) = 'x
    (*TODO: possibly need to bind: 'one 'two 'dummy1 'dummy2 *)
    type no_freevar = closed_freevar1
    type 'state _pop = ('tail, 'freevars) _state
    constraint (('tail, 'v, 'fv) _stack, 'freevars) _state = 'state
    type 'state _discard = ('tail, 'freevars) _state
    constraint (('tail, 'v, 'fv) _stack, 'freevars) _state = 'state
    constraint 'fv = [`FreeVar of 'fv_ * 'dummy close_freevars]
  end
  open Private

  (* Instantiate a new free variable. *)
  type ('state, 'freevar) _freevar = ('stack, 'new_freevars) _state
  constraint ('stack, 'freevars) _state = 'state
  constraint 'new_freevars * 'freevar = 'freevars

  (* Unwrap the single element on the stack. *)
  type 'state return = 'returned
  constraint 'state = ((__start, 'returned _typ, 'fvs) _stack, closed_freevar1) _state
  constraint 'fvs = [`FreeVar of 'fvs_ * 'dummy close_freevars]

  (* Quote a type and place it on the stack. *)
  type ('state, 't) typ  = (('stack, 't _typ, no_freevar) _stack, 'freevars) _state
  constraint ('stack, 'freevars) _state = 'state

  (* Quote a polymorphic type and place it on the stack. *)
  (* 'x should be a free variable on the caller's site,
     'xt should be 'x t where t is the polymorphic type to quote. *)
  type ('state, 'x, 'xt) polytyp = (('stack, ('xt * 'x) _polytyp, ('x, 'fv) _fv) _stack, 'freevars) _state
  constraint ('stack, 'freevars) _state = ('state, 'x * 'fv) _freevar

  (* type-level booleans *)
  type 'state tru  = ('state2, ('a * 'b * 'a * 'b), (('a, 'fva) _fv, ('b, 'fvb) _fv, 'fvab) _fvs) _push
  constraint 'state2 = ('state, 'a * 'b * 'fva * 'fvb * 'fvab) _freevar
  type 'state fals = ('state2, ('a * 'b * 'b * 'a), (('a, 'fva) _fv, ('b, 'fvb) _fv, 'fvab) _fvs) _push
  constraint 'state2 = ('state, 'a * 'b * 'fva * 'fvb * 'fvab) _freevar

  type ('x, 'y) _cons = Cons of 'x * 'y
  type 'state cons = ('state2 _pop _pop,
                      ('state2 _pop _peekv,  'state2 _peekv)  _cons,
                      ('state2 _pop _peekfv, 'state2 _peekfv, 'fv) _fvs) _push
  constraint 'state2 = ('state, 'fv) _freevar
  type 'state uncons = (('state _pop, 'x, 'fvx) _push, 'y, 'fvy) _push
  constraint ('x,            'y) _cons = 'state _peekv
  constraint ('fvx, 'fvy, 'fvxy) _fvs  = 'state _peekfv
  constraint 'fvxy = (  unit * unit (* ignored *)
                        * 'a   * [`FreeVar of 'one * 'dummy1 close_freevars]
                        * 'b   * [`FreeVar of 'two * 'dummy2 close_freevars])

  (* type-level conditional *)
  type 'state ifelse = (('tail, 'tresult, 'fvr) _stack, 'freevars) _state
  constraint 'state = (((('tail, 'tcondition, 'fvc) _stack, 'tthen, 'fvt) _stack, 'telse, 'fve) _stack, 'freevars) _state
  constraint 'tcondition =   ([`v_fv of 'tthen    * 'fvt])
                           * ([`v_fv of 'telse    * 'fve])
                           * ([`v_fv of 'tresult  * 'fvr])
                           * ([`v_fv of 'tdiscard * 'fvd])
  (* Since the discarded value may contain some free variables, we need to
     bind them to some dummy value. *)
  constraint 'fvd = [`FreeVar of 'fvd_ * 'dummy close_freevars]
  constraint 'fvc = [`FreeVar of 'fvab * 'x]
  constraint 'x   = unit (* release the 'x of 'fvc, since the condition was used *)

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
  type u1 = 't tru u
  type u2 = 't fals u
end

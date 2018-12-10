(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)

exception InvalidFormat of string * string

(** Type of source code locations *)
type location = {
  loc_file : string;
  loc_pos : ((int * int) * (int * int)) option;
}

(** Tez constants are stored with strings *)
type tez = { tezzies : string; mutez : string option }

(** Unbounded integer constants *)
type integer = { integer : Z.t }


(** Liquidity constants *)
type const =
  | CUnit
  | CBool of bool
  | CInt of integer
  | CNat of integer
  | CTez of tez
  | CTimestamp of string
  | CString of string
  | CBytes of string
  | CKey of string
  | CSignature of string
  | CTuple of const list
  | CNone
  | CSome of const

  | CMap of (const * const) list
  | CBigMap of (const * const) list
  | CList of const list
  | CSet of const list

  | CLeft of const
  | CRight of const

  | CKey_hash of string
  | CContract of string
  | CAddress of string

  | CRecord of (string * const) list
  | CConstr of string * const


(** Liquidity types *)
and datatype =
  (* michelson *)
  | Tunit
  | Tbool
  | Tint
  | Tnat
  | Ttez
  | Tstring
  | Tbytes
  | Ttimestamp
  | Tkey
  | Tkey_hash
  | Tsignature
  | Toperation
  | Taddress

  | Ttuple of datatype list

  | Toption of datatype
  | Tlist of datatype
  | Tset of datatype

  | Tmap of datatype * datatype
  | Tbigmap of datatype * datatype
  | Tcontract of contract_sig
  | Tor of datatype * datatype
  | Tlambda of datatype * datatype

  (* liquidity extensions *)
  | Trecord of string * (string * datatype) list
  | Tsum of string * (string * datatype) list
  | Tclosure of (datatype * datatype) * datatype
  | Tfail

  | Tvar of string

(** A signature for an entry point *)
and entry_sig = {
  entry_name : string;     (** name of the entry point *)
  parameter : datatype;    (** type of the parameter *)
  parameter_name : string; (** name of the parameter argument *)
  storage_name : string;   (** name of the storage argument (type is
                               common to all entry points in a
                               contract) *)
}

(** An entry point *)
and 'exp entry = {
  entry_sig : entry_sig; (** Signature of the entry point *)
  code : 'exp;           (** Liquidity code for the entry point *)
}

(** A Liquidity contract *)
and 'exp contract = {
  contract_name : string;    (** Name of the contract (Capitalized) *)
  storage : datatype;        (** Type of storage *)
  values : (string * bool (* inline *) * 'exp) list;
  (** Global constants or functions *)
  entries : 'exp entry list; (** Entry points of the contract *)
  ty_env : env;
  c_init : 'exp init option;
}

and entries_sig = entry_sig list

(** Signature (or interface) of a contract *)
and contract_sig = {
  sig_name : string option;
  entries_sig : entries_sig;
}

and full_contract_sig = {
  f_sig_name : string option;
  f_storage : datatype;
  f_entries_sig : entries_sig;
}

and extprim = {
  tvs : string list;
  atys : datatype list;
  rty : datatype;
  effect : bool;
  nb_arg : int;
  nb_ret : int;
  minst : string;
}

(** Environment for parsing *)
and env = {
  (* name of file being compiled *)
  filename : string;

  (* name of contract being compiled *)
  contractname : string;
  (* fields modified in LiquidFromOCaml *)
  (* type definitions *)
  mutable types : datatype StringMap.t;
  (* contract type definitions *)
  mutable contract_types : contract_sig StringMap.t;
  (* labels of records in type definitions *)
  mutable labels : (string * int * datatype) StringMap.t;
  (* constructors of sum-types in type definitions *)
  mutable constrs : (string * datatype) StringMap.t;
  (* extended primitives definitions *)
  mutable ext_prims : extprim StringMap.t;
  (* englobing env *)
  top_env : env option;
}


(** Representation of Liquidity contract initializers *)
and 'exp init = {
  init_name : string;
  init_args : (string * location * datatype) list; (* arguments *)
  init_body : 'exp; (* init code *)
}

(** Type of located Liquidity errors *)
type error = { err_loc: location; err_msg: string }

(** Liquidity errors *)
exception LiquidError of error

(** Type of Michelson contracts *)
type 'a mic_contract = {
  mic_parameter : datatype;
  mic_storage : datatype;
  mic_code : 'a;
}

(** Allowed built-in primities in Liquidity *)
type primitive =
  (* resolved in LiquidCheck *)
  | Prim_unknown
  | Prim_coll_find
  | Prim_coll_update
  | Prim_coll_mem
  | Prim_coll_size

  (* extended primitives *)
  | Prim_extension of string * bool * (datatype list) * int * int * string

  (* generated in LiquidCheck *)
  | Prim_unused of string option

  (* primitives *)
  | Prim_tuple_get
  | Prim_tuple_set
  | Prim_tuple

  | Prim_self
  | Prim_balance
  | Prim_now
  | Prim_amount
  | Prim_gas
  | Prim_Left
  | Prim_Right
  | Prim_source
  | Prim_sender
  | Prim_eq
  | Prim_neq
  | Prim_lt
  | Prim_le
  | Prim_gt
  | Prim_ge
  | Prim_compare
  | Prim_add
  | Prim_sub
  | Prim_mul
  | Prim_ediv

  | Prim_map_find
  | Prim_map_update
  | Prim_map_add
  | Prim_map_remove
  | Prim_map_mem
  | Prim_map_size

  | Prim_set_update
  | Prim_set_add
  | Prim_set_remove
  | Prim_set_mem
  | Prim_set_size

  | Prim_Some

  | Prim_list_size
  | Prim_list_rev

  | Prim_create_account
  | Prim_blake2b
  | Prim_sha256
  | Prim_sha512
  | Prim_hash_key
  | Prim_check
  | Prim_default_account
  | Prim_set_delegate
  | Prim_address
  | Prim_pack

  | Prim_Cons
  | Prim_or
  | Prim_and
  | Prim_xor
  | Prim_not
  | Prim_abs
  | Prim_is_nat
  | Prim_int
  | Prim_neg
  | Prim_lsr
  | Prim_lsl

  | Prim_exec

  | Prim_bytes_size
  | Prim_string_size

  | Prim_slice
  | Prim_bytes_sub
  | Prim_string_sub

  | Prim_concat
  | Prim_concat_two
  | Prim_string_concat
  | Prim_bytes_concat

(** Allowed built-in primities for fold applications *)
type prim_fold =
  | Prim_map_iter
  | Prim_set_iter
  | Prim_list_iter
  | Prim_map_fold
  | Prim_set_fold
  | Prim_list_fold

  | Prim_coll_iter
  | Prim_coll_fold

(** Allowed built-in primities for map applications *)
type prim_map =
  | Prim_map_map
  | Prim_set_map
  | Prim_list_map
  | Prim_coll_map

(** Allowed built-in primities for map-fold applications *)
type prim_map_fold =
  | Prim_map_map_fold
  | Prim_set_map_fold
  | Prim_list_map_fold
  | Prim_coll_map_fold


(** Type of constructors, of both sum types and variants.
    [variant] is the only parameterized type authorized in Liquidity.
    Its constructors, [Left] and [Right] must be constrained with type
    annotations, for the correct types to be propagated in the sources.
*)
type constructor =
    Constr of string
  | Left of datatype
  | Right of datatype

(** A pattern in a pattern-matching construct is either a constructor
    [C (a, b, c)] or a wildcard [_] *)
type pattern =
  | CConstr of string * string list
  | CAny

(** Name with source location information *)
type loc_name = { nname : string; nloc: location }

(** The parameterized types of Liquidity expression. The parameter
    ['ty] is either [unit] or [datatype] for resp. typed and untyped
    expression. The parameter ['a] is a ghost type {!typed} or
    {!encoded} to enforce some properties about expression through
    typing.  *)
type ('ty, 'a) exp = {
  desc : ('ty, 'a) exp_desc; (** Actual expression *)
  name : string option;      (** Potential name of expression *)
  loc : location;            (** Source location *)
  ty : 'ty;                  (** Type of expression *)
  bv : StringSet.t;          (** Set of bound variable in the expression *)
  effect : bool;             (** Effect flag, [true] if evaluation of
                                 the expression has side effects *)
  transfer : bool;           (** Transfer flag, [true] if the
                                 expression can contain an operation
                                 (these should not be duplicated) *)
}

(** Type of raw Liquidity expression descriptions *)
and ('ty, 'a) exp_desc =
  | Let of { bnd_var: loc_name;
             inline: bool;
             bnd_val: ('ty, 'a) exp;
             body: ('ty, 'a) exp }
  (** Let binding: {[let@@inline bnd_var = bnd_val in body ]} *)

  | Var of string (** Simple variable : [x] *)

  | SetField of { record : ('ty, 'a) exp;
                  field: string;
                  set_val: ('ty, 'a) exp }
  (** Functional record field update: {[ record.field <- set_val ]} *)

  | Project of { field: string;
                 record: ('ty, 'a) exp }
  (** Record projection: {[ record.field ]} *)

  | Const of { ty: datatype;
               const: const }
  (** Constant with its type *)

  | Apply of { prim: primitive;
               args: ('ty, 'a) exp list }
  (** Built-in function application. Functions that are not built-in
      use the special primitive {!Prim_unknown}. *)

  | If of { cond: ('ty, 'a) exp;
            ifthen: ('ty, 'a) exp;
            ifelse: ('ty, 'a) exp }
  (** If-then-else constructs: {[ if cond then ifthen else ifelse ]} *)

  | Seq of ('ty, 'a) exp * ('ty, 'a) exp
  (** Sequences: {[ e1; e2 ]} *)

  | Transfer of { dest: ('ty, 'a) exp;
                  amount: ('ty, 'a) exp }
  (** Transfers:
      - {[ Account.transfer ~dest ~amount ]} *)

  | Call of { contract: ('ty, 'a) exp;
              amount: ('ty, 'a) exp;
              entry: string option;
              arg: ('ty, 'a) exp }
  (** Contract calls:
      - {[ contract.entry arg ~amount ]} *)

  | MatchOption of { arg : ('ty, 'a) exp;
                     ifnone: ('ty, 'a) exp;
                     some_name: loc_name;
                     ifsome: ('ty, 'a) exp }
  (** Pattern matching over optional values:
      {[ match arg with
        | None -> ifnone
        | Some some_name -> ifsome ]} *)

  | MatchList of { arg: ('ty, 'a) exp;
                   head_name: loc_name;
                   tail_name: loc_name;
                   ifcons: ('ty, 'a) exp;
                   ifnil: ('ty, 'a) exp }
  (** Pattern matching over lists:
      {[ match arg with
        | head_name :: tail_name -> ifcons
        | [] -> ifnil ]} *)

  | Loop of { arg_name: loc_name;
              body: ('ty, 'a) exp;
              arg: ('ty, 'a) exp }
  (** Functional loops:
      {[ Loop.loop (fun arg_name -> body) arg ]} *)

  | LoopLeft of { arg_name: loc_name;
                  body: ('ty, 'a) exp;
                  arg: ('ty, 'a) exp;
                  acc: ('ty, 'a) exp option }
  (** Functional loops with accumulator:
      {[ Loop.left (fun arg_name -> body) arg acc ]} *)

  | Fold of { prim: prim_fold;
              arg_name: loc_name;
              body: ('ty, 'a) exp;
              arg: ('ty, 'a) exp;
              acc: ('ty, 'a) exp }
  (** Fold over collections with accumulator:
      {[ List.fold (fun arg_name -> body) arg acc ]} *)

  | Map of { prim: prim_map;
             arg_name: loc_name;
             body: ('ty, 'a) exp;
             arg: ('ty, 'a) exp }
  (** Map over collections:
      {[ List.map (fun arg_name -> body) arg ]} *)

  | MapFold of { prim: prim_map_fold;
                 arg_name: loc_name;
                 body: ('ty, 'a) exp;
                 arg: ('ty, 'a) exp;
                 acc: ('ty, 'a) exp }
  (** Map-Fold (like map-reduce) over collections with accumulator:
      {[ List.map_fold (fun arg_name -> body) arg acc ]} *)

  | Lambda of { arg_name: loc_name;
                arg_ty: datatype;
                body: ('ty, 'a) exp;
                ret_ty: datatype; (* inferred during typechecking *)
                recursive: string option;
              }
  (** Pure lambda abstractions:
      {[ fun (arg_name : arg_ty) -> (body : ret_ty) ]} *)

  | Closure of { arg_name: loc_name;
                 arg_ty: datatype;
                 call_env: (string * ('ty, 'a) exp) list;
                 body: ('ty, 'a) exp;
                 ret_ty: datatype; (* inferred during typechecking *)
               }
  (** Closures: same as lambda-abstractions but with a call
      environment. Closures cannot be written explicitely in Liquidity
      syntax, rather lambda's that are not pure are transformed into
      closures during typechecking. *)

  | Record of (string * ('ty, 'a) exp) list
  (** Record constructs:
      {[ { a = exp1; b = exp2; c = ... }]} *)

  | Constructor of { constr: constructor;
                     arg: ('ty, 'a) exp }
  (** Constructor application: {[ C arg ]} *)

  | MatchVariant of { arg: ('ty, 'a) exp;
                      cases: (pattern * ('ty, 'a) exp) list }
  (** Pattern matching over sum or variant types:
      {[ match arg with
        | C1 (a1, a2) -> exp1
        | C2 (x1, x2, x3) -> exp2
        | _ -> exp3 *)

  | MatchNat of { arg: ('ty, 'a) exp;
                  plus_name: loc_name;
                  ifplus: ('ty, 'a) exp;
                  minus_name: loc_name;
                  ifminus: ('ty, 'a) exp }
  (** Special constructs for pattern matching over the sign of an integer:
      {[ match%nat arg with
        | Plus plus_name -> ifplus
        | Minus minus_name -> ifminus ]}*)

  | Failwith of ('ty, 'a) exp
  (** Failwith: {[ failwith arg ]} *)

  | CreateContract of { args: ('ty, 'a) exp list;
                        contract: ('ty, 'a) exp contract }
  (** Oringinating contracts:
      {[ Contract.create ~manager ~delegate ~spendable ~delegatable ~amount
          (contract C) ]} *)

  | ContractAt of { arg: ('ty, 'a) exp;
                    c_sig: contract_sig }
  (** Contract from address: {[ (Contract.at arg : (contract C_sig) option ]} *)

  | Unpack of { arg: ('ty, 'a) exp;
                ty: datatype }
  (** Unpacking bytes with type annotation:
      {[ (Bytes.unpack arg : ty option) ]} *)

  | TypeAnnot of { e: ('ty, 'a) exp;
                   ty: datatype }
  (** Type annotation: {[ (e : ty) ]} *)

  | Type of datatype
  (** Type, for use with extended primitives : [ty] *)


(** Ghost type for typed expressions *)
type typed
(** Ghost type for encoded expressions *)
type encoded
(** Untyped expressions *)
type syntax_exp = (unit, unit) exp
(** Typed expressions *)
type typed_exp = (datatype, typed) exp
(** Endoced expressions *)
type encoded_exp = (datatype, encoded) exp
(** Simplified expressions *)
type live_exp = (datatype * datatype StringMap.t, encoded) exp


(** Type of Michelson expression *)
type michelson_exp =
  | M_INS of string * string list
  | M_INS_CST of string * datatype * const * string list
  | M_INS_EXP of string * datatype list * michelson_exp list * string list

(** Intermediate representation for Michelson expressions, the first
    parameter to allow annotated (or not) expressions *)
type 'a pre_michelson =
  | RENAME of string option
  | SEQ of 'a list
  | DIP of int * 'a
  | IF of 'a * 'a
  | IF_NONE of 'a * 'a
  | IF_CONS of 'a * 'a
  | IF_LEFT of 'a * 'a
  | LOOP of 'a
  | ITER of 'a
  | MAP of 'a
  | LOOP_LEFT of 'a

  | LAMBDA of datatype * datatype * 'a
  | EXEC

  | DUP of int
  | DIP_DROP of int * int
  | DROP
  | CAR of string option
  | CDR of string option
  | CDAR of int * string option
  | CDDR of int * string option
  | PUSH of datatype * const
  | PAIR
  | RECORD of string * string option
  | COMPARE
  | LE | LT | GE | GT | NEQ | EQ
  | FAILWITH
  | NOW
  | TRANSFER_TOKENS
  | ADD
  | SUB
  | BALANCE
  | SWAP
  | GET
  | UPDATE
  | SOME
  | CONCAT
  | MEM
  | SLICE

  | SELF
  | AMOUNT
  | STEPS_TO_QUOTA
  | CREATE_ACCOUNT
  | BLAKE2B
  | SHA256
  | SHA512
  | HASH_KEY
  | CHECK_SIGNATURE
  | ADDRESS

  | CONS
  | OR
  | XOR
  | AND
  | NOT

  | INT
  | ABS
  | ISNAT
  | NEG
  | MUL

  | LEFT of datatype * string option
  | RIGHT of datatype * string option
  | CONTRACT of datatype

  | EDIV
  | LSL
  | LSR

  | SOURCE
  | SENDER

  | SIZE
  | IMPLICIT_ACCOUNT
  | SET_DELEGATE

  | CREATE_CONTRACT of 'a mic_contract

  | PACK
  | UNPACK of datatype

  | EXTENSION of string * datatype list

  (* obsolete *)
  | MOD
  | DIV

(** Intermediate Michelson expressions with location information and
    names *)
type loc_michelson = {
  loc : location;
  ins : loc_michelson pre_michelson;
  mutable loc_name : string option;
}

(* let mic ins = ins *)
(* let mic_loc loc ins = { loc; ins } *)

(** Type of closure environment used to typechecking *)
type closure_env = {
  env_vars :  (string (* name outside closure *)
               * datatype
               * int (* index *)
               * (int ref * (* usage counter inside closure *)
                  int ref (* usage counter outside closure *)
                 )) StringMap.t;
  env_bindings : (encoded_exp (* expression to access variable inside closure *)
                  * (int ref * (* usage counter inside closure *)
                     int ref (* usage counter outside closure *)
                    )) StringMap.t;
  call_bindings : (string * encoded_exp) list;
}

(** Environment for typechecking *)
type typecheck_env = {
  warnings : bool;
  annot : bool;
  decompiling : bool;
  counter : int ref;
  vars : (string * datatype * bool (* fails *) ) StringMap.t;
  vars_counts : int ref StringMap.t;
  env : env;
  to_inline : encoded_exp StringMap.t ref;
  force_inline : encoded_exp StringMap.t ref;
  t_contract_sig : full_contract_sig;
  clos_env : closure_env option;
}


(** {2 decompilation } *)

(** Graph-like structure used for symbolic execution during
    decompilation *)
type node = {
  num : int;
  loc : location;
  mutable node_name : string option;
  mutable kind : node_kind;
  mutable args : node list; (* dependencies *)

  mutable next : node option;
  mutable prevs : node list;
}

and node_kind =
  | N_UNKNOWN of string
  | N_VAR of string
  | N_START
  | N_IF of node * node
  | N_IF_RESULT of node * int
  | N_IF_THEN of node
  | N_IF_ELSE of node
  | N_IF_END of node * node
  | N_IF_END_RESULT of node * node option * int
  | N_IF_NONE of node
  | N_IF_SOME of node * node
  | N_IF_NIL of node
  | N_IF_CONS of node * node * node
  | N_IF_LEFT of node * node
  | N_IF_RIGHT of node * node
  | N_IF_PLUS of node * node
  | N_IF_MINUS of node * node
  | N_TRANSFER
  | N_CALL
  | N_CREATE_CONTRACT of node_exp mic_contract
  | N_CONST of datatype * const
  | N_PRIM of string
  | N_FAILWITH
  | N_ARG of node * int
  | N_LOOP of node * node
  | N_LOOP_BEGIN of node
  | N_LOOP_RESULT of (* N_LOOP *) node
                                  * (* N_LOOP_BEGIN *) node * int
  | N_LOOP_END of (* N_LOOP *) node
                               * (* N_LOOP_BEGIN *) node
                               * (* final_cond *) node
  | N_LOOP_LEFT_BEGIN of node
  | N_LOOP_LEFT_END of node * node * node
  | N_LOOP_LEFT_RESULT of node * node * int
  | N_LOOP_LEFT of node * node

  | N_FOLD of node * node
  | N_FOLD_BEGIN of node
  | N_FOLD_RESULT of node (* N_FOLD *)
                     * node * int (* N_FOLD_BEGIN *)
  | N_FOLD_END of node (* N_FOLD *)
                  * node (* N_FOLD_BEGIN *)
                  * node (* accumulator *)
  | N_MAP of node * node
  | N_MAP_BEGIN of node
  | N_MAP_RESULT of node (* N_MAP *)
                    * node * int (* N_MAP_BEGIN *)
  | N_MAP_END of node (* N_MAP *)
                 * node (* N_MAP_BEGIN *)
                 * node (* accumulator *)
  | N_LAMBDA of node * node * datatype * datatype
  | N_LAMBDA_BEGIN
  | N_LAMBDA_END of node
  | N_END
  | N_LEFT of datatype
  | N_RIGHT of datatype
  | N_CONTRACT of datatype
  | N_UNPACK of datatype
  | N_ABS
  | N_RECORD of string list
  | N_SETFIELD of string
  | N_PROJ of string
  | N_CONSTR of string
  | N_RESULT of node * int

and node_exp = node * node

type syntax_contract = syntax_exp contract
type typed_contract = typed_exp contract
type encoded_contract = encoded_exp contract
type michelson_contract = michelson_exp list
type node_contract = node_exp mic_contract
type loc_michelson_contract = loc_michelson mic_contract

(** Types of warning *)
type warning =
  | Unused of string
  | UnusedMatched of string
  | NotRecursive of string
  | AlwaysFails

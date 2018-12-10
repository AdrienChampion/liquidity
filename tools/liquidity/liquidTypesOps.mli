(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(** Size of a type.

Only successful on tuples and records. *)
val size_of_type : datatype -> int

(** Comparable types can be used as, e.g., keys in a map.

This corresponds to comparable types in Michelson *)
val comparable_type : datatype -> bool

(** Equality between types.

Contract signatures are first order values in types, and equality between those is modulo renaming
(of signatures). *)
val eq_types : datatype -> datatype -> bool

(** Equality between signatures modulo names of signatures . *)
val eq_signature : contract_sig -> contract_sig -> bool

(** Returns true if a type contains an operation.

Except in lambdas where they are allowed in Michelson. *)
val type_contains_nonlambda_operation : datatype -> bool

(** Extract the signature of a contract. *)
val sig_of_contract : 'a contract -> contract_sig

(** Extract the full signature, including storage type, of a contract. *)
val full_sig_of_contract : 'a contract -> full_contract_sig

(** Erases the storage type from a contract signature. *)
val sig_of_full_sig : full_contract_sig -> contract_sig

(** Primitive corresponding to a string. *)
val primitive_of_string : string -> primitive
(** String representation of a primitive. *)
val string_of_primitive : primitive -> string

(** Fold primitive corresponding to a string. *)
val fold_primitive_of_string : string -> prim_fold
(** String representation of a fold primitive. *)
val string_of_fold_primitive : prim_fold -> string

(** Map primitive corresponding to a string. *)
val map_primitive_of_string : string -> prim_map
(** String representation of a map primitive. *)
val string_of_map_primitive : prim_map -> string

(** Map/fold primitive corresponding to a string. *)
val map_fold_primitive_of_string : string -> prim_map_fold
(** String representation of a map/fold primitive. *)
val string_of_map_fold_primitive : prim_map_fold -> string

(** Smart constructor for Liquidity expressions. *)
val mk : ?name:string -> loc:location -> ('a, 'b) exp_desc -> 'a -> ('a, 'b) exp


(** True if the two expressions are equal, typed version. *)
val eq_typed_exp : (string -> string -> bool) -> (datatype, 'a) exp -> (datatype, 'a) exp -> bool
(** True if the two expressions are equal, syntactic version. *)
val eq_syntax_exp : ('a, 'b) exp -> ('a, 'b) exp -> bool

(** Creates an empty type-checking environment. *)
val empty_typecheck_env : warnings:bool -> full_contract_sig -> env -> typecheck_env

(** A dummy location, in an unspecified file. *)
val noloc : location

(** Builds a default contract signature for a parameter type.

The resulting signature has a single [main] entry point. *)
val contract_sig_of_param : ?sig_name:string -> datatype -> contract_sig

(** Predefined signature for contract with unit parameter. *)
val unit_contract_sig : contract_sig

(** Dummy contract signature.

Has no name, stores unit, no entry points. *)
val dummy_contract_sig : full_contract_sig

(** Checks if a string is a reserved keyword. *)
val is_reserved : string -> bool

(** True if the string has a reserved prefix. *)
val has_reserved_prefix : string -> bool

(** Prefix for constructors used to encode entry point names. *)
val entry_prefix : string
(** Prefixes the name of an entry point with the appropriate prefix. *)
val prefix_entry : string -> string
(** Retrieves the entry point name from a prefixed entry point name. *)
val entry_name_of_case : string -> string
(** True if the input corresponds to a prefixed entry point name. *)
val is_entry_case : string -> bool

(** Prefix for constructors used to encode contracts. *)
val contract_prefix : string
(** Retrieves the contract name from a prefixed contract name. *)
val contract_name_of_annot : string -> string


(** Lifts the name of a type. *)
val lift_type : env -> datatype -> datatype

(** Looks for a record label in an environment. *)
val find_label : string -> env -> string * int * datatype
(** Looks for a type in an environment. *)
val find_type : string -> env -> datatype
(** Looks for a sum-type constructor. *)
val find_constr : string -> env -> string * datatype
(** Looks for the type of a contract. *)
val find_contract_type : string -> env -> contract_sig

(** Substitution over type variables. *)
val tv_subst : (string * datatype) list -> datatype -> datatype
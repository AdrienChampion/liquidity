(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Types = LiquidTypes

open Types

(** A dummy location, in an unspecified file. *)
val noloc : location

module DTypes : sig
    (** Size of a type.

        Only successful on tuples and records. *)
    val size : datatype -> int

    (** True if the datatype is comparable.

        Comparable types can be used as, e.g., keys in a map. This corresponds to comparable types
        in Michelson *)
    val is_comparable : datatype -> bool

    (** Equality between types.

        Contract signatures are first order values in types, and equality between those is modulo
        renaming (of signatures). *)
    val eq : datatype -> datatype -> bool


    (** Returns true if a type contains an operation.

        Except in lambdas where they are allowed in Michelson. *)
    val contains_nonlambda_operation : datatype -> bool

    (** Lifts the name of a type. *)
    val lift : env -> datatype -> datatype

    (** Substitution over type variables. *)
    val tv_subst : (string * datatype) list -> datatype -> datatype
end

module Contracts : sig
    (** Equality between signatures modulo names of signatures . *)
    val eq_sig : contract_sig -> contract_sig -> bool

    (** Extract the signature of a contract. *)
    val to_sig : 'a contract -> contract_sig

    (** Extract the full signature, including storage type, of a contract. *)
    val to_full_sig : 'a contract -> full_contract_sig

    (** Erases the storage type from a contract signature. *)
    val sig_of_full_sig : full_contract_sig -> contract_sig


    (** Builds a default contract signature for a parameter type.

        The resulting signature has a single [main] entry point. *)
    val sig_of_param : ?sig_name:string -> datatype -> contract_sig
    (** Predefined signature for contract with unit parameter. *)
    val unit_sig : contract_sig
    (** Dummy contract signature.

        Has no name, stores unit, no entry points. *)
    val dummy_sig : full_contract_sig
end

(** Functions over primitives. *)
module Prims : sig
    (** Primitive corresponding to a string. *)
    val of_string : string -> primitive
    (** String representation of a primitive. *)
    val to_string : primitive -> string
    (** True if the string corresponds to a primitive. *)
    val is_primitive : string -> bool

    (** Fold primitives. *)
    module Fold : sig
        (** Fold primitive corresponding to a string. *)
        val of_string : string -> prim_fold
        (** String representation of a fold primitive. *)
        val to_string : prim_fold -> string
        (** True if the string corresponds to a fold primitive. *)
        val is_primitive : string -> bool
    end

    (** Map primitives. *)
    module Map : sig
        (** Map primitive corresponding to a string. *)
        val of_string : string -> prim_map
        (** String representation of a map primitive. *)
        val to_string : prim_map -> string
        (** True if the string corresponds to a map primitive. *)
        val is_primitive : string -> bool

        (** Map/fold primitives. *)
        module Fold : sig
            (** Map/fold primitive corresponding to a string. *)
            val of_string : string -> prim_map_fold
            (** String representation of a map/fold primitive. *)
            val to_string : prim_map_fold -> string
        (** True if the string corresponds to a map/fold primitive. *)
        val is_primitive : string -> bool
        end
    end
end

(** Functions over expressions. *)
module Expr : sig
    (** Smart constructor for Liquidity expressions. *)
    val mk :
        ?name:string ->
        loc:location ->
        ('a, 'b) exp_desc ->
        'a ->
        ('a, 'b) exp

    (** True if the two expressions are equal, typed version. *)
    val eq_typed :
        (string -> string -> bool) ->
        (datatype, 'a) exp ->
        (datatype, 'a) exp ->
        bool

    (** True if the two expressions are equal, syntactic version. *)
    val eq_syntax : ('a, 'b) exp -> ('a, 'b) exp -> bool
end

(** Function over environments. *)
module Env : sig

    module Find : sig
        (** Looks for a record label. *)
        val label : string -> env -> string * int * datatype
        (** Looks for a type. *)
        val dtype : string -> env -> datatype
        (** Looks for a sum-type constructor. *)
        val constr : string -> env -> string * datatype
        (** Looks for the type of a contract. *)
        val contract_type : string -> env -> contract_sig
    end

    module TCheck : sig
        (** Creates an empty type-checking environment. *)
        val empty : warnings:bool -> full_contract_sig -> env -> typecheck_env
    end
end

(** Functions over identifiers (strings). *)
module Idents : sig
    (** Checks if a string is a reserved keyword. *)
    val is_reserved : string -> bool
    (** True if the string has a reserved prefix. *)
    val has_reserved_prefix : string -> bool

    (** Entry-point-related identifiers. *)
    module Entry : sig
        (*  (** Prefix for constructors used to encode entry point names. *)
            val prefix : string
        *)
        (** Prefixes the name of an entry point with the appropriate prefix. *)
        val add_prefix : string -> string
        (** Retrieves the entry point name from a prefixed entry point name. *)
        val name_of_prefixed : string -> string
        (** True if the input corresponds to a prefixed entry point name. *)
        val is_prefixed : string -> bool
    end

    (** Contract-related identifiers. *)
    module Contract : sig
        (*  (** Prefix for constructors used to encode contracts. *)
            val prefix : string
        *)
        (** Prefixes the name of a contract with the appropriate prefix. *)
        val add_prefix : string -> string
        (** Retrieves the contract name from a prefixed contract name. *)
        val name_of_prefixed : string -> string
    end
end
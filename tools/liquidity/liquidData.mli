(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Liquid.Types

val default_const : datatype -> const

(** same as [default_const] but with empty values for collections *)
val default_empty_const : datatype -> const

val translate_const_exp : encoded_exp -> const

val translate : env -> full_contract_sig -> string -> datatype -> const

val string_of_const : ?ty:datatype -> const -> string

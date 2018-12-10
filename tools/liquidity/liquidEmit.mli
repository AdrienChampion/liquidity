(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Liquid.Types

(** Convert from intermediate Michelson reprsentation to Michelson
    expression *)
val emit_code : expand:bool -> loc_michelson -> michelson_exp

(** Convert from intermediate Michelson contract to Michelson contract *)
val emit_contract :
  expand:bool -> loc_michelson_contract -> michelson_contract

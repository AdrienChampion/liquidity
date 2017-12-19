(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception RequestError of string

type from =
  | From_string of string
  | From_file of string

let run _ _ _ =
  failwith "mini version cannot run"

let forge_deploy _ _ =
  failwith "mini version cannot deploy"

let deploy _ _ =
  failwith "mini version cannot deploy"

let get_storage _ _ =
  failwith "mini version cannot query node"

let forge_call _ _ _ =
  failwith "mini version cannot call"

let call _ _ _ =
  failwith "mini version cannot call"
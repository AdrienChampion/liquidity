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

let noloc = { loc_file = "<unspecified>"; loc_pos = None }



(* The following functions are not exported directly. They are defined here because they're
mutually recursive, but are exported through the modules below. *)


let rec eq_types ty1 ty2 = match ty1, ty2 with
| Tunit, Tunit
| Tbool, Tbool
| Tint, Tint
| Tnat, Tnat
| Ttez, Ttez
| Tstring, Tstring
| Tbytes, Tbytes
| Ttimestamp, Ttimestamp
| Tkey, Tkey
| Tkey_hash, Tkey_hash
| Tsignature, Tsignature
| Toperation, Toperation
| Taddress, Taddress
| Tfail, Tfail -> true

| Ttuple l1, Ttuple l2 -> begin
    try List.for_all2 eq_types l1 l2
    with Invalid_argument _ -> false
end

| Toption t1, Toption t2
| Tlist t1, Tlist t2
| Tset t1, Tset t2 ->
    eq_types t1 t2

| Tmap (a1, b1), Tmap (a2, b2)
| Tbigmap (a1, b1), Tbigmap (a2, b2)
| Tor (a1, b1), Tor (a2, b2)
| Tlambda (a1, b1), Tlambda (a2, b2) ->
    eq_types a1 a2 && eq_types b1 b2

| Tclosure ((a1, b1), c1), Tclosure ((a2, b2), c2) ->
    eq_types a1 a2 && eq_types b1 b2 && eq_types c1 c2

| Trecord (n1, l1), Trecord (n2, l2)
| Tsum (n1, l1), Tsum (n2, l2) ->
    n1 = n2 && begin
        try List.for_all2 (fun (x1, t1) (x2, t2) -> x1 = x2 && eq_types t1 t2) l1 l2
        with Invalid_argument _ -> false
    end

| Tcontract csig1, Tcontract csig2 -> eq_signature csig1 csig2

| _, _ -> false


(** Equality between signatures modulo names of signatures  *)
and eq_signature { entries_sig = s1 } { entries_sig = s2 } =
    try
        List.for_all2 (fun e1 e2 ->
            e1.entry_name = e2.entry_name &&
            (* e1.parameter_name = e2.parameter_name &&
             * e1.storage_name = e2.storage_name && *)
            eq_types e1.parameter e2.parameter
        ) s1 s2
    with Invalid_argument _ -> false

(** Returns true if a type contains an operation (excepted in lambda's
    where they are allowed in Michelson). *)
let rec type_contains_nonlambda_operation = function
| Toperation ->
    true
| Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes
| Ttimestamp | Tkey | Tkey_hash | Tsignature | Taddress | Tfail ->
    false
| Ttuple l ->
    List.exists type_contains_nonlambda_operation l
| Toption ty | Tlist ty | Tset ty ->
    type_contains_nonlambda_operation ty
| Tmap (t1, t2) | Tbigmap (t1, t2) | Tor (t1, t2) ->
    type_contains_nonlambda_operation t1 || type_contains_nonlambda_operation t2
| Trecord (_, l) | Tsum (_, l) ->
    List.exists (fun (_, t) -> type_contains_nonlambda_operation t) l
| Tcontract s ->
    List.exists (fun e -> type_contains_nonlambda_operation e.parameter) s.entries_sig
| Tlambda _ | Tclosure _ ->
    false
| Tvar _ ->
    false








let lift_name inner_name n = String.concat "." [inner_name; n]

let lift_name env = lift_name env.contractname

let rec lift_type env ty = match ty with
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp
  | Tkey | Tkey_hash | Tsignature | Toperation | Taddress | Tfail -> ty
  | Ttuple l -> Ttuple (List.map (lift_type env) l)
  | Toption t -> Toption (lift_type env t)
  | Tlist t -> Tlist (lift_type env t)
  | Tset t -> Tset (lift_type env t)
  | Tmap (t1, t2) -> Tmap (lift_type env t1, lift_type env t2)
  | Tbigmap (t1, t2) -> Tbigmap (lift_type env t1, lift_type env t2)
  | Tor (t1, t2) -> Tor (lift_type env t1, lift_type env t2)
  | Tlambda (t1, t2) -> Tlambda (lift_type env t1, lift_type env t2)
  | Tcontract c_sig -> Tcontract (lift_contract_sig env c_sig)
  | Trecord (name, fields) when StringMap.mem name env.types ->
    Trecord (lift_name env name,
             List.map (fun (f, ty) -> lift_name env f, lift_type env ty) fields)
  | Trecord (name, fields) ->
    Trecord (name, List.map (fun (f, ty) -> f, lift_type env ty) fields)
  | Tsum (name, constrs) when StringMap.mem name env.types ->
    Tsum (lift_name env name,
          List.map (fun (c, ty) -> lift_name env c, lift_type env ty) constrs)
  | Tsum (name, constrs) ->
    Tsum (name, List.map (fun (c, ty) -> c, lift_type env ty) constrs)
  | Tclosure ((t1, t2), t3) ->
    Tclosure ((lift_type env t1, lift_type env t2), lift_type env t3)
  | Tvar _ -> ty

and lift_contract_sig env c_sig =
      { sig_name = c_sig.sig_name;
        entries_sig = List.map (fun es ->
            { es with parameter = lift_type env es.parameter }
          ) c_sig.entries_sig
      }
















module DTypes = struct
    let size = function
    | Ttuple l -> List.length l
    | Trecord (_, l) -> List.length l
    | _ -> raise (Invalid_argument "size_of_type")

    let is_comparable = function
    | Tbool
    | Tint
    | Tnat
    | Ttez
    | Tstring
    | Tbytes
    | Ttimestamp
    | Tkey_hash
    | Taddress -> true
    | _ -> false

    let eq = eq_types

    let contains_nonlambda_operation = type_contains_nonlambda_operation

    let lift = lift_type

    let tv_subst s ty =
        let rec aux = function
        | Ttuple tyl -> Ttuple (List.map aux tyl)
        | Toption ty -> Toption (aux ty)
        | Tlist ty -> Tlist (aux ty)
        | Tset ty -> Tset (aux ty)
        | Tmap (ty1, ty2) -> Tmap (aux ty1, aux ty2)
        | Tbigmap (ty1, ty2) -> Tbigmap (aux ty1, aux ty2)
        | Tcontract c ->
            Tcontract {
                c with entries_sig = List.map (
                    fun es -> { es with parameter = aux es.parameter }
                ) c.entries_sig
            }
        | Tor (ty1, ty2) -> Tor (aux ty1, aux ty2)
        | Tlambda (ty1, ty2) -> Tlambda (aux ty1, aux ty2)
        | Trecord (rn, fl) ->
            Trecord (rn, List.map (fun (fn, fty) -> (fn, aux fty)) fl)
        | Tsum (sn, cl) ->
            Tsum (sn, List.map (fun (cn, cty) -> (cn, aux cty)) cl)
        | Tclosure ((ty1, ty2), ty3) ->
            Tclosure ((aux ty1, aux ty2), aux ty3)
        | Tvar tv -> List.assoc tv s
        | ty -> ty
        in
        aux ty

end

module Contracts = struct
    let eq_sig = eq_signature

    let to_sig c = {
      sig_name = None;
      entries_sig = List.map (fun e -> e.entry_sig) c.entries;
    }

    let to_full_sig c = {
      f_sig_name = None;
      f_storage = c.storage;
      f_entries_sig = List.map (fun e -> e.entry_sig) c.entries;
    }

    let sig_of_full_sig s = {
      sig_name = s.f_sig_name;
      entries_sig = s.f_entries_sig;
    }

    let sig_of_param ?sig_name parameter = {
        sig_name;
        entries_sig = [{
            entry_name = "main";
            parameter;
            parameter_name = "parameter";
            storage_name = "storage";
        }];
    }

    let unit_sig = sig_of_param ~sig_name:"UnitContract" Tunit

    let dummy_sig = {
      f_sig_name = None;
      f_storage = Tunit;
      f_entries_sig = [];
    }
end

module Prims = struct
    (*  Could not find a way to put the following elsewhere due to no-forward-ref and type
        re-exports. *)
    let primitive_of_string = Hashtbl.create 101
    let string_of_primitive = Hashtbl.create 101

    (* Some primitives should be kept internal:
     * get and set
     * get_last and set_last
     * tuple
    *)
    let () = List.iter (
        fun (n,p) ->
            Hashtbl.add primitive_of_string n p;
            Hashtbl.add string_of_primitive p n;
    ) [
        "get", Prim_tuple_get;
        "set", Prim_tuple_set;
        "tuple", Prim_tuple;

        "Array.get", Prim_tuple_get;
        "Array.set", Prim_tuple_set;

        "Current.balance", Prim_balance;
        "Current.time", Prim_now;
        "Current.amount", Prim_amount;
        "Current.gas", Prim_gas;
        "Current.source", Prim_source;
        "Current.sender", Prim_sender;

        "Left", Prim_Left;
        "Right", Prim_Right;
        "=", Prim_eq;
        "<>", Prim_neq;
        "<", Prim_lt;
        "<=", Prim_le;
        ">", Prim_gt;
        ">=", Prim_ge;
        "compare", Prim_compare;
        "+", Prim_add;
        "-", Prim_sub;
        "*", Prim_mul;
        "/", Prim_ediv;
        "~-", Prim_neg;

        "Map.find", Prim_map_find;
        "Map.update", Prim_map_update;
        "Map.add", Prim_map_add;
        "Map.remove", Prim_map_remove;
        "Map.mem", Prim_map_mem;
        "Map.cardinal", Prim_map_size;
        "Map.size", Prim_map_size;

        "Set.update", Prim_set_update;
        "Set.add", Prim_set_add;
        "Set.remove", Prim_set_remove;
        "Set.mem", Prim_set_mem;
        "Set.cardinal", Prim_set_size;
        "Set.size", Prim_set_size;

        "Some", Prim_Some;

        "List.rev", Prim_list_rev;
        "List.length", Prim_list_size;
        "List.size", Prim_list_size;

        "Contract.set_delegate", Prim_set_delegate;
        "Contract.address", Prim_address;
        "Contract.self", Prim_self;

        "Account.create", Prim_create_account;
        "Account.default", Prim_default_account;

        "Crypto.blake2b", Prim_blake2b;
        "Crypto.sha256", Prim_sha256;
        "Crypto.sha512", Prim_sha512;
        "Crypto.hash_key", Prim_hash_key;
        "Crypto.check", Prim_check;

        "Bytes.pack", Prim_pack;
        "Bytes.length", Prim_bytes_size;
        "Bytes.size", Prim_bytes_size;
        "Bytes.concat", Prim_bytes_concat;
        "Bytes.slice", Prim_bytes_sub;
        "Bytes.sub", Prim_bytes_sub;

        "String.length", Prim_string_size;
        "String.size", Prim_string_size;
        "String.concat", Prim_string_concat;
        "String.slice", Prim_string_sub;
        "String.sub", Prim_string_sub;

        "@", Prim_concat_two;

        "::", Prim_Cons;
        "lor", Prim_or;
        "or", Prim_or;
        "||", Prim_or;
        "&", Prim_and;
        "land", Prim_and;
        "&&", Prim_and;
        "lxor", Prim_xor;
        "xor", Prim_xor;
        "not", Prim_not;
        "abs", Prim_abs;
        "is_nat", Prim_is_nat;
        "int", Prim_int;
        ">>", Prim_lsr;
        "lsr", Prim_lsr;
        "<<", Prim_lsl;
        "lsl", Prim_lsl;

        "Lambda.pipe" , Prim_exec;
        "|>", Prim_exec;

        "Coll.update", Prim_coll_update;
        "Coll.mem", Prim_coll_mem;
        "Coll.find", Prim_coll_find;
        "Coll.size",Prim_coll_size;
        "Coll.concat",Prim_concat;
        "Coll.slice",Prim_slice;

        "<unknown>", Prim_unknown;
        "<unused>", Prim_unused None;
        "<extension>", Prim_extension ("", false, [], 0, 0, "");
    ]



    let of_string s = Hashtbl.find primitive_of_string s
    let to_string prim =
        try
            match prim with
            | Prim_unused (Some s) -> Printf.sprintf "<unused:%s>" s
            | Prim_extension (l, _, _, _, _, _) -> Printf.sprintf "<extension:%s>" l
            | _ ->
                Hashtbl.find string_of_primitive prim
        with Not_found ->
            Printf.eprintf "Debug: string_of_primitive(%d) raised Not_found\n%!"
                (Obj.magic prim : int);
            raise Not_found
    let is_primitive s = Hashtbl.mem primitive_of_string s

    module Fold = struct
        let fold_primitive_of_string = Hashtbl.create 8
        let string_of_fold_primitive = Hashtbl.create 8
        let () = List.iter (
            fun (n,p) ->
                Hashtbl.add fold_primitive_of_string n p;
                Hashtbl.add string_of_fold_primitive p n;
        ) [
            "Map.iter", Prim_map_iter;
            "Set.iter", Prim_set_iter;
            "List.iter", Prim_list_iter;
            "Map.fold", Prim_map_fold;
            "Set.fold", Prim_set_fold;
            "List.fold", Prim_list_fold;
            "Coll.iter", Prim_coll_iter;
            "Coll.fold", Prim_coll_fold;
        ]

        let of_string s = Hashtbl.find fold_primitive_of_string s
        let to_string prim =
            try
                Hashtbl.find string_of_fold_primitive prim
            with Not_found ->
                Printf.eprintf "Debug: string_of_fold_primitive(%d) raised Not_found\n%!"
                    (Obj.magic prim : int);
                raise Not_found
        let is_primitive s = Hashtbl.mem fold_primitive_of_string s
    end

    module Map = struct
        let map_primitive_of_string = Hashtbl.create 4
        let string_of_map_primitive = Hashtbl.create 4
        let () = List.iter (fun (n,p) ->
            Hashtbl.add map_primitive_of_string n p;
            Hashtbl.add string_of_map_primitive p n;
        ) [
            "Map.map", Prim_map_map;
            "List.map", Prim_list_map;
            "Coll.map", Prim_coll_map;
        ]

        let of_string s = Hashtbl.find map_primitive_of_string s
        let to_string prim =
            try
                Hashtbl.find string_of_map_primitive prim
            with Not_found ->
                Printf.eprintf "Debug: string_of_map_primitive(%d) raised Not_found\n%!"
                    (Obj.magic prim : int);
                raise Not_found
        let is_primitive s = Hashtbl.mem map_primitive_of_string s

        module Fold = struct
            let map_fold_primitive_of_string = Hashtbl.create 4
            let string_of_map_fold_primitive = Hashtbl.create 4
            let () = List.iter (fun (n,p) ->
                Hashtbl.add map_fold_primitive_of_string n p;
                Hashtbl.add string_of_map_fold_primitive p n;
            ) [
                "Map.map_fold", Prim_map_map_fold;
                "List.map_fold", Prim_list_map_fold;
                "Coll.map_fold", Prim_coll_map_fold;
            ]

            let of_string s = Hashtbl.find map_fold_primitive_of_string s
            let to_string prim =
                try
                    Hashtbl.find string_of_map_fold_primitive prim
                with Not_found ->
                    Printf.eprintf "Debug: string_of_map_fold_primitive(%d) raised Not_found\n%!"
                        (Obj.magic prim : int);
                    raise Not_found
            let is_primitive s = Hashtbl.mem map_fold_primitive_of_string s
        end
    end
end

module Expr = struct
    (** Smart constructor for Liquidity expressions *)
    let mk =
        let bv = StringSet.empty in
        fun ?name ~loc desc ty -> (
            let effect, transfer = match desc with
            | Const _
            | Var _ -> false, false

            | Failwith _ -> true, false

            | Project { record = e }
            | Constructor { arg = e}
            | ContractAt { arg = e}
            | Unpack { arg = e }
            | Lambda { body = e } -> e.effect, false (* e.transfer *)

            | SetField { record = e1; set_val = e2 }
            | Seq (e1, e2)
            | Let { bnd_val = e1; body = e2 }
            | Loop { body = e1; arg = e2 }
            | LoopLeft { body = e1; arg = e2 ; acc = None }
            | Map { body = e1; arg = e2 } ->
                e1.effect || e2.effect, false (* e1.transfer || e2.transfer *)

            | Transfer { dest; amount } ->
                dest.effect || amount.effect, true

            | Call { contract; amount; arg } ->
                contract.effect || amount.effect || arg.effect, true

            | If { cond = e1; ifthen = e2; ifelse = e3 }
            | MatchOption { arg = e1; ifnone = e2; ifsome = e3 }
            | MatchNat { arg = e1; ifplus = e2; ifminus = e3 }
            | MatchList { arg = e1; ifcons = e2; ifnil = e3 }
            | LoopLeft { body = e1; arg = e2 ; acc = Some e3 }
            | Fold { body = e1;  arg = e2; acc = e3 }
            | MapFold { body = e1;  arg = e2; acc = e3 } ->
                e1.effect || e2.effect || e3.effect, false
                (* e1.transfer || e2.transfer || e3.transfer *)

            | Apply { prim; args } ->
                let prim_eff = match prim with
                | Prim_extension (_, eff, _, _, _, _) -> eff
                | _ -> false
                in
                (
                    prim_eff || List.exists (fun e -> e.effect) args,
                    prim = Prim_set_delegate || prim = Prim_create_account
                    (* || List.exists (fun e -> e.transfer) args *)
                )

            | Closure { call_env; body } ->
                body.effect || List.exists (fun (_, e) -> e.effect) call_env,
                false (* e.transfer || List.exists (fun (_, e) -> e.transfer) env *)

            | Record fields ->
                List.exists (fun (_, e) -> e.effect) fields,
                false (* List.exists (fun (_, e) -> e.transfer) labels *)

            | MatchVariant { arg; cases } ->
                arg.effect || List.exists (fun (_, e) -> e.effect) cases,
                false (* e.transfer || List.exists (fun (_, e) -> e.transfer) cases *)

            | CreateContract { args } ->
                List.exists (fun e -> e.effect) args,
                true

            | TypeAnnot { e } ->
                e.effect, false (* e.transfer *)

            | Type _ ->
                false, false
            in
            { desc; name; loc; ty; bv; effect; transfer }
        )

    let rec eq_exp_desc eq_ty eq_var e1 e2 = match e1, e2 with
    | Const c1, Const c2 -> c1.const = c2.const && eq_types c1.ty c2.ty
    | Var v1, Var v2 -> eq_var v1 v2
    | Failwith e1, Failwith e2 -> eq_exp eq_ty eq_var e1 e2
    | Project p1, Project p2 ->
        p1.field = p2.field && eq_exp eq_ty eq_var p1.record p2.record
    | Constructor c1, Constructor c2 ->
        c1.constr = c2.constr && eq_exp eq_ty eq_var c1.arg c2.arg
    | ContractAt c1, ContractAt c2 ->
        eq_signature c1.c_sig c2.c_sig && eq_exp eq_ty eq_var c1.arg c2.arg
    | Unpack u1, Unpack u2 ->
        eq_types u1.ty u2.ty && eq_exp eq_ty eq_var u1.arg u2.arg
    | Lambda l1, Lambda l2 ->
        l1.arg_name.nname = l2.arg_name.nname && eq_types l1.arg_ty l2.arg_ty &&
        eq_types l1.ret_ty l2.ret_ty && eq_exp eq_ty eq_var l1.body l2.body
    | SetField s1, SetField s2 ->
        s1.field = s2.field && eq_exp eq_ty eq_var s1.record s2.record &&
        eq_exp eq_ty eq_var s1.set_val s2.set_val
    | Seq (x1, y1), Seq (x2, y2) ->
        eq_exp eq_ty eq_var x1 x2 && eq_exp eq_ty eq_var x1 x2
    | Let l1, Let l2 ->
        l1.bnd_var.nname = l2.bnd_var.nname && l1.inline = l2.inline &&
        eq_exp eq_ty eq_var l1.bnd_val l1.bnd_val &&
        eq_exp eq_ty eq_var l1.body l2.body
    | Loop l1, Loop l2 ->
        l1.arg_name.nname = l2.arg_name.nname && eq_exp eq_ty eq_var l1.arg l2.arg &&
        eq_exp eq_ty eq_var l1.body l2.body
    | LoopLeft l1, LoopLeft l2 ->
        l1.arg_name.nname = l2.arg_name.nname &&
        eq_exp eq_ty eq_var l1.arg l2.arg &&
        eq_exp eq_ty eq_var l1.body l2.body &&
        (
            match l1.acc, l2.acc with
            | None, None -> true
            | Some a1, Some a2 -> eq_exp eq_ty eq_var a1 a2
            | _ -> false
        )
    | Map m1, Map m2 ->
        m1.prim = m2.prim && m1.arg_name.nname = m2.arg_name.nname &&
        eq_exp eq_ty eq_var m1.arg m2.arg && eq_exp eq_ty eq_var m1.body m2.body
    | MapFold m1, MapFold m2 ->
        m1.prim = m2.prim && m1.arg_name.nname = m2.arg_name.nname &&
        eq_exp eq_ty eq_var m1.arg m2.arg && eq_exp eq_ty eq_var m1.acc m2.acc &&
        eq_exp eq_ty eq_var m1.body m2.body
    | Fold f1, Fold f2 ->
        f1.prim = f2.prim && f1.arg_name.nname = f2.arg_name.nname &&
        eq_exp eq_ty eq_var f1.arg f2.arg && eq_exp eq_ty eq_var f1.acc f2.acc &&
        eq_exp eq_ty eq_var f1.body f2.body
    | Call t1, Call t2 ->
        t1.entry = t2.entry &&
        eq_exp eq_ty eq_var t1.contract t2.contract &&
        eq_exp eq_ty eq_var t1.amount t2.amount &&
        eq_exp eq_ty eq_var t1.arg t2.arg
    | Transfer t1, Transfer t2 ->
        eq_exp eq_ty eq_var t1.dest t2.dest &&
        eq_exp eq_ty eq_var t1.amount t2.amount
    | If ite1, If ite2 ->
        eq_exp eq_ty eq_var ite1.cond ite2.cond &&
        eq_exp eq_ty eq_var ite1.ifthen ite2.ifthen &&
        eq_exp eq_ty eq_var ite1.ifelse ite2.ifelse
    | MatchOption m1, MatchOption m2 ->
        m1.some_name.nname = m2.some_name.nname &&
        eq_exp eq_ty eq_var m1.arg m2.arg &&
        eq_exp eq_ty eq_var m1.ifnone m2.ifnone &&
        eq_exp eq_ty eq_var m1.ifsome m2.ifsome
    | MatchNat n1, MatchNat n2 ->
        n1.plus_name.nname = n2.plus_name.nname &&
        n1.minus_name.nname = n2.minus_name.nname &&
        eq_exp eq_ty eq_var n1.arg n2.arg &&
        eq_exp eq_ty eq_var n1.ifplus n2.ifplus &&
        eq_exp eq_ty eq_var n1.ifminus n2.ifminus
    | MatchList m1, MatchList m2 ->
        m1.head_name.nname = m2.head_name.nname &&
        m1.tail_name.nname = m2.tail_name.nname &&
        eq_exp eq_ty eq_var m1.arg m2.arg &&
        eq_exp eq_ty eq_var m1.ifnil m2.ifnil &&
        eq_exp eq_ty eq_var m1.ifcons m2.ifcons
    | Apply a1, Apply a2 ->
        a1.prim = a2.prim && (
            try List.for_all2 (eq_exp eq_ty eq_var) a1.args a2.args
            with Invalid_argument _ -> false
        )
    | Closure c1, Closure c2 ->
        c1.arg_name.nname = c2.arg_name.nname && eq_types c1.arg_ty c2.arg_ty &&
        eq_types c1.ret_ty c2.ret_ty && eq_exp eq_ty eq_var c1.body c2.body &&
        (
            try List.for_all2 (fun (n1, e1) (n2, e2) ->
                n1 = n2 && eq_exp eq_ty eq_var e1 e2) c1.call_env c2.call_env
            with Invalid_argument _ -> false
        )
    | Record r1, Record r2 ->
        (
            try List.for_all2 (fun (n1, e1) (n2, e2) ->
                 n1 = n2 && eq_exp eq_ty eq_var e1 e2) r1 r2
            with Invalid_argument _ -> false
        )
    | MatchVariant m1, MatchVariant m2 ->
        eq_exp eq_ty eq_var m1.arg m2.arg &&
        (
            try List.for_all2 (fun (c1, e1) (c2, e2) ->
                c1 = c2 && eq_exp eq_ty eq_var e1 e2) m1.cases m2.cases
            with Invalid_argument _ -> false
        )
    | CreateContract c1, CreateContract c2 ->
        (
            try
                List.for_all2 (eq_exp eq_ty eq_var) c1.args c2.args &&
                eq_types c1.contract.storage c2.contract.storage &&
                List.for_all2 (
                    fun (v1, i1, e1) (v2, i2, e2) ->
                        v1 = v2 && i1 = i2 && eq_exp eq_ty eq_var e1 e2
                )
                c1.contract.values c2.contract.values &&
                List.for_all2 (
                    fun e1 e2 ->
                        e1.entry_sig.entry_name = e2.entry_sig.entry_name &&
                        e1.entry_sig.parameter_name = e2.entry_sig.parameter_name &&
                        e1.entry_sig.storage_name = e2.entry_sig.storage_name &&
                        eq_types e1.entry_sig.parameter e2.entry_sig.parameter &&
                        eq_exp eq_ty eq_var e1.code e2.code
                ) c1.contract.entries c2.contract.entries
            with Invalid_argument _ -> false
        )
    | TypeAnnot a1, TypeAnnot a2 ->
        eq_exp eq_ty eq_var a1.e a2.e && eq_types a1.ty a2.ty
    | _, _ -> false

    (** Generic equality between expressions modulo location, renaming, etc. *)
    and eq_exp eq_ty eq_var e1 e2 =
        eq_ty e1.ty e2.ty && eq_exp_desc eq_ty eq_var e1.desc e2.desc

    (** Instances of above function {!eq_exp} *)
    let eq_typed eq_var e1 e2 = eq_exp eq_types eq_var e1 e2
    let eq_syntax e1 e2 = eq_exp (fun _ _ -> true) (=) e1 e2
end

module Env = struct

    let rec rec_find s env proj = try StringMap.find s (proj env) with Not_found -> (
        match env.top_env with
        | None -> raise Not_found
        | Some env -> rec_find s env proj
    )

    module Find = struct
        let dtype s env = rec_find s env (fun env -> env.types)
        let contract_type s env = rec_find s env (fun env -> env.contract_types)
        let label s env = rec_find s env (fun env -> env.labels)
        let constr s env = rec_find s env (fun env -> env.constrs)
    end

    module TCheck = struct
        let empty ~warnings t_contract_sig env = {
            warnings;
            decompiling = false;
            annot = false;
            counter = ref 0;
            vars = StringMap.empty;
            vars_counts = StringMap.empty;
            to_inline = ref StringMap.empty;
            force_inline = ref StringMap.empty;
            env = env;
            clos_env = None;
            t_contract_sig;
        }
    end
end

(** Functions over identifiers (strings). *)
module Idents = struct
    let reserved_keywords = [
        "let"; "in"; "match" ; "int"; "bool"; "string"; "bytes";
        "get"; "set"; "tuple"; "with"; "fun"; "or"; "and"; "land";
        "lor"; "xor"; "not"; "lsl"; "lsr"; "lxor"; "abs"; "type";
        "is_nat";
        "at"; (* Reserved for ContractSig.at *)
    ]

    let is_reserved s = List.mem s reserved_keywords

    let has_reserved_prefix s =
        (* [
                "tz1"; "tz2"; "tz3" ;
                "edpk"; "sppk"; "p2pk";
                "edsig"; "spsig1"; "p2sig";
            ]
        *)
        let len = String.length s in
        if not (len >= 3) then false else (
            match s.[0], s.[1], s.[2] with
            | 't', 'z', ('1' | '2' | '3') -> true
            | 'e', 'd', 'p'
            | 's', 'p', 'p'
            | 'p', '2', 'p' -> len >= 4 && s.[3] = 'k'
            | 'e', 'd', 's'
            | 'p', '2', 's' -> len >= 5 && s.[3] = 'i' && s.[4] = 'g'
            | 's', 'p', 's' -> len >= 6 && s.[3] = 'i' && s.[4] = 'g' && s.[4] = '1'
            | _ -> false
        )

    module Entry = struct
        let prefix = "_Liq_entry_"
        let add_prefix s = prefix ^ s

        let name_of_prefixed s =
            Scanf.sscanf s
                (Scanf.format_from_string prefix "" ^^ "%s%!")
                (fun x -> x)

        let is_prefixed s =
            try
                ignore (name_of_prefixed s);
                true
            with _ -> false
    end

    module Contract = struct
        let prefix = "_Liq_contract_"
        let add_prefix s = prefix ^ s

        let name_of_prefixed s =
            Scanf.sscanf s
                (Scanf.format_from_string prefix "" ^^ "%s%!")
                (fun x -> x)
    end
end
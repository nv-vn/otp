open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Lwt
open Cohttp
open Cohttp_lwt_unix

exception JsonException

type ct_or_tk =
  Ct of core_type | Tk of type_kind

let uniq =
  let x = ref 0 in
  fun start ->
    x := !x + 1;
    start ^ string_of_int !x

let get_json url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
    body |> Cohttp_lwt_body.to_string >|= fun body -> body

let yj_to_string = function
  | `Null -> ""
  | any -> Yojson.Basic.Util.to_string any

let rec make_record json loc =
  let open Yojson.Basic.Util in
  let items = to_list json in
  let pairs = List.map (fun x -> (x |> member "name" |> to_string,
                                  x |> member "type" |> to_string)) items in
  Ptype_record (List.map (fun (name, typ) ->
                           Type.field ~loc {txt = name; loc = loc}
                             (Typ.constr ~loc {txt = Lident typ; loc = loc} [])) pairs)

and make_product json loc =
  let open Yojson.Basic.Util in
  let items = to_list json |> filter_string in
  Typ.tuple ~loc (List.map (fun item -> Typ.constr ~loc {txt = Lident item; loc = loc} []) items)

and make_alias json loc =
  let open Yojson.Basic.Util in
  let name = to_string json in
  Typ.constr ~loc {txt = Lident name; loc = loc} []

let type_of_url url loc =
  get_json url >>= fun text ->
    let json = Yojson.Basic.from_string text in
    (* Now process the JSON *)
    let open Yojson.Basic.Util in
    let record  = json |> member "record"
    and product = json |> member "product"
    and alias   = json |> member "alias" in
    return @@ match (record, product, alias) with
    | (record, `Null, `Null)  -> Tk (make_record  record  loc)
    | (`Null, product, `Null) -> Ct (make_product product loc)
    | (`Null, `Null, alias)   -> Ct (make_alias   alias   loc)
    | (_, _, _) -> Ct (make_alias (`String "int") loc)

let struct_of_url url mapper mapper' loc =
  Mod.structure ~loc
    [Str.module_ ~loc
      (Mb.mk ~loc {txt = "Hide"; loc = loc}
        (Mod.structure ~loc
          [Str.type_ ~loc
            [mapper.type_declaration mapper' (Type.mk ~loc {txt = "t"; loc = loc}
              ~manifest:(Typ.extension ~loc
                ({txt = "json_type"; loc = loc},
                 PStr [Str.eval ~loc (Exp.constant ~loc (Const_string (url, None)))])))]]))]

let rec json_type_mapper argv =
  { default_mapper with
    type_declaration = begin fun mapper type_decl ->
      match type_decl with
      | { ptype_attributes; ptype_name; ptype_manifest = Some {
            ptyp_desc = Ptyp_extension ({txt = "json_type"; loc}, pstr) } } ->
        begin match pstr with
        | PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
          begin match Lwt_unix.run @@ type_of_url sym loc with
          | Ct ct -> {type_decl with ptype_manifest = Some ct}
          | Tk tk -> {type_decl with ptype_kind = tk; ptype_manifest = None}
          end
        | _ ->
          raise (Location.Error
                  (Location.error ~loc "[%json_type ...] accepts a string, e.g. [%json_type \"http://google.com\"]"))
        end
      | x -> default_mapper.type_declaration mapper x
    end;
    module_expr = begin fun mapper mod_expr ->
      match mod_expr with
      | { pmod_attributes; pmod_loc; pmod_desc = Pmod_extension ({txt = "json"; loc}, pstr) } ->
        begin match pstr with
        | PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
          struct_of_url sym (json_type_mapper argv) mapper loc
        | _ ->
          raise (Location.Error
                  (Location.error ~loc "[%json ...] accepts a string, e.g. [%json \"http://google.com\"]"))
        end
      | x -> default_mapper.module_expr mapper x
    end;
  }

let _ = register "json_type" json_type_mapper

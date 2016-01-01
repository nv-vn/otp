open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Lwt
open Cohttp
open Cohttp_lwt_unix

exception JsonException

type ty =
  | Name of string
  | List of ty
  | Product of ty list
  | Record of (string * ty) list

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

let rec ty_of_json json =
  let open Yojson.Basic.Util in
  match json |> member "kind" |> yj_to_string with
  | "name" -> Name (json |> member "val" |> yj_to_string)
  | "list" -> List (json |> member "val" |> ty_of_json)
  | "product" ->
    let items = json |> member "val" |> to_list in
    Product (List.map (fun item -> ty_of_json item) items)
  | "record" ->
    let fields = json |> member "val" |> to_list in
    Record (fields |> List.map (fun field ->
      (field |> member "name" |> to_string, field |> member "type" |> ty_of_json)))
  | _ -> Name "unit"

let tconcat l r =
  match l, r with
  | {pstr_desc = Pstr_type lxs; pstr_loc}, {pstr_desc = Pstr_type rxs; _} ->
    Str.type_ ~loc:pstr_loc (lxs @ rxs)
  | _ -> raise JsonException

let deriving loc attrs =
  let idents = List.map (fun attr -> Exp.ident ~loc {txt = Lident attr; loc = loc}) attrs in
  [{txt = "deriving"; loc = loc},
   PStr [Str.eval ~loc (Exp.tuple ~loc idents)]]

let rec make_types name loc = function
  | Name s ->
    Str.type_ ~loc
      [Type.mk ~loc {txt = name; loc = loc}
        ~attrs:(deriving loc ["show"; "yojson"])
        ~manifest:(Typ.constr ~loc {txt = Lident s; loc = loc} [])]
  | List t ->
    let dname = uniq name in
    let dep = make_types dname loc t in
    let dconstr = Typ.constr ~loc {txt = Lident dname; loc = loc} [] in
    tconcat dep (Str.type_ ~loc
                  [Type.mk ~loc {txt = name; loc = loc}
                    ~attrs:(deriving loc ["show"; "yojson"])
                    ~manifest:(Typ.constr ~loc {txt = Lident "list"; loc = loc} [dconstr])]) 
  | Product xs ->
    begin
      let items, names = create loc name xs in
      let tuple = List.map (fun item -> Typ.constr ~loc {txt = Lident item; loc = loc} []) names in
      tconcat items (Str.type_ ~loc
                      [Type.mk {txt = name; loc = loc}
                        ~attrs:(deriving loc ["show"; "yojson"])
                        ~manifest:(Typ.tuple ~loc tuple)])
    end
  | Record xs ->
    begin
      let types = List.map (fun (name, typ) -> typ) xs in
      let items, names = create loc name types in
      let fields = List.map2 (fun (name, typ) type_ ->
                               Type.field ~loc {txt = name; loc = loc}
                                 (Typ.constr {txt = Lident type_; loc = loc} [])) xs names in
      tconcat items (Str.type_ ~loc
                      [Type.mk {txt = name; loc = loc}
                        ~attrs:(deriving loc ["show"; "yojson"])
                        ~kind:(Ptype_record fields)])
    end

and create loc name ?(items=Str.type_ ~loc []) ?(names=[]) = function
  | [] -> items, names
  | x :: xs ->
    let n = uniq name in
    let items' = tconcat items (make_types n loc x) in
    create loc name ~items:items' ~names:(names @ [n]) xs


let struct_of_url ?(extra=false) url loc =
  get_json url >>= fun text ->
    let json = Yojson.Basic.from_string text in
    let ty = ty_of_json json in
    let struct_members = make_types "t" loc ty in
    return @@ Mod.structure ~loc begin
      [Str.module_ ~loc
        (Mb.mk ~loc {txt = "Hide"; loc = loc}
          (Mod.structure ~loc [struct_members]));
       Str.type_ ~loc
         [Type.mk {txt = "t"; loc = loc}
           ~manifest:(Typ.constr ~loc {txt = Ldot (Lident "Hide", "t"); loc = loc} [])];
       Str.value ~loc Nonrecursive
         [Vb.mk ~loc (Pat.var ~loc {txt = "show"; loc = loc})
                     (Exp.ident ~loc {txt = Ldot (Lident "Hide", "show"); loc = loc});
          Vb.mk ~loc (Pat.var ~loc {txt = "to_json"; loc = loc})
                     (Exp.ident ~loc {txt = Ldot (Lident "Hide", "to_yojson"); loc = loc});
          Vb.mk ~loc (Pat.var ~loc {txt = "of_json"; loc = loc})
                     (Exp.ident ~loc {txt = Ldot (Lident "Hide", "of_yojson"); loc = loc})]]
     @ if extra then
       [[%stri let to_string obj =
                 Yojson.Safe.pretty_to_string (Conglomerate.to_json obj)];
        [%stri let from_string str =
                 of_json (Yojson.Safe.from_string str)];
        [%stri let from_url url =
                 let open Lwt in
                 let open Cohttp in
                 let open Cohttp_lwt_unix in
                 (Client.get (Uri.of_string url) >>= fun (resp, body) ->
                   body |> Cohttp_lwt_body.to_string >|= fun body ->
                     body) >>= fun text ->
                       return @@ of_json (Yojson.Safe.from_string text)]]
     else [] end

let rec json_type_mapper argv =
  { default_mapper with
    module_expr = begin fun mapper mod_expr ->
      match mod_expr with
      | { pmod_attributes; pmod_loc; pmod_desc = Pmod_extension ({txt = "json"; loc}, pstr) } ->
        begin match pstr with
        | PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
          Lwt_unix.run @@ struct_of_url sym loc
        | _ ->
          raise (Location.Error
                  (Location.error ~loc "[%json ...] accepts a string, e.g. [%json \"http://google.com\"]"))
        end
      | { pmod_attributes; pmod_loc; pmod_desc = Pmod_extension ({txt = "json_extra"; loc}, pstr) } ->
        begin match pstr with
        | PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
          Lwt_unix.run @@ struct_of_url ~extra:true sym loc
        | _ ->
          raise (Location.Error
                  (Location.error ~loc "[%json_extra ...] accepts a string, e.g. [%json_extra \"http://google.com\"]"))
        end
      | x -> default_mapper.module_expr mapper x
    end;
  }

let _ = register "json_type" json_type_mapper

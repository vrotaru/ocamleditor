(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)

open Printf
open Location
open Lexing
open Miscellanea


module Log = Common.Log.Make(struct let prefix = "Binannot" end)
let _ = Log.set_verbosity `ERROR

type name = string

type definition = {
  mutable def_name  : string;
  mutable def_loc   : Location.t;
  mutable def_scope : Location.t
}

type ident_kind =
  | Def of definition
  | Def_constr of definition
  | Def_module of definition
  | Int_ref of Location.t (* Location of its defintion *)
  | Ext_ref
  | Open of Location.t (* Scope *)

type ident = {
  mutable ident_fname : string; (* Filename *)
  mutable ident_kind  : ident_kind;
  ident_loc           : name Location.loc; (* Name of the ident and location *)
}

type entry = { (* An entry collects all ident annotations of a source file. *)
  timestamp           : float; (* mtime of the cmt file read *)
  locations           : ident option array; (* Map from file offsets to idents *)
  int_refs            : (Location.t, ident) Hashtbl.t; (* Map from def's locations to all its internal refs. *)
  ext_refs            : (string, ident) Hashtbl.t; (* Map from module names (compilation units) to all external references *)
  mutable definitions : definition list; (* List of all definitions in the file *)
}

module Longident = struct
  include Longident

  (*
    parse "A.B.c";;
    - : t = Ldot (Ldot (Lident "A", "B"), "c")
    flatten (parse "A.B.c");;
    - : string list = ["A"; "B"; "c"]
    last (parse "A.B.c");;
    - : string = "c"
  *)

  let qualified = function
    | Lident _ -> false
    | Ldot _ -> true
    | Lapply _ -> true

  let of_type_expr typ name =
    match typ with
    | Ldot (path, _) -> Ldot (path, name)
    | Lapply (path, _) -> Ldot (path, name)
    | Lident _ -> Longident.Lident name

end

let table_idents : (string, entry) Hashtbl.t = Hashtbl.create 7 (* source filename, entry *)

let string_of_kind = function
  | Def _ -> "Def"
  | Def_module _ -> "Def_module"
  | Def_constr _ -> "Def_constr"
  | Int_ref _ -> "Int_ref"
  | Ext_ref -> "Ext_ref"
  | Open _ -> "Open"

let string_of_loc loc =
  let filename, _, _ = Location.get_pos_info loc.loc_start in
  sprintf "%s:%d--%d" filename (loc.loc_start.pos_cnum) (loc.loc_end.pos_cnum);;

let linechar_of_loc loc =
  let _, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  ((a - 1), b), ((c - 1), d)

let cnum_of_loc loc =
  loc.loc_start.pos_fname, loc.loc_start.pos_cnum, loc.loc_end.pos_cnum

let string_of_type_expr te = Odoc_info.string_of_type_expr te;;
let lid_of_type_expr te = Longident.parse (string_of_type_expr te)

let (<==) loc offset = loc.loc_start.pos_cnum <= offset && offset <= loc.loc_end.pos_cnum
let (<==<) loc offset = loc.loc_start.pos_cnum <= offset && (offset <= loc.loc_end.pos_cnum || loc.loc_end.pos_cnum = -1)

(** print_ident *)
let print_ident ?filter {ident_kind; ident_loc; _} =
  let loc' =
    match ident_kind with
      | Def loc -> "scope: " ^ (string_of_loc loc.def_scope)
      | Def_module def -> "def: " ^ (string_of_loc def.def_loc)
      | Def_constr def -> "def: " ^ (string_of_loc def.def_loc)
      | Int_ref def_loc -> "def: " ^ (string_of_loc def_loc)
      | Ext_ref -> "---"
      | Open scope -> "scope: " ^ (string_of_loc scope)
  in
  let { Asttypes.txt; loc } = ident_loc in
  match filter with
    | Some x when x <> txt -> ()
    | _ ->
      printf "%-11s: %-30s (use: %-12s) (%-19s) %s\n%!"
        (String.uppercase_ascii (string_of_kind ident_kind))
        ident_loc.txt
        (string_of_loc loc)
        loc'
        loc.loc_start.pos_fname;
;;






open Miscellanea
open Binannot
open Binannot_type

module Log = Common.Log.Make(struct let prefix = "Cmt_common" end)
let _ = Log.set_verbosity `ERROR

type t = {
  ba_loc  : Location.t;
  ba_type : string;
}

exception Found of t


(** read_cmt *)
let read_cmt ~project ~filename:source ?(timestamp=0.) ?compile_buffer () =
  try
    let result =
      let ext = if source ^^^ ".ml" then Some ".cmt" else if source ^^^ ".mli" then Some ".cmti" else None in
      match ext with
        | Some ext ->
          Option.fold (Project.tmp_of_abs project source) ~none:None ~some:(fun (tmp, relname) ->
            let source_tmp = tmp // relname in
            let source_avail =
              if Sys.file_exists source_tmp then source_tmp
              else Option.fold compile_buffer ~none:source ~some:(fun f -> f (); source_tmp)
            in
            let cmt = (Filename.chop_extension source_avail) ^ ext in
            let mtime_cmt = (Unix.stat cmt).Unix.st_mtime in
            if mtime_cmt = timestamp then None else begin
              let mtime_src = (Unix.stat source_avail).Unix.st_mtime in
              if mtime_cmt >= mtime_src then begin
                Some (source, mtime_cmt, Cmt_format.read_cmt cmt)
              end else None
            end
          )
        | _ -> None
      in
      result
  with
    | Unix.Unix_error (Unix.ENOENT as err, "stat", _) as ex ->
      Log.println `WARN "%s - %s" (Printexc.to_string ex) (Unix.error_message err);
      None
    | Unix.Unix_error (err, _, _) as ex ->
      Log.println `ERROR "%s - %s" (Printexc.to_string ex) (Unix.error_message err);
      None
    | ex ->
      Log.println `ERROR "%s" (Printexc.to_string ex);
      None
;;

(** find_by_offset *)
let find_by_offset ~project ~filename ~offset ?compile_buffer () =
  match read_cmt ~project ~filename ?compile_buffer () with
    | Some (_, _, cmt) ->
      begin
        let f ba_loc ba_type =
          Log.println `TRACE "%d; %s : %s" offset (string_of_loc ba_loc) ba_type;
          raise (Found {ba_loc; ba_type});
        in
        try
          Odoc_info.reset_type_names();
          begin
            match [@warning "-4"] cmt.cmt_annots with
              | Implementation {str_items; _} -> List.iter (find_structure_item f offset) str_items
              | Partial_implementation parts -> Array.iter (find_part_impl f offset) parts
              | _ -> ()
          end;
          None
        with Found ba -> Some ba
      end;
    | _ -> None

(** find *)
let find ~page ?iter () =
  let compile_buffer () = page#compile_buffer ?join:(Some true) () in
  let iter = match iter with Some it -> it | _ -> page#buffer#get_iter `INSERT in
  find_by_offset ~project:page#project ~filename:page#get_filename ~offset:iter#offset ~compile_buffer ()


(** register *)
let register filename entry ({ident_loc; ident_kind; _} as ident) =
  let { Asttypes.loc; _ } = ident_loc in
  if loc <> Location.none then begin
    ident.ident_fname <- filename;
    let start = loc.loc_start.pos_cnum in
    let stop  = loc.loc_end.pos_cnum in
    for i = start to stop do entry.locations.(i) <- Some ident done;
    match ident_kind with
      | Int_ref x when x = Location.none ->
        begin
          match List_opt.find begin fun {def_name; def_scope; _} ->
            def_name = ident.ident_loc.txt  && (def_scope <== start || def_scope = Location.none)
          end entry.definitions with
            | Some def ->
              ident.ident_kind <- (Int_ref def.def_loc);
              Hashtbl.add entry.int_refs def.def_loc ident;
            | _ -> () (*assert false*)
        end;
      | Int_ref def_loc ->
        Hashtbl.add entry.int_refs def_loc ident;
      | Ext_ref | Open _ ->
        begin
          match Longident.flatten (Longident.parse ident.ident_loc.txt) with
            | modname :: _ -> Hashtbl.add entry.ext_refs modname ident
            | _ -> ()
        end;
      | (Def def | Def_constr def | Def_module def) as ident_def ->
        if def.def_name = "" then (def.def_name <- ident_loc.txt);
        if def.def_loc = Location.none then def.def_loc <- loc;
        begin
          match ident_def with
            | Def_module def ->
              let parents = List.filter (fun d -> d.def_scope <== def.def_loc.loc_start.pos_cnum) entry.definitions in
              let parents = List.map (fun d -> d.def_name) parents in
              let path = if parents = [] then (Miscellanea.modname_of_path filename) else String.concat "." parents in
              def.def_name <- String.concat "." (List.filter ((<>) "") [path; def.def_name]);
            | Def _ | Def_constr _ | Int_ref _ | Ext_ref | Open _ -> ()
        end;
        entry.definitions <- def :: entry.definitions;
  end

(** scan *)
let critical_scan = Mutex.create()

let scan ~project ~filename ?compile_buffer () =
  Mutex.lock critical_scan;
  try
    begin
      let timestamp = try (Hashtbl.find table_idents filename).timestamp with Not_found -> 0. in
      match read_cmt ~project ~filename ~timestamp ?compile_buffer () with
        | Some (filename, timestamp, ({cmt_sourcefile = Some cmt_sourcefile; _} as cmt)) ->
          let size = (Unix.stat (cmt.cmt_builddir // cmt_sourcefile)).Unix.st_size + 1 in
          let entry = {
            timestamp;
            locations   = Array.make size None;
            int_refs    = Hashtbl.create 7;
            ext_refs    = Hashtbl.create 7;
            definitions = [];
          } in
          Hashtbl.replace table_idents filename entry;
          let f = register filename entry in
          begin
            match [@warning "-4"] cmt.cmt_annots with
              | Implementation {str_items; _} ->
                List.iter (fun item -> ignore (Binannot_ident_scan.iter_structure_item f item)) str_items;
              (*| Partial_implementation parts -> Array.iter (fold_part_impl f) parts*)
              | _ -> ()
          end
        | _ -> ()
    end;
    Mutex.unlock critical_scan;
  with ex -> begin
    Mutex.unlock critical_scan;
    raise ex;
  end
;;

(** scan_project_files *)
let scan_project_files ~project ?(sort=true) f =
  let src_files = File_util.readdirs ~links:false (Some (fun x -> x ^^^ ".ml")) (project.Prj.root // Prj.default_dir_src) in
  let src_files = if sort then List.sort compare src_files else src_files in
  List.fold_left begin fun acc filename ->
    scan ~project ~filename ();
    try
      let entry = Hashtbl.find table_idents filename in
      f acc filename entry
    with Not_found -> acc
  end [] src_files
;;



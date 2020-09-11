open Miscellanea



module Log = Common.Log.Make(struct let prefix = "Cmt_common" end)
let _ = Log.set_verbosity `ERROR

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

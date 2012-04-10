(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


open Miscellanea
open Printf

let show ~editor ~page () =
  let window = GWindow.file_chooser_dialog
    ~action:`SAVE ~icon:Icons.oe
    ~title:(sprintf "Save \xC2\xAB%s\xC2\xBB as..." (Filename.basename page#get_filename))
    ~position:`CENTER ~modal:true ~show:false ()
  in
  window#set_select_multiple false;
  window#add_select_button_stock `OK `OK;
  window#add_button_stock `CANCEL `CANCEL;
  window#set_default_response `OK;
  ignore (window#set_filename page#get_filename);
  let rec run () =
    match window#run () with
      | `OK ->
        Gaux.may window#filename ~f:begin fun filename ->
          let buffer : GText.buffer = page#buffer#as_text_buffer#as_gtext_buffer in
          let write_file () =
            File.write filename (buffer#get_text());
            page#revert();
            (*editor#close page;*)
            match editor#open_file ~active:true ~offset:0 filename with
              | Some page ->
                editor#load_page ?scroll:None page;
                editor#goto_view page#view;
                window#destroy()
              | _ ->
                Dialog.info ~title:"Error" ~message_type:`ERROR
                  ~message:(sprintf "Cannot open file %s" filename) window;
          in
          if Sys.file_exists filename then begin
            let overwrite () =
              if Sys.os_type = "Win32" then begin
                (* Because of the Win32 case insensitiveness of filenames
                   we delete the target file and close the editor page of
                   the target file *)
                Sys.remove filename;
                let lc_filename = String.lowercase filename in
                List_opt.may_find (fun p -> String.lowercase p#get_filename = lc_filename)
                  editor#pages editor#close ();
              end;
              write_file ()
            in
            match
              Dialog.confirm ~message:(sprintf
                "Are you sure you want to overwrite file \xC2\xAB%s\xC2\xBB?" (Filename.basename filename))
                ~yes:("Overwrite", overwrite)
                ~no:("Do Not Overwrite", ignore)
                ~title:"Overwrite File" window;
            with `CANCEL | `NO -> run() | `YES -> window#destroy()
          end else (write_file())
        end;
      | _ -> window#destroy()
  in run()





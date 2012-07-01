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


let is_win32 = Sys.os_type = "Win32"

(** Directories *)
let user_home =
  try Sys.getenv "HOME" with Not_found ->
    (try (Sys.getenv "HOMEDRIVE") // (Sys.getenv "HOMEPATH")
    with Not_found -> failwith "Please set your HOME environment variable.")

let ocamleditor_user_home =
  let dirname =
    if try ignore (Sys.getenv "OCAMLEDITOR_MINGW"); true with Not_found -> false then ".ocamleditor.mingw"
    else if Common.application_debug then ".ocamleditor.test"
    else ".ocamleditor"
  in
  let ocamleditor_user_home = user_home // dirname in
  if not (Sys.file_exists ocamleditor_user_home) then (Unix.mkdir ocamleditor_user_home 509);
  ocamleditor_user_home


(** Configuration Section =================================================== *)

let editor_pixels_above_lines            = 0
let editor_pixels_below_lines            = 0
let save_all_before_compiling            = true
let autosave_enabled                     = true
let autosave_interval                    = 5_000 (* milliseconds *)
let autosave_keep_backup                 = 3. *. 24. *. 60. *. 60.  (* 3 days, in milliseconds *)
let current_line_border_enabled          = true
let ocamldoc_paragraph_border_enabled    = true
let ocamldoc_paragraph_bgcolor_enabled   = true
let fade_window_enabled                  = true (* Fade effect for popup windows *)
let dot_leaders_enabled                  = true
let indent_lines_solid_color             = `NAME "#d0d0d0" (*"#e3e3e3"*)
let indent_lines_dashed_color            = `NAME "#a0a0a0" (*"#c9c9c9"*)
let matching_delim_border_color          = `NAME "#ff0000"
let right_margin_line_color              = `NAME "#e0e0e0"
let error_popup_bg_color                 = `NAME "#ffeef2"
let error_popup_border_color             = `NAME "#ff6a99"
let error_underline_color                = `NAME "#ff0000"
let warning_popup_bg_color               = `NAME "#fff4e8"
let warning_popup_border_color           = `NAME "#ffc56a"
let warning_underline_color              = warning_popup_border_color
let warning_unused_color                 = "#c0c0c0"
let warning_unused_properties            = [`FOREGROUND warning_unused_color; `STYLE `ITALIC]
let warning_tootip_enabled               = false
(* Gutter colors:
  `CALC factor    : Calculated according to the bg color of the text view.
                    [darker] 0.5 <= factor <= 1.0 [same as text view]
  `THEME          : Based on the GTK theme.
  `NAME "#ffffff" : Specific color. *)
let gutter_bg_color                      = (*`THEME*) `CALC 0.93
let gutter_fg_color                      = (*`THEME*) `CALC 0.60
let gutter_border_color                  = (*`THEME*) `CALC 0.875
let gutter_marker_color                  = (*`THEME*) `CALC 0.65
let code_folding_scope_color             = `NAME "#e5e5e5" (* disabled *)
let code_folding_highlight_color         = "#eeeeee"
let code_folding_hightlight_gradient     = ["#f4f4f4"; "#f9f9f9"; "#fefefe"] (* [] for no gradient *)
let horizontal_line_color                = `NAME "#000000" (* Line drawn by clicking on the gutter. *)
let code_folding_font                    = ref (Some "-*-*-medium-r-*-sans-10-*-*-*-*-*-*-*")
                                          (* Font for the "n lines" label in the fold line; it must be 10 pixels height. None for no label *)
let global_gutter_comments_enabled       = true
let global_gutter_comments_color         = `NAME "#00db00"
let global_gutter_comments_bgcolor       = `NAME "#d2edd2"
let global_gutter_no_errors              = `NAME "#daedd0"
let find_references_title_bgcolor        = "#000000"
let find_references_title_fgcolor        = "#ffffff"
let find_replace_history_max_length      = 75
(* Condensed font for the file list in the search results pane. None is default font. (`STRETCH `CONDENSED doesn't work) *)
let find_text_output_font_condensed      = Some (match Sys.os_type with "Win32" -> "Arial" | _ -> "Helvetica") (*None*)
let find_text_output_border_color        = `NAME "#707070" (* Current line border color of the find text output pane *)
let find_text_output_highlight           = `DEFAULT, `DEFAULT (*`NAME "#ffff7e", `NONE*) (* Background and foreground colors to highlight occurrences where the pattern matches.
                                          (`NONE=do not change color; `DEFAULT=default color; `NAME=specific color)*)
let find_text_output_linenumber_fgcolor  = `FOREGROUND "#000000"
let file_history_filename                = ocamleditor_user_home // "file_history"
let file_history_max_length              = 300
let project_history_filename             = ocamleditor_user_home // "project_history"
let project_history_max_length           = 15
let location_history_proximity           = 20 (* characters *)
let location_history_max_length          = 30 (* hint *)
let location_history_max_edit            = 5
let module_browser_max_results           = 150 (* Max. number of search results to display in the search_entry as you type *)
let module_browser_secondary_title_color = "#877033"
let completion_popup_default_dimensions  = 640, 300
let odoc_tag_properties                  = [ (* These properties apply to ocamldoc comments only, not to the type descriptions. *)
                                          `PIXELS_INSIDE_WRAP 2;
                                          `PIXELS_BELOW_LINES 2;
                                          `WRAP_MODE `WORD]
let outline_type_color                   = module_browser_secondary_title_color
let layout_find_references               = `VERTICAL
let layout_find_module_browser           = `VERTICAL
(* Path relative to the project home directory where to find custom templates. *)
let template_project_filename            = ".extensions" // "templates.cma"
(* Adjustments according to the GTK version *)
let gtk_major, gtk_minor, _              = GMain.Main.version
let current_line_border_adjust, dash_style, dash_style_offset =
  match gtk_major, gtk_minor with
    | 2, 14 -> 2, `ON_OFF_DASH, None
    | 2, 16 -> 2, `DOUBLE_DASH, None
    | 2, 20 -> 1, `ON_OFF_DASH, (Some 2)
    | 2, 22 -> 2, `DOUBLE_DASH, None
    | 2, 24 -> 1, `ON_OFF_DASH, (Some 2)
    | _     -> 1, `DOUBLE_DASH, None

(** End of Configuration Section ============================================ *)




let title = "OCamlEditor"
let version = "1.7.4"

let ocaml_codeset = "ISO-8859-1"

let _ = Printexc.record_backtrace true
let _ = Unix.putenv "TERM" ""
let getenv_ocamllib = try Some (Sys.getenv "OCAMLLIB") with Not_found -> None

let _ =
  (* Check whether "code_folding_font" can be loaded. *)
  match !code_folding_font with
    | None -> ()
    | Some fontset ->
      begin
        try ignore (Gdk.Font.load_fontset fontset)
        with Gpointer.Null -> begin
          eprintf "Warning: could not load fontset \"%s\".\n%!" fontset;
          code_folding_font := None
        end
      end;;

let find_best ?(param="--help") prog =
  let redirect_stderr = if Sys.os_type = "Win32" then " 2>NUL" else " 2>/dev/null" in
  try
    List.find begin fun comp ->
      let ok =
        try
          let cmd = sprintf "%s %s%s" (Filename.quote comp) param redirect_stderr in
          if Common.application_debug then (printf "Checking for %s... %!" cmd);
          ignore (Cmd.expand ~first_line:true cmd);
          true
        with _ -> false
      in
      if Common.application_debug then (printf "%b\n%!" ok);
      ok
    end prog
  with Not_found ->
    kprintf failwith "Cannot find: %s" (String.concat ", " prog)

(** Directories *)
let ocamleditor_bin = !! Sys.executable_name

(** Commands *)
let oebuild_command =
  let commands = [
    begin
      let basename = "oebuild.opt" ^ (if is_win32 then ".exe" else "") in
      let filename = (Sys.getcwd()) // "oebuild" // basename in
      if Sys.file_exists filename then filename
      else begin
        let path = (!! Sys.executable_name) // basename in
        if Sys.file_exists path then path else basename;
      end
    end;
    begin
      let basename = "oebuild" ^ (if is_win32 then ".exe" else "") in
      let filename = (Sys.getcwd()) // "oebuild" // basename in
      if Sys.file_exists filename then filename
      else begin
        let path = (!! Sys.executable_name) // basename in
        if Sys.file_exists path then path else basename;
      end
    end;
  ] in
  find_best commands

let oeproc_command =
  if is_win32 then
    let commands = [
      begin
        let basename = "oeproc.opt.exe" in
        let filename = (Sys.getcwd()) // "oeproc" // basename in
        if Sys.file_exists filename then filename else ((Sys.getcwd()) // basename)
      end;
      begin
        let basename = "oeproc.exe" in
        let filename = (Sys.getcwd()) // "oeproc" // basename in
        if Sys.file_exists filename then filename else ((Sys.getcwd()) // basename)
      end;
    ] in
    find_best ~param:"" commands
  else "unused"

(** Clear OCAMLLIB environment variable *)
let _ = Ocaml_config.putenv_ocamllib None

(** Theme *)
let _ =
  let themes = (Filename.dirname ocamleditor_bin) // "share" // "themes" in
  if is_win32 && Sys.file_exists themes
  then begin
    let themes =
      List.filter begin fun x ->
        let name = themes // x in
        Sys.is_directory name && x.[0] <> '#'
      end (Array.to_list (Sys.readdir themes))
    in
    match themes with
      | theme_name :: _ ->
        kprintf GtkMain.Rc.parse_string "gtk-theme-name = \"%s\"" theme_name;
        GtkMain.Rc.parse_string "gtk-font-name=\"Sans 8\"";
      | _ -> ()
  end;;











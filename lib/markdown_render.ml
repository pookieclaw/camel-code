(** Post-process streamed text with markdown formatting.

    Called after streaming completes to re-render the full response
    with proper ANSI styling. *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let italic s = Printf.sprintf "\027[3m%s\027[0m" s
let cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let magenta s = Printf.sprintf "\027[35m%s\027[0m" s

(** Render inline formatting. *)
let render_inline text =
  let buf = Buffer.create (String.length text) in
  let i = ref 0 in
  let len = String.length text in
  while !i < len do
    if !i + 1 < len && text.[!i] = '`' && text.[!i+1] <> '`' then begin
      let start = !i + 1 in
      let j = ref start in
      while !j < len && text.[!j] <> '`' do incr j done;
      if !j < len then begin
        Buffer.add_string buf (cyan (String.sub text start (!j - start)));
        i := !j + 1
      end else begin
        Buffer.add_char buf text.[!i]; incr i
      end
    end else if !i + 2 < len && text.[!i] = '*' && text.[!i+1] = '*' then begin
      let start = !i + 2 in
      let j = ref start in
      while !j + 1 < len && not (text.[!j] = '*' && text.[!j+1] = '*') do incr j done;
      if !j + 1 < len then begin
        Buffer.add_string buf (bold (String.sub text start (!j - start)));
        i := !j + 2
      end else begin
        Buffer.add_char buf text.[!i]; incr i
      end
    end else if !i < len && text.[!i] = '*' && (!i = 0 || text.[!i-1] = ' ') then begin
      let start = !i + 1 in
      let j = ref start in
      while !j < len && text.[!j] <> '*' do incr j done;
      if !j < len && !j > start then begin
        Buffer.add_string buf (italic (String.sub text start (!j - start)));
        i := !j + 1
      end else begin
        Buffer.add_char buf text.[!i]; incr i
      end
    end else begin
      Buffer.add_char buf text.[!i]; incr i
    end
  done;
  Buffer.contents buf

(** Make file:line references into OSC 8 hyperlinks if terminal supports it. *)
let linkify_paths text =
  (* Simple regex-free approach: find patterns like /path/to/file.ext:123 *)
  let buf = Buffer.create (String.length text) in
  let i = ref 0 in
  let len = String.length text in
  while !i < len do
    if !i < len && text.[!i] = '/' then begin
      (* Scan for file:line pattern *)
      let j = ref !i in
      while !j < len && text.[!j] <> ' ' && text.[!j] <> '\n' && text.[!j] <> ')' && text.[!j] <> ',' do incr j done;
      let token = String.sub text !i (!j - !i) in
      (* Check if it looks like a file path *)
      if String.contains token '.' && String.length token > 3 then begin
        (* OSC 8 hyperlink *)
        Buffer.add_string buf (Printf.sprintf "\027]8;;file://%s\027\\%s\027]8;;\027\\" token (magenta token));
        i := !j
      end else begin
        Buffer.add_char buf text.[!i]; incr i
      end
    end else begin
      Buffer.add_char buf text.[!i]; incr i
    end
  done;
  Buffer.contents buf

(** Full markdown render — headers, code blocks, lists, inline formatting. *)
let render text =
  let lines = String.split_on_char '\n' text in
  let buf = Buffer.create (String.length text * 2) in
  let in_code = ref false in
  List.iter (fun line ->
    let trimmed = String.trim line in
    if String.length trimmed >= 3 && String.sub trimmed 0 3 = "```" then begin
      if !in_code then begin
        in_code := false;
        Buffer.add_string buf (dim "  ```");
        Buffer.add_char buf '\n'
      end else begin
        in_code := true;
        let lang = String.trim (String.sub trimmed 3 (String.length trimmed - 3)) in
        Buffer.add_string buf (dim (Printf.sprintf "  ```%s" lang));
        Buffer.add_char buf '\n'
      end
    end else if !in_code then begin
      Buffer.add_string buf (green (Printf.sprintf "  %s" line));
      Buffer.add_char buf '\n'
    end else if String.length trimmed > 0 && trimmed.[0] = '#' then begin
      let level = ref 0 in
      while !level < String.length trimmed && trimmed.[!level] = '#' do incr level done;
      let txt = String.trim (String.sub trimmed !level (String.length trimmed - !level)) in
      let styled = match !level with
        | 1 -> bold (yellow txt)
        | 2 -> bold txt
        | _ -> bold (dim txt)
      in
      Buffer.add_string buf (Printf.sprintf "\n  %s\n" styled)
    end else if String.length trimmed > 1 && (trimmed.[0] = '-' || trimmed.[0] = '*') && trimmed.[1] = ' ' then begin
      let item = String.sub trimmed 2 (String.length trimmed - 2) in
      Buffer.add_string buf (Printf.sprintf "  %s %s\n" (dim "\xe2\x80\xa2") (render_inline item))
    end else if String.length trimmed > 2 && trimmed.[0] >= '0' && trimmed.[0] <= '9' && trimmed.[1] = '.' then begin
      Buffer.add_string buf (Printf.sprintf "  %s\n" (render_inline trimmed))
    end else begin
      Buffer.add_string buf (Printf.sprintf "%s\n" (linkify_paths (render_inline line)))
    end
  ) lines;
  Buffer.contents buf

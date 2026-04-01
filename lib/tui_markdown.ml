(** Simple markdown-to-ANSI renderer.

    Handles: headers, bold, italic, code blocks, inline code, lists. *)

let bold s = Printf.sprintf "\027[1m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let italic s = Printf.sprintf "\027[3m%s\027[0m" s
let cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s

(** Render inline markdown formatting (bold, italic, code). *)
let render_inline text =
  let buf = Buffer.create (String.length text) in
  let i = ref 0 in
  let len = String.length text in
  while !i < len do
    if !i + 1 < len && text.[!i] = '`' then begin
      (* Inline code *)
      let start = !i + 1 in
      let j = ref start in
      while !j < len && text.[!j] <> '`' do incr j done;
      if !j < len then begin
        let code = String.sub text start (!j - start) in
        Buffer.add_string buf (cyan code);
        i := !j + 1
      end else begin
        Buffer.add_char buf text.[!i];
        incr i
      end
    end else if !i + 2 < len && text.[!i] = '*' && text.[!i+1] = '*' then begin
      (* Bold *)
      let start = !i + 2 in
      let j = ref start in
      while !j + 1 < len && not (text.[!j] = '*' && text.[!j+1] = '*') do incr j done;
      if !j + 1 < len then begin
        Buffer.add_string buf (bold (String.sub text start (!j - start)));
        i := !j + 2
      end else begin
        Buffer.add_char buf text.[!i];
        incr i
      end
    end else if !i < len && text.[!i] = '*' then begin
      (* Italic *)
      let start = !i + 1 in
      let j = ref start in
      while !j < len && text.[!j] <> '*' do incr j done;
      if !j < len then begin
        Buffer.add_string buf (italic (String.sub text start (!j - start)));
        i := !j + 1
      end else begin
        Buffer.add_char buf text.[!i];
        incr i
      end
    end else begin
      Buffer.add_char buf text.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(** Render a full markdown text to ANSI-styled string. *)
let render text =
  let lines = String.split_on_char '\n' text in
  let buf = Buffer.create (String.length text * 2) in
  let in_code_block = ref false in
  List.iter (fun line ->
    let trimmed = String.trim line in
    if String.length trimmed >= 3 && String.sub trimmed 0 3 = "```" then begin
      if !in_code_block then begin
        in_code_block := false;
        Buffer.add_string buf (dim "```");
        Buffer.add_char buf '\n'
      end else begin
        in_code_block := true;
        let lang = String.trim (String.sub trimmed 3 (String.length trimmed - 3)) in
        if lang <> "" then
          Buffer.add_string buf (dim (Printf.sprintf "```%s" lang))
        else
          Buffer.add_string buf (dim "```");
        Buffer.add_char buf '\n'
      end
    end else if !in_code_block then begin
      Buffer.add_string buf (green line);
      Buffer.add_char buf '\n'
    end else if String.length trimmed > 0 && trimmed.[0] = '#' then begin
      (* Header *)
      let level = ref 0 in
      while !level < String.length trimmed && trimmed.[!level] = '#' do incr level done;
      let header_text = String.trim (String.sub trimmed !level (String.length trimmed - !level)) in
      let styled = match !level with
        | 1 -> bold (yellow header_text)
        | 2 -> bold header_text
        | _ -> bold (dim header_text)
      in
      Buffer.add_string buf styled;
      Buffer.add_char buf '\n'
    end else if String.length trimmed > 1 && (trimmed.[0] = '-' || trimmed.[0] = '*')
                && trimmed.[1] = ' ' then begin
      (* List item *)
      let item = String.sub trimmed 2 (String.length trimmed - 2) in
      Buffer.add_string buf (Printf.sprintf "  %s %s" (dim "•") (render_inline item));
      Buffer.add_char buf '\n'
    end else begin
      Buffer.add_string buf (render_inline line);
      Buffer.add_char buf '\n'
    end
  ) lines;
  Buffer.contents buf

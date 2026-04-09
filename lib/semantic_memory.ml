(** Semantic memory — FNV-1a hashed token embeddings with cosine similarity recall. *)

(* ── Types ────────────────────────────────────────────────────── *)

type memory_entry = {
  id : string;
  content : string;
  embedding : (int * float) list;  (* sparse vector: (dim_index, value) *)
  confidence : float;
  created_at : float;
  last_accessed : float;
  access_count : int;
  tags : string list;
}

type memory_store = {
  entries : memory_entry list;
  version : int;
}

let empty_store = { entries = []; version = 1 }

let embedding_dims = 128
let decay_lambda = 0.001  (* half-life ~29 days *)
let dedup_threshold = 0.85
let compact_merge_threshold = 0.9
let compact_min_confidence = 0.05
let default_min_confidence = 0.1

(* ── FNV-1a hash ──────────────────────────────────────────────── *)

let fnv1a_hash s =
  let basis = 0x811c9dc5 in
  let prime = 0x01000193 in
  let h = ref basis in
  String.iter (fun c ->
    h := !h lxor (Char.code c);
    h := !h * prime;
    h := !h land 0x3fffffff  (* keep positive in OCaml's 63-bit int *)
  ) s;
  !h

(* ── Tokenizer ────────────────────────────────────────────────── *)

let stopwords = [
  "a"; "an"; "the"; "is"; "it"; "in"; "on"; "at"; "to"; "for";
  "of"; "and"; "or"; "but"; "not"; "with"; "this"; "that"; "was";
  "are"; "be"; "has"; "had"; "have"; "do"; "does"; "did"; "will";
  "would"; "could"; "should"; "may"; "might"; "can"; "i"; "you";
  "he"; "she"; "we"; "they"; "my"; "your"; "his"; "her"; "our";
  "their"; "me"; "him"; "us"; "them"; "so"; "if"; "then"; "than";
  "just"; "also"; "very"; "too"; "as"; "by"; "from"; "up"; "out";
]

let tokenize s =
  let buf = Buffer.create 64 in
  let tokens = ref [] in
  let flush () =
    if Buffer.length buf > 0 then begin
      let t = String.lowercase_ascii (Buffer.contents buf) in
      if String.length t > 1 && not (List.mem t stopwords) then
        tokens := t :: !tokens;
      Buffer.clear buf
    end
  in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
       || (c >= '0' && c <= '9') || c = '_' then
      Buffer.add_char buf c
    else
      flush ()
  ) s;
  flush ();
  List.rev !tokens

(* ── Embedding ────────────────────────────────────────────────── *)

let embed text =
  let tokens = tokenize text in
  let dims = Array.make embedding_dims 0.0 in
  List.iter (fun tok ->
    let idx = (fnv1a_hash tok) mod embedding_dims in
    dims.(idx) <- dims.(idx) +. 1.0
  ) tokens;
  (* L2 normalize *)
  let norm = Array.fold_left (fun acc v -> acc +. v *. v) 0.0 dims in
  let norm = sqrt norm in
  if norm > 0.0 then
    Array.iteri (fun i v -> dims.(i) <- v /. norm) dims;
  (* Sparse representation: only non-zero dims *)
  let sparse = ref [] in
  Array.iteri (fun i v ->
    if v <> 0.0 then sparse := (i, v) :: !sparse
  ) dims;
  List.rev !sparse

(* ── Cosine similarity ────────────────────────────────────────── *)

let cosine_similarity a b =
  (* Both are L2-normalized sparse vectors, so cosine = dot product *)
  List.fold_left (fun acc (i, v) ->
    match List.assoc_opt i b with
    | Some bv -> acc +. v *. bv
    | None -> acc
  ) 0.0 a

(* ── Confidence decay ─────────────────────────────────────────── *)

let decay_confidence entry ~now =
  let dt_hours = (now -. entry.last_accessed) /. 3600.0 in
  if dt_hours <= 0.0 then entry
  else
    let decayed = entry.confidence *. exp (-. decay_lambda *. dt_hours) in
    { entry with confidence = decayed }

(* ── UUID generation ──────────────────────────────────────────── *)

let generate_id () =
  let ic = Unix.open_process_in
    "uuidgen 2>/dev/null || cat /proc/sys/kernel/random/uuid 2>/dev/null || echo unknown" in
  let id = try String.trim (input_line ic) with _ -> "unknown" in
  ignore (Unix.close_process_in ic);
  String.lowercase_ascii id

(* ── Store / Recall ───────────────────────────────────────────── *)

let store mem ~content ?(tags = []) ?(confidence = 1.0) () =
  let emb = embed content in
  let now = Unix.gettimeofday () in
  (* Dedup: check if a very similar entry already exists *)
  let found = List.find_opt (fun e ->
    cosine_similarity emb e.embedding > dedup_threshold
  ) mem.entries in
  match found with
  | Some existing ->
    (* Merge: update content, boost confidence *)
    let updated = {
      existing with
      content;
      confidence = min 1.0 (existing.confidence +. 0.1);
      last_accessed = now;
      access_count = existing.access_count + 1;
      tags = List.sort_uniq String.compare (existing.tags @ tags);
    } in
    let entries = List.map (fun e ->
      if e.id = existing.id then updated else e
    ) mem.entries in
    { mem with entries }
  | None ->
    let entry = {
      id = generate_id ();
      content;
      embedding = emb;
      confidence;
      created_at = now;
      last_accessed = now;
      access_count = 0;
      tags;
    } in
    { mem with entries = mem.entries @ [entry] }

let recall mem ~query ~top_k ?(min_confidence = default_min_confidence) () =
  let q_emb = embed query in
  let now = Unix.gettimeofday () in
  (* Decay all entries, compute similarity, filter, sort, take top_k *)
  let scored = List.filter_map (fun e ->
    let e = decay_confidence e ~now in
    if e.confidence < min_confidence then None
    else
      let sim = cosine_similarity q_emb e.embedding in
      if sim > 0.01 then Some (sim *. e.confidence, e)
      else None
  ) mem.entries in
  let sorted = List.sort (fun (s1, _) (s2, _) -> compare s2 s1) scored in
  let top = List.filteri (fun i _ -> i < top_k) sorted in
  (* Update last_accessed on recalled entries *)
  let recalled_ids = List.map (fun (_, e) -> e.id) top in
  let updated_entries = List.map (fun e ->
    if List.mem e.id recalled_ids then
      { e with last_accessed = now; access_count = e.access_count + 1 }
    else e
  ) mem.entries in
  let _mem = { mem with entries = updated_entries } in
  List.map snd top

let compact mem =
  let now = Unix.gettimeofday () in
  (* Decay and remove low-confidence entries *)
  let entries = List.filter_map (fun e ->
    let e = decay_confidence e ~now in
    if e.confidence < compact_min_confidence then None
    else Some e
  ) mem.entries in
  (* Merge near-duplicates *)
  let rec merge_pass entries =
    match entries with
    | [] -> []
    | e :: rest ->
      let (dups, others) = List.partition (fun o ->
        o.id <> e.id && cosine_similarity e.embedding o.embedding > compact_merge_threshold
      ) rest in
      let merged = List.fold_left (fun acc dup ->
        { acc with
          confidence = min 1.0 (acc.confidence +. dup.confidence *. 0.5);
          access_count = acc.access_count + dup.access_count;
          tags = List.sort_uniq String.compare (acc.tags @ dup.tags);
        }
      ) e dups in
      merged :: merge_pass others
  in
  { mem with entries = merge_pass entries }

(* ── JSON serialization ───────────────────────────────────────── *)

let entry_to_json e =
  `Assoc [
    ("id", `String e.id);
    ("content", `String e.content);
    ("embedding", `List (List.map (fun (i, v) ->
      `List [`Int i; `Float v]) e.embedding));
    ("confidence", `Float e.confidence);
    ("created_at", `Float e.created_at);
    ("last_accessed", `Float e.last_accessed);
    ("access_count", `Int e.access_count);
    ("tags", `List (List.map (fun t -> `String t) e.tags));
  ]

let entry_of_json json =
  let open Yojson.Safe.Util in
  let embedding = json |> member "embedding" |> to_list |> List.map (fun pair ->
    match to_list pair with
    | [`Int i; `Float v] -> (i, v)
    | [`Int i; `Int v] -> (i, float_of_int v)
    | _ -> failwith "bad embedding pair"
  ) in
  {
    id = json |> member "id" |> to_string;
    content = json |> member "content" |> to_string;
    embedding;
    confidence = (try json |> member "confidence" |> to_float with _ -> 1.0);
    created_at = (try json |> member "created_at" |> to_float with _ -> 0.0);
    last_accessed = (try json |> member "last_accessed" |> to_float with _ -> 0.0);
    access_count = (try json |> member "access_count" |> to_int with _ -> 0);
    tags = (try json |> member "tags" |> to_list |> List.map to_string with _ -> []);
  }

let to_json mem =
  `Assoc [
    ("version", `Int mem.version);
    ("entries", `List (List.map entry_to_json mem.entries));
  ]

let of_json json =
  let open Yojson.Safe.Util in
  {
    version = (try json |> member "version" |> to_int with _ -> 1);
    entries = json |> member "entries" |> to_list |> List.map entry_of_json;
  }

(* ── Persistence ──────────────────────────────────────────────── *)

let memory_dir () =
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  Filename.concat (Filename.concat home ".camel") "memory"

let memory_path () =
  Filename.concat (memory_dir ()) "semantic.json"

let ensure_dir dir =
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)))

let load () =
  let path = memory_path () in
  if Sys.file_exists path then begin
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      of_json (Yojson.Safe.from_string content)
    with _ -> empty_store
  end else
    empty_store

let save mem =
  let dir = memory_dir () in
  ensure_dir dir;
  let path = memory_path () in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string (to_json mem));
  close_out oc

(* ── Display ──────────────────────────────────────────────────── *)

let entry_to_string e =
  let tags_str = match e.tags with
    | [] -> ""
    | ts -> Printf.sprintf " [%s]" (String.concat ", " ts) in
  Printf.sprintf "- %s (confidence: %.2f)%s" e.content e.confidence tags_str

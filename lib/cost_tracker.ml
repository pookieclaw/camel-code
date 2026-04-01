(** Token usage and cost tracking. *)

type cost_info = {
  input_cost_per_mtok : float;
  output_cost_per_mtok : float;
}

let model_costs = [
  "claude-sonnet-4-20250514", { input_cost_per_mtok = 3.0; output_cost_per_mtok = 15.0 };
  "claude-opus-4-20250514", { input_cost_per_mtok = 15.0; output_cost_per_mtok = 75.0 };
  "claude-haiku-4-5-20251001", { input_cost_per_mtok = 0.80; output_cost_per_mtok = 4.0 };
]

let default_cost = { input_cost_per_mtok = 3.0; output_cost_per_mtok = 15.0 }

let get_cost_info model =
  match List.assoc_opt model model_costs with
  | Some i -> i
  | None -> default_cost

type t = {
  mutable total_usage : Message.usage;
  mutable turn_count : int;
  model : string;
}

let create ~model =
  { total_usage = Message.empty_usage; turn_count = 0; model }

let add_turn t u =
  t.total_usage <- Message.add_usage t.total_usage u;
  t.turn_count <- t.turn_count + 1

let compute_cost t =
  let info = match List.assoc_opt t.model model_costs with
    | Some i -> i | None -> default_cost
  in
  let u = t.total_usage in
  let inp = Float.of_int u.input_tokens *. info.input_cost_per_mtok /. 1_000_000.0 in
  let out = Float.of_int u.output_tokens *. info.output_cost_per_mtok /. 1_000_000.0 in
  inp +. out

let summary t =
  let cost = compute_cost t in
  Printf.sprintf "Turns: %d | Tokens: %d in / %d out | Cost: $%.4f"
    t.turn_count t.total_usage.input_tokens t.total_usage.output_tokens cost

open DT1D

let line_seq =
  let open Seq in
  let rec loop ch () =
    let line_opt = try Some (input_line ch) with End_of_file -> None in
    match line_opt with None -> Nil | Some line -> Cons (line, loop ch)
  in
  loop

let learn input_path output_path min_n max_depth use_biniou =
  let input_ch = match input_path with None -> stdin | Some p -> open_in p in
  let lines = line_seq input_ch in
  let xy =
    Seq.map
      (fun line ->
        match String.split_on_char ' ' line with
        | [ x_s; y_s ] -> (float_of_string x_s, float_of_string y_s)
        | x ->
          failwith
            (Printf.sprintf "expecting two tokens per line, got %d"
               (List.length x)))
      lines
  in
  let xy = Array.of_seq xy in
  close_in input_ch;
  let t = Regression.learn ~min_n ~max_depth xy in
  let output_ch =
    match output_path with None -> stdout | Some p -> open_out p
  in
  let serialize = if use_biniou then Codec.biniou_of_t else Codec.json_of_t in
  let s = serialize t in
  output_string output_ch s;
  close_out output_ch

let infer input_path output_path model_path model_uses_biniou =
  let model_s =
    match Bos.OS.File.read (Fpath.v model_path) with
    | Ok contents -> contents
    | Error (`Msg msg) -> failwith msg
  in
  let deserialize =
    if model_uses_biniou then Codec.t_of_biniou else Codec.t_of_json
  in
  let model = deserialize model_s in
  let infer x = Regression.infer x model in
  let input_ch = match input_path with None -> stdin | Some p -> open_in p in
  let output_ch =
    match output_path with None -> stdout | Some p -> open_out p
  in
  let lines = line_seq input_ch in
  Seq.iter
    (fun line ->
      match String.split_on_char ' ' line with
      | x_s :: rest ->
        let x = float_of_string x_s in
        let y_hat = infer x in
        Printf.fprintf output_ch "%s " x_s;
        List.iter (Printf.fprintf output_ch "%s ") rest;
        Printf.fprintf output_ch "%+.4e\n" y_hat
      | [] -> ())
    lines;
  close_in input_ch

open Cmdliner

let _ =
  let doc = "train and evaluate 1D regression tree models" in

  let train_cmd =
    let doc = "train a regression tree model." in
    let input_path =
      let doc =
        "a two-column data file where each column contains a numbers and where \
         the two columns are separated by a space. (absent: stdin)"
      in
      Arg.(
        value & opt (some file) None & info [ "i"; "input" ] ~docv:"PATH" ~doc)
    in
    let output_path =
      let doc = "path of output model file (absent: stdout)" in
      Arg.(
        value
        & opt (some string) None
        & info [ "o"; "output"; "model" ] ~docv:"PATH" ~doc)
    in
    let min_n =
      let doc = "the minimum number of observations necessary for splitting" in
      Arg.(
        required & opt (some int) None & info [ "n"; "min-n" ] ~docv:"INT" ~doc)
    in
    let max_depth =
      let doc = "the maximum depth of the decision tree" in
      Arg.(
        required
        & opt (some int) None
        & info [ "d"; "max-depth" ] ~docv:"INT" ~doc)
    in
    let use_biniou =
      let doc =
        "in serializing the model file, use the Biniou serialization format \
         instead of JSON"
      in
      Arg.(value & flag & info [ "b"; "biniou" ] ~doc)
    in
    Cmd.v (Cmd.info "train" ~doc)
      Term.(
        const learn $ input_path $ output_path $ min_n $ max_depth $ use_biniou)
  in

  let infer_cmd =
    let doc = "evaluate a regression tree model over inputs." in
    let input_path =
      let doc =
        "a columnar data file where each column is speparated by a space. The \
         first column is interpreted as the function input. All input columns \
         are echoed in the output. (absent: stdin)"
      in
      Arg.(
        value & opt (some file) None & info [ "i"; "input" ] ~docv:"PATH" ~doc)
    in
    let model_path =
      let doc = "path of the model file" in
      Arg.(
        required
        & opt (some file) None
        & info [ "m"; "model" ] ~docv:"PATH" ~doc)
    in
    let model_uses_biniou =
      let doc = "the model file is serialized using Biniou (absent: JSON)" in
      Arg.(value & flag & info [ "b"; "biniou" ] ~doc)
    in

    let output_path =
      let doc = "path of the output file (absent: stdout)" in
      Arg.(
        value
        & opt (some string) None
        & info [ "o"; "output" ] ~docv:"PATH" ~doc)
    in
    Cmd.v (Cmd.info "infer" ~doc)
      Term.(
        const infer $ input_path $ output_path $ model_path $ model_uses_biniou)
  in

  let main_cmd = Cmd.group (Cmd.info "dt1d" ~doc) [ train_cmd; infer_cmd ] in
  Cmd.eval ~catch:false main_cmd

open DT1D

let line_seq =
  let open Seq in
  let rec loop ch () =
    let line_opt =
      try
        Some (input_line ch)
      with End_of_file ->
        None
    in
    match line_opt with
    | None -> Nil
    | Some line -> Cons (line, loop ch)
  in
  loop

let learn input_path output_path min_n max_depth =
  let input_ch =
    match input_path with
    | None -> stdin
    | Some p -> open_in p
  in
  let lines = line_seq input_ch in
  let xy = Seq.map (
    fun line ->
      match String.split_on_char ' ' line with
      | [x_s; y_s] ->
        float_of_string x_s, float_of_string y_s
      | x ->
        failwith (Printf.sprintf "expecting two tokens per line, got %d" (List.length x))
  ) lines in
  let xy = Array.of_seq xy in
  close_in input_ch;
  let t = Regression.learn ~min_n ~max_depth xy in
  let output_ch =
    match output_path with
    | None -> stdout
    | Some p -> open_out p
  in
  let j = Regression.json_of_t t in
  output_string output_ch j;
  close_out output_ch

let infer input_path output_path model_path =
  let model_s =
    match Bos.OS.File.read (Fpath.v model_path) with
    | Ok contents -> contents
    | Error (`Msg msg) -> failwith msg
  in
  let model = Regression.t_of_json model_s in
  let infer x = Regression.infer x model in
  let input_ch =
    match input_path with
    | None -> stdin
    | Some p -> open_in p
  in
  let output_ch =
    match output_path with
    | None -> stdout
    | Some p -> open_out p
  in
  let lines = line_seq input_ch in
  Seq.iter (
    fun line ->
      match String.split_on_char ' ' line with
      | x_s :: rest ->
        let x = float_of_string x_s in
        let y_hat = infer x in
        Printf.fprintf output_ch "%s " x_s;
        List.iter (Printf.fprintf output_ch "%s ") rest;
        Printf.fprintf output_ch "%+.4e\n" y_hat

      | [] -> ()
  ) lines;
  close_in input_ch


open Cmdliner

let _ =
  let main_cmd =
    let doc = "train and evaluate 1D regression tree models" in
    Term.(pure ()), Term.info "dt1d" ~doc
  in

  let train_cmd =
    let doc = "train a regression tree model." in
    let input_path =
      let doc = "a two-column data file where each column contains a \
                 numbers and where the two columns are separated by a \
                 space. (absent: stdin)" in
      Arg.(value & opt (some file) None & info ["i";"input"] ~docv:"PATH" ~doc)
    in
    let output_path =
      let doc = "path of output model file (absent: stdout)" in
      Arg.(value & opt (some string) None &
           info ["o";"output";"model"] ~docv:"PATH" ~doc)
    in
    let min_n =
      let doc = "the minimum number of observations necessary for splitting" in
      Arg.(required & opt (some int) None & info ["n";"min-n"] ~docv:"INT" ~doc)
    in
    let max_depth =
      let doc = "the maximum depth of the decision tree" in
      Arg.(required & opt (some int) None & info ["d";"max-depth"] ~docv:"INT" ~doc)
    in
    Term.(pure learn $ input_path $ output_path $ min_n $ max_depth),
    Term.info "train" ~doc
  in

  let infer_cmd =
    let doc = "evaluate a regression tree model over inputs." in
    let input_path =
      let doc = "a columnar data file where each column is speparated \
                 by a space. The first column is interpreted as the \
                 function input. All input columns are echoed in the \
                 output. (absent: stdin)" in
      Arg.(value & opt (some file) None & info ["i";"input"] ~docv:"PATH" ~doc)
    in
    let model_path =
      let doc = "path of the model file" in
      Arg.(required & opt (some file) None & info ["m";"model"] ~docv:"PATH" ~doc)
    in

    let output_path =
      let doc = "path of the output file (absent: stdout)" in
      Arg.(value & opt (some string) None & info ["o";"output"] ~docv:"PATH" ~doc)
    in
    Term.(pure infer $ input_path $ output_path $ model_path ),
    Term.info "infer" ~doc
  in

  let commands = [train_cmd; infer_cmd] in
  match Term.eval_choice ~catch:false main_cmd commands with
  | `Error _ -> exit 1
  | _ -> exit 0


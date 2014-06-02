{shared{

let (>>=) = Lwt.bind

open Eliom_content
open Html5.D

type diff = (int  * string) array
    deriving(Json)

type request = {client : int; from_revision : int; diffs : (int * string) list}
    deriving(Json)

type bus_message =
  | Patch of (int * diff * int)
  | Hello of int
        deriving(Json)

}}
{server{

type patch_result = Failure of string
                  | Success of string


type response =
  | Applied of int
  | Rejected of (int * string) array list


type revision = {id : int; text : string }


let init_collaborative_editor ~name =
  let patches_bus = Eliom_bus.create
      ~scope:Eliom_common.site_scope Json.t<bus_message>
  in


let insert_at text str id =
  try
    begin
      let beginning = Str.string_before text id in
      let ending = Str.string_after text id in
      Success (beginning ^ str ^ ending)
    end
  with
  | _ -> Failure "Error while inserting text"


let apply_deletion text str i =
  try
    begin
      let current_chunk = Str.string_after !text i in
      let to_delete = Str.string_before current_chunk (String.length str) in
      if to_delete = str then
        begin
          text := (Str.string_before !text i) ^ (Str.string_after !text (i + (String.length str)));
          true
        end
      else
        false
    end
  with
  | Invalid_argument _ -> false


let apply_addition text str i =
  match insert_at !text str i with
  | Success str -> text := str; true
  | Failure _ -> false


let check_coherence i s text =
  try
    let start = Str.string_before text (i + (String.length s)) in
    let chunk = Str.string_after start i in
    chunk = s
  with
  | Invalid_argument _ -> false


let apply_diffs text diffs =
  let rtext = ref text in
  let rec inner diffs i =
    match diffs with
    | [] -> Success !rtext
    | (-1, str)::xs ->
      if apply_deletion rtext str i then
        inner xs i
      else
          Failure "Impossible to delete chunk"
    | (0, str)::xs -> if check_coherence i str !rtext then inner xs (i + (String.length str))
      else Failure "Retain don't match"
    | (1, str)::xs ->
      if apply_addition rtext str i then
        inner xs (i + (String.length str))
      else
        Failure "Impossible to patch chunk"
    | _ -> Failure "Unknown patch operation"
  in inner diffs 0


let (append_shadowcopies, get_shadowcopies) =
  let default_value = [{id = 0; text = ""}] in
  let eref = Eliom_reference.eref ~scope:Eliom_common.site_scope default_value in
  let get = Eliom_reference.get in
  ((fun elm -> get eref
     >>= fun shdwcopies -> Eliom_reference.set eref (elm::shdwcopies)),
  (fun () -> get eref))


let handle_patch_request (request : request) =
  let verify_patch cscopy oscopies =
    let cid, ctext = cscopy.id, cscopy.text in
    let rid, rdiffs, ruid = request.from_revision, request.diffs, request.client in
    match apply_diffs ctext request.diffs with
    | Failure s -> Lwt.return (`Refused (cid, ctext))
    | Success ntext -> if rid = cid then
        begin
          let ncopy = { id = cid + 1;
                        text = ntext; } in
          ignore(append_shadowcopies ncopy);
          ignore(Eliom_bus.write patches_bus (Patch (ruid, (Array.of_list rdiffs), (cid + 1))));
          Lwt.return (`Applied (cid + 1, ntext))
        end
      else begin Lwt.return (`Refused (cid, ctext)) end
  in
  get_shadowcopies ()
  >>= fun scopies ->
  match scopies with
  | [] -> Lwt.return (`Refused (0, ""))
  | x::xs -> verify_patch x xs


let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()


let get_document =
  Eliom_service.Ocaml.coservice'
    ~rt:(Eliom_service.rt : [`Result of (string * int) | `NotConnected] Eliom_service.rt)
    ~get_params: (Eliom_parameter.string "document")
    ()

let send_patch =
  Eliom_service.Ocaml.post_coservice'
    ~rt:(Eliom_service.rt :
           [`Applied of int * string | `Refused of int * string]
             Eliom_service.rt)
    ~post_params: (Eliom_parameter.ocaml "lol" Json.t<request>)
    ()

}}

{client{

module Html = Dom_html
let (>>=) = Lwt.bind
open Dom


type phase =
  | Init of (int * diff * int) list
  | Ok of (int * diff * int) list
  | Disconnected


let load_document editor old rev =
  Eliom_client.call_ocaml_service ~service:%get_document "toto" ()
  >>= fun response ->
  begin
    match response with
    | `Result (document, id) ->
      editor##innerHTML <- (Js.string document);
      old := (Js.string document);
      rev := id; Lwt.return_unit
    | `NotConnected -> Lwt.return_unit
  end


let make_diff text old_text rev client_id =
  let dmp = DiffMatchPatch.make () in
  let diff = DiffMatchPatch.diff_main dmp old_text text in
  {from_revision = rev; diffs = (Array.to_list diff); client = client_id;}


let get_editor _ = Js.Opt.get (Html.document##getElementById
                                 (Js.string "editor")) (fun () -> assert false)

let apply_patches rev editor shadow_copy patches =
  List.iter (fun (id, diff, prev) ->
      if prev = !rev then
        print_endline "lol";
        let dmp = DiffMatchPatch.make () in
        let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
        let patch_editor = DiffMatchPatch.patch_make dmp (Js.to_string editor##innerHTML) diff in
        editor##innerHTML <- Js.string @@ DiffMatchPatch.patch_apply
            dmp patch_editor (Js.to_string editor##innerHTML);
        shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
            dmp patch_scopy (Js.to_string !shadow_copy);
        rev := prev) (List.rev patches)


let onload _ =
  Random.self_init ();

  (* Is the current revision server-side *)
  let shadow_copy = ref (Js.string "") in
  (* Is the revision number of this client *)
  let rev = ref 0 in
  (* this client id *)
  let client_id = Random.int 4096 in

  let phase = ref (Init []) in
  let is_ok _ = match !phase with
    | Ok _ -> true
    | _ -> false in
  let d = Html.document in

  let body =
    Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  let editor = Html.createDiv d in

  Dom.appendChild body editor;
  (* get document content *)


  Lwt.async (fun _ -> Lwt_stream.iter
  (function
    | Hello id -> (* First, check if the bus is running
                     by checking our own Hello message *)
      if id = client_id then
        begin
          match !phase with (* if its ours, then, go into the Ok phase
                               and start loading the document *)
          | Init msg_buffer -> begin
              load_document editor shadow_copy rev;
              phase := Ok [] end
          | _ -> ()
        end
      else ()
    | Patch (id, diff, prev) when prev = (!rev + 1) ->
      begin
        try
          begin
        if id != client_id && is_ok () then
          let editor = get_editor () in
          let dmp = DiffMatchPatch.make () in
          let patch_scopy = DiffMatchPatch.patch_make dmp
              (Js.to_string !shadow_copy) diff in
          let patch_editor = DiffMatchPatch.patch_make dmp
              (Js.to_string editor##innerHTML) diff in
          editor##innerHTML <- Js.string @@ DiffMatchPatch.patch_apply
              dmp patch_editor (Js.to_string editor##innerHTML);
          shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
              dmp patch_scopy (Js.to_string !shadow_copy);
          rev := prev
        else if id != client_id then
          begin
            match !phase with
            | Init l -> phase := Init ((id, diff, prev)::l)
            | _ -> ()
          end
        else
          ()
          end
        with
        | _ -> print_diffs diff; Eliom_lib.debug "EXN ! id = %d\nprev = %d\n current_rev = %d" id prev !rev
      end
    | _ -> ()
  )
  (Eliom_bus.stream %patches_bus));
  Eliom_bus.write %patches_bus (Hello (client_id));

  (* changes handler *)
  Lwt_js_events.(
    async
    (fun () ->
        inputs Dom_html.document
          (fun ev _ ->
             Lwt_js.sleep 0.3 >>= fun () ->
             let editor = get_editor () in
             let diff = make_diff (Js.to_string editor##innerHTML)
                 (Js.to_string !shadow_copy) !rev client_id in
             Eliom_client.call_ocaml_service ~service:%send_patch () diff
             >>= fun response ->
             begin
               match response with
               | `Applied (srev, scopy) -> rev := srev;
                 shadow_copy := (Js.string scopy); Lwt.return_unit
               | `Refused (srev, scopy) -> Lwt.return ()
             end
          )))

}}

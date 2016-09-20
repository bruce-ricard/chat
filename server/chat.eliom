[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D

    type number_users_connected = Zero | One | Two
    let number_users_connected = ref Zero

    let incr n = match !n with
        Zero -> n := One; true
      | One -> n := Two; true
      | Two -> false

    type user = U1 | U2
]

let user_one_messages, send_message_user_one = React.E.create ()
let user_two_messages, send_message_user_two = React.E.create ()
let user_one_acks, send_ack_user_one = React.E.create ()
let user_two_acks, send_ack_user_two = React.E.create ()

let current_user =
    Eliom_reference.eref ~scope:Eliom_common.default_process_scope (None : user option)

module Chat_app =
  Eliom_registration.App (
    struct
      let application_name = "chat"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let chat_service =
  Eliom_service.App.post_coservice'
    ~post_params:Eliom_parameter.(string "new_message")
    ()

let ack_service =
  Eliom_service.App.post_coservice'
    ~post_params:Eliom_parameter.(string "message")
    ()

let%client sent_message_ts = ref None

let chat_logs message acks =
  let logs =
    ul [
      ] in
  let _ = [%client (
                let dom_logs = Eliom_content.Html5.To_dom.of_element ~%logs in
                let update_with_message = React.E.map (fun s -> dom_logs##.innerHTML := Js.string (Js.to_string (dom_logs##.innerHTML) ^ "<li> them: " ^ s ^ "</li>");
                                                                Eliom_client.call_service ~service:~%ack_service () s
                                                      )
                                                      ~%message in
                let update_with_ack = React.E.map (fun s ->
                                          match !sent_message_ts with
                                          | None -> failwith "impossible"
                                          | Some ts ->
                                             let elapsed_time_ms = int_of_float (1000. *. (Unix.gettimeofday () -. ts)) in
                                             dom_logs##.innerHTML :=
                                               Js.string (Js.to_string (dom_logs##.innerHTML) ^ (Printf.sprintf "<li> me: %s (%d ms) </li>" s elapsed_time_ms)))
                                                  ~%acks in
                () : unit
          )] in
  logs

let chat_input () =
  let submit_button = Form.input ~input_type:`Submit ~value:"Send" Form.string in
  let form = Form.post_form
    ~service:chat_service
    (
      fun new_message ->
      let input_text_field = Form.input ~input_type:`Text ~name:new_message Form.string in
      [
        input_text_field;
        submit_button
      ]
    )
    ()
  in
  let _ = [%client
              (
                match Eliom_content.Html5.Manip.nth ~%form 1 with
                  None -> ()
                | Some node ->
                   begin
                     let dom_text = Eliom_content.Html5.To_dom.of_element node in
                     let dom_button = Eliom_content.Html5.To_dom.of_element ~%submit_button in
                     Lwt.async (fun () ->
                         Lwt_js_events.clicks
                           dom_button
                           (fun _ _ ->
                             sent_message_ts := Some (Unix.gettimeofday ());
                             (*dom_text##value := Js.string ""; doesn't work, no method value :( *)
                             Lwt.return ()
                           )
                       );
                     ()
                   end
                   : unit
              )
          ] in
  form

let chat_box message acks =
  div [
      chat_logs message acks;
      chat_input ()
    ]

let user_to_string = function
    U1 -> "user1" | U2 -> "user2"

let main_page user message acks =
  div [
      pcdata (Printf.sprintf "Welcome %s" (user_to_string user));
      chat_box message acks
    ]

let set_current_user () =
  ignore (
      match !number_users_connected with
        One -> Eliom_reference.set current_user (Some U1)
      | Two -> Eliom_reference.set current_user (Some U2)
      | Zero -> failwith "impossible 1"
    )

let get_user_page () =
  let%lwt user = Eliom_reference.get current_user in
  match user with
  | None -> Lwt.return (failwith "impossible 2")
  | Some U1 -> Lwt.return (main_page
                             U1
                             (Eliom_react.Down.of_react user_one_messages)
                             (Eliom_react.Down.of_react user_one_acks))
  | Some U2 -> Lwt.return (main_page
                             U2
                             (Eliom_react.Down.of_react user_two_messages)
                             (Eliom_react.Down.of_react user_two_acks))

let () =
  Chat_app.register
    ~service:main_service
    (fun () () ->
      match incr number_users_connected with
      | false -> Lwt.return
                   (Eliom_tools.F.html
                      ~title:"chat"
                      ~css:[["css";"chat.css"]]
                      Html5.F.(body [(div [pcdata "too many users"])]))
      | true ->
          set_current_user ();
          let%lwt page = get_user_page () in
          Lwt.return
            (Eliom_tools.F.html
               ~title:"chat"
               ~css:[["css";"chat.css"]]
               Html5.F.(body [
                            page
    ])));

  Eliom_registration.Action.register
    ~service:chat_service
    ~options:`NoReload
    (fun () new_message ->
          if !number_users_connected = Two then
            let%lwt user = Eliom_reference.get current_user in
            Lwt.return (
            match user with
              None -> failwith "impossible"
            | Some U1 -> send_message_user_two new_message
            | Some U2 -> send_message_user_one new_message
              )
          else
            Lwt.return ()
    );

    Eliom_registration.Action.register
    ~service:ack_service
    ~options:`NoReload
    (fun () new_message ->
      let%lwt user =  Eliom_reference.get current_user in
      Lwt.return (
          match user with
            None -> failwith "impossible"
          | Some U1 -> send_ack_user_two new_message
          | Some U2 -> send_ack_user_one new_message
        )
    )

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
    ~post_params:Eliom_parameter.(int "id" ** string "new_message")
    ()

let ack_service =
  Eliom_service.App.post_coservice'
    ~post_params:Eliom_parameter.(int "id" ** bool "ack")
    ()

let chat_logs_elt =
  ul [
    ]

let%client insert_their_message_in_chat_logs message =
  let new_chat_line = li [pcdata (Printf.sprintf "them: %s" message)] in
  Eliom_content.Html5.Manip.appendChild ~%chat_logs_elt new_chat_line

let%client messages_unacked : (int, bool -> unit) Hashtbl.t = Hashtbl.create 3

let chat_logs message acks =
  let _ = [%client (
                let update_with_message = React.E.map (fun (id,msg) -> insert_their_message_in_chat_logs msg;
                                                                       Eliom_client.call_service ~service:~%ack_service () (id,true)
                                                      )
                                                      ~%message in
                let update_with_ack = React.E.map
                                        (fun (id,ack) ->
                                          try
                                            (Hashtbl.find messages_unacked id) ack;
                                            Hashtbl.remove messages_unacked id
                                          with
                                            Not_found -> failwith "impossible"
                                        )
                                        ~%acks in
                () : unit
          )] in
  chat_logs_elt

let%client new_message_id =
  let id = ref 0 in
  function () -> Pervasives.incr id; !id

let%client ack_message message start_ts dom ack =
  if ack then
    let elapsed_time_ms = int_of_float (1000. *. (Unix.gettimeofday () -. start_ts)) in
    dom##.innerHTML := Js.string (Printf.sprintf "me: %s (%d ms)" message elapsed_time_ms)
  else
    dom##.innerHTML := Js.string (Printf.sprintf "me: %s (couldn't be sent)" message)

let chat_input () =
  let submit_button = Form.input ~input_type:`Submit ~value:"Send" Form.string in
  let input_text_field = Form.input ~input_type:`Text  Form.string in
  let form =
    div [
        input_text_field;
        submit_button
      ]
  in
  let _ = [%client
              (
                let dom_text = Eliom_content.Html5.To_dom.of_input ~%input_text_field in
                let dom_button = Eliom_content.Html5.To_dom.of_element ~%submit_button in
                Lwt.async (fun () ->
                    Lwt_js_events.clicks
                      dom_button
                      (fun _ _ ->
                        let message = Js.to_string dom_text##.value in
                        let message_id = new_message_id () in
                        let new_chat_line = li [pcdata (Printf.sprintf "me: %s (sending...)" message)] in
                        Eliom_content.Html5.Manip.appendChild ~%chat_logs_elt new_chat_line;
                        let new_chat_dom = Eliom_content.Html5.To_dom.of_element new_chat_line in
                        Hashtbl.add messages_unacked message_id (ack_message message (Unix.gettimeofday ()) new_chat_dom);
                        Eliom_client.call_service ~service:~%chat_service () (message_id, message);
                        dom_text##.value := Js.string "";
                        (* TODO: create a thread that sleeps 5 seconds and updates the table if no ack was recieved *)
                        (* For some reason importing lwt.unix makes my server not work anymore *)
                        Lwt.return ()
                      )
                  );
                ()
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

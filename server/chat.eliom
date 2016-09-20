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

let () =
  send_message_user_one "blah";
  send_message_user_two "blah";
  send_ack_user_one "msg";
  send_ack_user_two "msg"

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

let chat_logs message acks =
  let logs =
    ul [
      li [(pcdata "chat")];
      li [(pcdata "box")]
      ] in
  let _ = [%client (
                let dom_logs = Eliom_content.Html5.To_dom.of_element ~%logs in
                let update_with_message = React.E.map (fun s -> dom_logs##.innerHTML := Js.string (Js.to_string (dom_logs##.innerHTML) ^ "<li> them: " ^ s ^ "</li>");
                                                                Eliom_client.call_service ~service:~%ack_service () s
                                                      )
                                                      ~%message in
                let update_with_ack = React.E.map (fun s ->  dom_logs##.innerHTML := Js.string (Js.to_string (dom_logs##.innerHTML) ^ "<li> me: " ^ s ^ " (received) </li>")) ~%acks in
                () : unit
          )] in
  logs

let chat_input () =
  Form.post_form
    ~service:chat_service
    (
      fun new_message ->
      [
        Form.input ~input_type:`Text ~name:new_message Form.string;
        Form.input ~input_type:`Submit ~value:"Send" Form.string
      ]
    )
    ()

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
            | Some U1 -> (print_endline (Printf.sprintf "sending %s to user2 from 1" new_message); send_message_user_two new_message)
            | Some U2 -> (print_endline (Printf.sprintf "sending %s to user1 from 2" new_message); send_message_user_one new_message);
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

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
]

let user_one_messages, send_message_user_one = React.E.create ()
let user_two_messages, send_message_user_two = React.E.create ()
let user_one_acks, send_ack_user_one = React.E.create ()
let user_two_acks, send_ack_user_two = React.E.create ()

let () =
  send_message_user_one "blah";
  send_message_user_two "blah";
  send_ack_user_one false;
  send_ack_user_two true

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

let chat_logs () =
  ul [
      li [(pcdata "chat")];
      li [(pcdata "box")]
    ]

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

let chat_box () =
  div [
      chat_logs ();
      chat_input ()
    ]

let main_page () =
  chat_box ()


let () =
  Chat_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"chat"
           ~css:[["css";"chat.css"]]
           Html5.F.(body [
                        match incr number_users_connected with
                          false -> pcdata "too many users"
                        | true -> main_page ()
    ])));

  Chat_app.register
    ~service:chat_service
    (fun () new_message ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"chat"
           ~css:[["css";"chat.css"]]
           Html5.F.(body [
                        main_page ()
                      ]
         ))
    )

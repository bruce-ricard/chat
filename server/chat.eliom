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

    type chat_message = string [@@deriving json]
    type ack_message = bool [@@deriving json]
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

let main_page () =
  pcdata "kikoo"


let () =
  Chat_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"chat"
           ~css:[["css";"chat.css"]]
           Html5.F.(body [
                        main_page ()
           ])))

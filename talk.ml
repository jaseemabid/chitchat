open Core.Std
open Async.Std

(* Wait for messages from the network, write the message to console, reply OK *)
let rec receiver buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> return ()
  | `Ok len ->
     (* let _ = Writer.write w "OK\n" in *)
     let _ = Writer.flushed w in
     let _ = print_endline (" ✉ " ^ (String.sub buffer 0 len)) in
     receiver buffer r w

(* Wait for messages from the stdin, send via the pipe*)
let rec sender buffer peer =
  let reader = Reader.create (Unix.Fd.stdin ()) in
  Reader.read reader buffer
  >>= function
  | `Eof -> return ()
  | `Ok len ->
     let _ = Writer.write peer buffer ~len in
     let _ = Writer.flushed peer in
     sender buffer peer

(* Callback function for new TCP connections from clients *)
let callback _addr r w =
  print_endline "Client connected";
  let sender_buffer = String.create (16 * 1024) in
  let receiver_buffer = String.create (16 * 1024) in
  let _ = sender sender_buffer w in
  receiver receiver_buffer r w

(** Starts a TCP server, which listens on the specified port, invoking callback
  every time a client connects. *)
let server port () =
  print_endline "Starting up the server";
  let where_to_listen = (Tcp.on_port port) in
  ignore  (Tcp.Server.create ~on_handler_error:`Raise where_to_listen callback);
  never ()

let client port () =
  print_endline "Starting up the client";
  let target = (Tcp.to_host_and_port "127.0.0.1" port) in
  ignore (Tcp.with_connection target callback);
  never ()

(* Unable to use the stdlib variant due to some build issue *)
let default x opt =
  match opt with
      | Some v -> v
      | None -> x

(* Command line argument handling *)
let command =
  Command.basic
    ~summary:"Chit chat!"
    ~readme:(fun () -> "Demo OCaml app")
    Command.Spec.(empty
                  +> flag "-s" no_arg ~doc:"Run as server"
                  +> flag "-p" (optional int) ~doc:"Port to bind/connect server")
    (fun s p () ->
     match s with
     | true -> let _ = server (default 8765 p) () in ()
     | false -> let _ = client (default 8765 p) () in ())

(* Call command and then start the scheduler *)
let () =
  Command.run command;
  never_returns (Scheduler.go ())

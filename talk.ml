open Core.Std
open Async.Std

(* Wait for messages from the network, write the message to console, reply OK *)
let rec receiver buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> return ()
  | `Ok len ->
     let _ = Writer.write w "OK\n" in
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

(* Callback function for new TCP client connections *)
let callback _addr r w =
  print_endline "Client connected";
  let sender_buffer = String.create (16 * 1024) in
  let receiver_buffer = String.create (16 * 1024) in
  let _ = sender sender_buffer w in
  receiver receiver_buffer r w

(** Starts a TCP server, which listens on the specified port, invoking callback
  every time a client connects. *)
let server () =
  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise (Tcp.on_port 8765) callback
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

(* Call [run], and then start the scheduler *)
let () =
  server ();
  never_returns (Scheduler.go ())

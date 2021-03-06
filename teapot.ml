open Batteries
open BatLog
open Uv

let loop = Loop.default ()
let server = Tcp.make loop

let on_allocation handle suggested_size buf =
  logf "allocation\n%!";
  Buf.allocate buf suggested_size

let on_write req status =
  try
    logf "write\n%!";
    handle_error status;
    ()
  with
  | _ ->
     logf "write error!\n%!"

let on_read stream nread buf =
  try
    logf "nread: %d\n%!" (Signed.Long.to_int nread);
    logf "stream handle type: %d\n%!" (Stream.to_handle stream |> Handle.get_type);
    if Stream.is_writable stream then
      logf "writable!\n%!";
    let req = Write.alloc () in
    Write.try_write' req stream buf on_write;
    ()
  with
  | _ ->
     logf "read error!\n%!"

let on_close handle =
  logf "close\n%!";
  ()

let on_new_connection stream status =
  logf "new connection\n%!";
  handle_error status;
  let client = Tcp.make loop in
  logf "client handle type: %d\n%!" (Tcp.to_handle client |> Handle.get_type);
  try
    let server_stream = Tcp.to_stream server in
    let client_stream = Tcp.to_stream client in
    Stream.accept server_stream client_stream;
    Stream.read_start client_stream on_allocation on_read
  with
  | _ ->
     let client_handle = Tcp.to_handle client in
     Handle.close client_handle on_close

let run () =
  logf "Init\n%!";
  Tcp.init loop server;
  let bind_addr = Addr.ip4 "127.0.0.1" 8000 in
  Tcp.bind server bind_addr Tcp.Flags.none;
  let server_stream = Tcp.to_stream server in
  logf "Listen\n%!";
  Stream.listen server_stream 128 on_new_connection;
  logf "Starting\n%!";
  Loop.run loop RunMode.default

let () = run ()

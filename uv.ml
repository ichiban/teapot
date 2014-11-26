open Ctypes
open PosixTypes
open Foreign

let ssize_t = long

exception Error of string * string
let err_name = int @-> returning string |> foreign "uv_err_name"
let strerror = int @-> returning string |> foreign "uv_strerror"
let handle_error errno =
  if errno >= 0 then
    ()
  else
    let name = err_name errno in
    let message = strerror errno in
    raise (Error (name, message))

module RunMode = struct
  type t = int
  let t = int
  let default : t = 0
  let once : t = 1
  let nowait : t = 2
end

module Loop = struct
  type t
  let t : t abstract typ =
    abstract
      ~name:"uv_loop_t"
      ~size:0
      ~alignment:0
  let t' = ptr t
  let default = void @-> returning t' |> foreign "uv_default_loop"
  let run loop run_mode =
    let native = t' @-> RunMode.t @-> returning int |> foreign "uv_run" in
    native loop run_mode |> handle_error
end

module HandleType = struct
  type t = int
  let t = int
  let unknown_handle : t = 0
  let async : t = 1
  let check : t = 2
  let fs_event : t = 3
  let fs_poll : t = 4
  let handle : t = 5
  let idle : t = 6
  let pipe : t = 7
  let poll : t = 8
  let prepare : t = 9
  let process : t = 10
  let stream : t = 11
  let tcp : t = 12
  let timer : t = 13
  let tty : t = 14
  let udp : t = 15
  let signal : t = 16
  let file : t = 17
  let size = t @-> returning size_t |> foreign "uv_handle_size"
end

module Buf = struct
  type t
  let t : t abstract typ =
    abstract
      ~name:"uv_buf_t"
      ~size:0
      ~alignment:0
  let t' = ptr t
  let get_base = t' @-> returning (ptr char) |> foreign "uv_buf_get_base"
  let set_base = t' @-> ptr char @-> returning void |> foreign "uv_buf_set_base"
  let get_len = t' @-> returning size_t |> foreign "uv_buf_get_len"
  let set_len = t' @-> size_t @-> returning void |> foreign "uv_buf_set_len"
  let allocate buf suggested_size =
    let size = Unsigned.Size_t.to_int suggested_size in
    let base = Ctypes.allocate_n char ~count:size in
    set_base buf base;
    set_len buf suggested_size
end

module Handle = struct
  type t
  let t : t abstract typ =
    abstract
      ~name:"uv_handle_t"
      ~size:HandleType.(size handle |> Unsigned.Size_t.to_int)
      ~alignment:1
  let t' = ptr t
  let alloc_cb = t' @-> size_t @-> Buf.t' @-> returning void |> funptr
  let close_cb = t' @-> returning void |> funptr
  let close = t' @-> close_cb @-> returning void |> foreign "uv_close"
end

module Idle = struct
  type t
  let t : t abstract typ =
    abstract
      ~name:"uv_idle_t"
      ~size:HandleType.(size idle |> Unsigned.Size_t.to_int)
      ~alignment:1
  let t' = ptr t
  let idle_cb = t' @-> returning void |> funptr
  let init loop idle =
    let native = Loop.t' @-> t' @-> returning int |> foreign "uv_idle_init" in
    native loop idle |> handle_error
  let start idle cb =
    let native = t' @-> idle_cb @-> returning int |> foreign "uv_idle_start" in
    native idle cb |> handle_error
  let stop idle =
    let native = t' @-> returning int |> foreign "uv_idle_stop" in
    native idle |> handle_error
end

module Stream = struct
  type t
  let t : t abstract typ =
    abstract
      ~name:"uv_stream_t"
      ~size:HandleType.(size stream |> Unsigned.Size_t.to_int)
      ~alignment:0
  let t' = ptr t
  let connection_cb = t' @-> int @-> returning void |> funptr
  let read_cb = t' @-> ssize_t @-> Buf.t' @-> returning void |> funptr
  let listen stream backlog cb =
    let native = t' @-> int @-> connection_cb @-> returning int |> foreign "uv_listen" in
    native stream backlog cb |> handle_error
  let accept server client =
    let native = t' @-> t' @-> returning int |> foreign "uv_accept" in
    native server client |> handle_error
  let read_start stream alloc_cb cb =
    let native = t' @-> Handle.alloc_cb @-> read_cb @-> returning int |> foreign "uv_read_start" in
    native stream alloc_cb cb |> handle_error
end

module Addr = struct
  (* http://www.linuxhowtos.org/data/6/sockaddr *)
  type in_addr
  let in_addr : in_addr structure typ = structure "in_addr"
  let in_addr' = ptr in_addr
  let s_addr = field in_addr "s_addr" ulong
  let () = seal in_addr
		
  type t
  let t : t structure typ = structure "sockaddr_in"
  let t' = ptr t
  let sin_family = field t "sin_family" short
  let sin_port = field t "sin_port" ushort
  let sin_addr = field t "sin_addr" in_addr
  let sin_zero = field t "sin_zero" (array 8 char)
  let () = seal t
		
  let ip4 address port =
    let sockaddr = t |> make |> addr in
    let native = string @-> int @-> t' @-> returning int |> foreign "uv_ip4_addr" in
    native address port sockaddr |> handle_error;
    sockaddr
end

module Tcp = struct
  module Flags = struct
    type t = Unsigned.UInt.t
    let t = uint
    let none : t = Unsigned.UInt.of_int 0
    let ipv6only : t = Unsigned.UInt.of_int 1
    let (+) = Unsigned.UInt.logor
  end

  type t
  let t : t abstract typ =
    abstract
      ~name:"uv_tcp_t"
      ~size:HandleType.(size tcp |> Unsigned.Size_t.to_int)
      ~alignment:0
  let t' = ptr t
  let to_stream = coerce t' Stream.t'
  let to_handle = coerce t' Handle.t'
  let alloc () =
    allocate_n t ~count:1
  let init loop tcp =
    let native = Loop.t' @-> t' @-> returning int |> foreign "uv_tcp_init" in
    native loop tcp |> handle_error
  let make loop =
    let tcp = alloc () in
    init loop tcp;
    tcp
  let bind tcp address flags =
    let native = t' @-> Addr.t' @-> Flags.t @-> returning int |> foreign "uv_tcp_bind" in
    native tcp address flags |> handle_error
end

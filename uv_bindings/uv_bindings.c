#include <stdlib.h>
#include <uv.h>

uv_loop_t* uv_handle_get_loop(uv_handle_t* handle) {
  return handle->loop;
}

void* uv_handle_get_data(uv_handle_t* handle) {
  return handle->data;
}

void uv_handle_set_data(uv_handle_t* handle, void* data) {
  handle->data = data;
}

size_t uv_buf_size(void) {
  return sizeof(uv_buf_t);
}

char* uv_buf_get_base(uv_buf_t* buf) {
  return buf->base;
}

void uv_buf_set_base(uv_buf_t* buf, char* base) {
  buf->base = base;
}

size_t uv_buf_get_len(uv_buf_t* buf) {
  return buf->len;
}

void uv_buf_set_len(uv_buf_t* buf, size_t len) {
  buf->len = len;
}

int uv_handle_get_type(uv_handle_t* handle) {
  return handle->type;
}

int uv_try_write_mod(uv_stream_t* handle,
		     uv_buf_t* buf) {
  return uv_try_write(handle, buf, 1);
}

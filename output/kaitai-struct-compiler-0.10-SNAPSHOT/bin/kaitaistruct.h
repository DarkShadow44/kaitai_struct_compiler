#include <stdint.h>

struct ks_handle_;
typedef struct ks_handle_ ks_handle;

struct ks_stream_;
typedef struct ks_stream_ ks_stream;

int ks_stream_read_u1(ks_stream* stream, uint8_t* value, void* ignored1, void* ignored2);
int ks_stream_read_u2le(ks_stream* stream, uint16_t* value, void* ignored1, void* ignored2);
int ks_stream_read_u4le(ks_stream* stream, uint32_t* value, void* ignored1, void* ignored2);

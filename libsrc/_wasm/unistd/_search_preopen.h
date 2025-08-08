#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef bool (*SearchFileCallback)(int base_fd, const char *fn, size_t fnlen, void* data);

// Search from preopens
bool _search_preopen(const char *fn, void *data, SearchFileCallback callback);

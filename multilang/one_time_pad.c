#include <stddef.h>

void one_time_pad(char *plaintext, char *key, char *out, size_t length) {
    for (size_t i = 0; i < length; ++i) {
        out[i] = plaintext[i] ^ key[i];
    }
}
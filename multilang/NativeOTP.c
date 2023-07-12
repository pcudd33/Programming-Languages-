#include <stddef.h>
#include "NativeOTP.h"

JNIEXPORT jbyteArray JNICALL Java_NativeOTP_one_1time_1pad
  (JNIEnv *env, jclass this, jbyteArray plain, jbyteArray key) {

    // native access to the byte array data
    jbyte* n_plain = (*env)->GetByteArrayElements(env, plain, NULL);
    jbyte* n_key = (*env)->GetByteArrayElements(env, key, NULL);

    // get the length 
    size_t len = (*env)->GetArrayLength(env, plain);

    // create the cipher array
    jbyteArray cipher = (*env)->NewByteArray(env, len);
    jbyte* n_cipher = (*env)->GetByteArrayElements(env, cipher, NULL);

    for (size_t i = 0; i < len; i++){
        n_cipher[i] = n_plain[i] ^ n_key[i];
    }

    // "release" our pointer to the arrays
    (*env)->ReleaseByteArrayElements(env, plain, n_plain, 0);
    (*env)->ReleaseByteArrayElements(env, key, n_key, 0);
    (*env)->ReleaseByteArrayElements(env, cipher, n_cipher, 0);

    return cipher;

  }
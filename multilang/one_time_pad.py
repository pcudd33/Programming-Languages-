import ctypes
import secrets
import string
import time

# load the shared object we compiled from c
native = ctypes.cdll.LoadLibrary("libone_time_pad.dylib")

def native_otp(plain, key):
    s = ctypes.create_string_buffer(b'\0' * len(plain))
    native.one_time_pad(ctypes.c_char_p(plain), ctypes.c_char_p(key), s, ctypes.c_size_t(len(plain)))
    # grab everything but the last character (extra null terminator)
    # internal bytestring in python
    return s.raw[:-1]


def python_otp(plain, key):
    # make a byte array for our output
    cipher = bytearray(b" " * len(plain))
    for i in range(len(plain)):
        cipher[i] = plain[i] ^ key[i]


    # convert the bytearray to a "byte" string
    return bytes(cipher)


message = b"The goal is to leave you with a high-quality test suite of snail programs that you can use to evaluate your own P5 Interpreter. Writing an interpreter requires you to consider many corner cases when reading the formal operational semantics rules in the snail specification. While you you can check for correct positive behavior by comparing your interpreter's output to the reference interpreters's output on the usual good snail programs, it is comparatively harder to check for corner case behavior. If you fail to construct a rich test suite of semantically-valid tricky programs you will face a frustrating series of you fail held-out negative test x reports for P5 proper, which can turn into unproductive guessing games. Because students often report that this is frustrating (even though it is, shall we say, infinitely more realistic than making all of the post-deployment tests visible in advance), this checkpoint provides a structured means to help you get started with the constuction of a rich test suite."

key = bytearray(b'\0' * len(message))
for i in range(len(message)):
    key[i] = ord(secrets.choice(string.printable))
key = bytes(key)

# run 100 times
tot = 0
for i in range(100):
    start = time.perf_counter_ns()
    res = python_otp(message, key)
    tot += (time.perf_counter_ns() - start)


print("python: ", tot / 100)

# run 100 times
tot = 0
for i in range(100):
    start = time.perf_counter_ns()
    res = native_otp(message, key)
    tot += (time.perf_counter_ns() - start)


print("native: ", tot / 100)

res = python_otp(message, key)
print(native_otp(res, key))
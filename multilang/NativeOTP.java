import java.security.SecureRandom;

public class NativeOTP {

    static {
        System.loadLibrary("otp");
    }

    // native (c) implementaion
    public static native byte[] one_time_pad(byte[] plain, byte[] key);

    // java implementation of OTP
    public static byte[] java_one_time_pad(byte[] plain, byte[] key) {
        byte[] cipher = new byte[plain.length];

        for (int i = 0; i < plain.length; i++) {
            cipher[i] = (byte) (plain[i] ^ key[i]);
        }

        return cipher;
    }

    public static void main(String[] args) {
        String message = "The goal is to leave you with a high-quality test suite of snail programs that you can use to evaluate your own P5 Interpreter. Writing an interpreter requires you to consider many corner cases when reading the formal operational semantics rules in the snail specification. While you you can check for correct positive behavior by comparing your interpreter's output to the reference interpreters's output on the usual good snail programs, it is comparatively harder to check for corner case behavior. If you fail to construct a rich test suite of semantically-valid tricky programs you will face a frustrating series of you fail held-out negative test x reports for P5 proper, which can turn into unproductive guessing games. Because students often report that this is frustrating (even though it is, shall we say, infinitely more realistic than making all of the post-deployment tests visible in advance), this checkpoint provides a structured means to help you get started with the constuction of a rich test suite.";

        // generate a key
        SecureRandom secRand = new SecureRandom();
        byte[] key = new byte[message.length()];
        // fills the key with secure truly random numbers
        secRand.nextBytes(key);
        
        long tot = 0;
        for (int i = 0; i < 100; i++){
            long startTime = System.nanoTime();
            byte[] encrypted = java_one_time_pad(message.getBytes(), key);
            tot += System.nanoTime() - startTime;
        }
        System.out.println("Java: "+ (tot / 100.) + "ns");
        // byte[] encrypted = java_one_time_pad(message.getBytes(), key);
        // byte[] decrypted = java_one_time_pad(encrypted, key);
        // System.out.println(new String(decrypted));

        tot = 0;
        for (int i = 0; i < 100; i++){
            long startTime = System.nanoTime();
            byte[] encrypted = one_time_pad(message.getBytes(), key);
            tot += System.nanoTime() - startTime;
        }
        System.out.println("Java: "+ (tot / 100.) + "ns");
    
    }
    
}
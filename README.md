#pwcliphs
---------
[pwclip](https://github.com/davidlazar/pwclip) in Haskell

Requirements: My fork of [DRBG](https://github.com/karshan/DRBG)

Usage: pwcliphs key.pgp passwordfile

passwordfile:

    PasswordSettings { len = 48, prefix = "", charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`~!@#$%^&*()_-+={}|[]\\:\";'<>?,./", url = "amazon.com", username = "bob@gmail.com" }

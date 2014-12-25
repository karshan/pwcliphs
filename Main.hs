{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>))
import Crypto.Random.DRBG (HmacDRBG, newGen, reseed, genBytes, GenError)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (pack, unpack, take, foldr, length, index, readFile, putStrLn)
import Data.Char (ord)
import Data.Default (Default(..))
import System.Environment (getArgs)

data PasswordSettings = PasswordSettings { len :: Int, prefix :: ByteString, charset :: ByteString, url :: ByteString, username :: ByteString } 
    deriving (Show, Read)

genPassword :: ByteString -> PasswordSettings -> Maybe ByteString -> Either GenError ByteString
genPassword key settings question = 
    newGen key >>=
    reseed (url settings) >>=
    reseed (username settings) >>=
    \(drbg::HmacDRBG) -> maybe (return drbg) (\q -> reseed q drbg) question >>=
    genBytes (len settings) >>=
    \(bytes, _) -> return $ BS.take (len settings) $ baseX bytes (charset settings)

-- convert bytes to integer assuming little endian ordering
readLE :: ByteString -> Integer
readLE = BS.foldr (\e s -> (s * 256) + (fromIntegral $ ord e)) 0

baseX :: ByteString -> ByteString -> ByteString
baseX bs alphabet = BS.pack $ reverse $ map (BS.index alphabet . fromIntegral . (`mod` x)) $ takeWhile (> 0) $ iterate (`div` x) $ readLE bs
    where
        x :: Integer
        x = fromIntegral $ BS.length alphabet

main = do
    (keyfile:pwfile:_) <- getArgs
    key <- BS.readFile keyfile
    pwsettings <- (read . BS.unpack) <$> BS.readFile pwfile
    either print BS.putStrLn $ genPassword key pwsettings Nothing

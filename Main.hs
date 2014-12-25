{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (pack, unpack, take, foldr, length, index, readFile, putStrLn)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack, unpack)
import Data.Char (ord)
import Data.Digest.Pure.SHA (bytestringDigest, hmacSha512)
import Data.Monoid ((<>))
import System.Environment (getArgs)

data PasswordSettings = PasswordSettings { len :: Int, prefix :: ByteString, charset :: ByteString, url :: ByteString, username :: ByteString } 
    deriving (Show, Read)

genPassword :: ByteString -> PasswordSettings -> ByteString -> ByteString
genPassword key settings additional = BS.take (len settings) $ baseX (charset settings) $ bytestringDigest' $ hmacSha512 (lbs key) (lbs d)
    where
        d = (url settings) <> (username settings) <> additional
        bytestringDigest' = BS.pack . LBS.unpack . bytestringDigest
        lbs = LBS.pack . BS.unpack

-- convert bytes to integer assuming little endian ordering
readLE :: ByteString -> Integer
readLE = BS.foldr (\e s -> (s * 256) + (fromIntegral $ ord e)) 0

baseX :: ByteString -> ByteString -> ByteString
baseX alphabet bs = BS.pack $ reverse $ map (BS.index alphabet . fromIntegral . (`mod` x)) $ takeWhile (> 0) $ iterate (`div` x) $ readLE bs
    where
        x :: Integer
        x = fromIntegral $ BS.length alphabet

main = do
    (keyfile:pwfile:_) <- getArgs
    key <- BS.readFile keyfile
    pwsettings <- (read . BS.unpack) <$> BS.readFile pwfile
    BS.putStrLn $ genPassword key pwsettings ""

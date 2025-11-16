-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultilineStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Wasm
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "add" add :: Int -> Int -> Int
foreign export javascript "sub" sub :: Int -> Int -> Int
foreign export javascript "domAppend" domAppend :: IO ()
foreign export javascript "jsaddleAppend" jsaddleAppend :: IO ()
#endif
-----------------------------------------------------------------------------
add :: Int -> Int -> Int
add x y = x + y
-----------------------------------------------------------------------------
sub :: Int -> Int -> Int
sub x y = x - y
-----------------------------------------------------------------------------
main :: IO ()
main = pure ()
-----------------------------------------------------------------------------
foreign import javascript
  """
  document.body.appendChild(document.createElement('div'));
  """ domAppend :: IO ()
-----------------------------------------------------------------------------
jsaddleAppend :: IO ()
jsaddleAppend = run $ do
  doc <- jsg "document"
  body <- jsg "document" ! "body"
  ele <- doc # "createElement" $ [ "div" ]
  body # "appendChild" $ [ ele ]
  pure ()
-----------------------------------------------------------------------------

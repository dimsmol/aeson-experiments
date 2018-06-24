{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( someFunc
  ) where

import Data.Aeson (FromJSON, Object, Value (Object, String), (.:), decode)
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as H
import Data.Text (Text, unpack)

-- TODO:
-- - move parseEither to the top as did in Lib5
-- - get rid of Ret
-- - put IO into subcall
-- - polymorphism
-- - (FromJSON a, FC c a) - allow either
-- - f call like callMe f ["field1", "field2"] ?

obj :: Object
obj = fromJust $ decode "{\"?\":\"print\",\"v\": {\"?\":\"show\",\"v\":{\"?\":\"add\",\"a\":{\"?\":\"add\",\"a\":1,\"b\":2},\"b\":3}}  }"

type Func a = Object -> Parser a

data Ret
  = RetString (Func String)
  | RetInt (Func Int)
  | RetIO (Func (IO ()))

class FuncBox b a where
  getFunc :: b -> String -> Func a

type ApiWrapped = [(String, Object -> IO (Either String String))]
data Api = Api [(String, Ret)]

instance FuncBox Api String where
  getFunc (Api l) k = case lookup k l of
    Nothing -> undefined
    Just r -> case r of
      RetString f -> f
      _ -> undefined

instance FuncBox Api Int where
  getFunc (Api l) k = case lookup k l of
    Nothing -> undefined
    Just r -> case r of
      RetInt f -> f
      _ -> undefined

add :: Int -> Int -> Int
add = (+)

show1 :: Int -> String
show1 = show

api :: Api
api = Api
  [ ("add", RetInt $ \o -> add <$> arg api "a" o <*> arg api "b" o)
  , ("show", RetString $ \o -> show1 <$> arg api "v" o)
  , ("print", RetIO $ \o -> putStrLn <$> arg api "v" o)
  ]

-- wrap :: Api -> ApiWrapped
-- wrap (Api l) = [("print", z)]

wrap :: Api -> ApiWrapped
wrap (Api l) = flip map l $ \(k, f) -> case f of
  RetString f' -> (k, pure . parseEither f')
  RetInt f' -> (k, \o -> pure $ show <$> parseEither f' o)
  RetIO f' ->
    ( k
    , \o -> case parseEither  f' o of
        Left l -> pure $ Left l
        Right r -> do
          r
          pure $ Right "done"
    )

someFunc :: IO ()
someFunc = do
  v <- call (wrap api) obj
  putStrLn $ "res: " ++ show v

arg :: (FromJSON a, FuncBox b a) => b -> Text -> Object -> Parser a
arg b k o = case H.lookup k o of
  Nothing -> o .:k
  Just v -> case v of
    Object o' -> case H.lookup "?" o' of
      Nothing -> o .: k
      Just v -> case v of
        String k' -> getFunc b (unpack k') o'
    _ -> o .: k

call :: ApiWrapped -> Object -> IO (Either String String)
call w o = case H.lookup "?" o of
  Nothing -> pure $ Left "no tag"
  Just v -> case v of
    String k -> case lookup (unpack k) w of
      Nothing -> pure $ Left $ "no func " ++ show k
      Just f -> f o
    _ -> pure $ Left "non-string tag"

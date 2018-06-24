{-# LANGUAGE OverloadedStrings #-}
module LibSimple
  ( someFunc
  ) where

import Data.Aeson (Encoding, Object, Value (String), ToJSON (toEncoding), withObject, (.:), decode)
import Data.Aeson.Types (Parser, parseEither, typeMismatch)
import Data.Aeson.Internal (JSONPathElement (Key), (<?>))
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as H
import Data.Text (Text, unpack)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- no subcalls

val :: Value
val = fromJust $ decode "{\"?\":\"print\",\"v\":\"zzz\"}"

print1 :: MonadIO m => Text -> m Text
print1 s = do
  liftIO $ putStrLn $ unpack s
  pure s

api :: Api IO Encoding
api =
  [ ("print", enc $ \o -> print1 <$> o .: "v")
  ]

someFunc :: IO ()
someFunc = do
  r <- parseExec (call api) val
  putStrLn $ "res: " ++ show r

-- util

type Api m a = [(Text, Object -> Parser (m a))]

enc :: (Functor f1, Functor f2, Functor f3, ToJSON a) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Encoding))
enc = fmap $ fmap $ fmap toEncoding

parseExec :: Applicative m => (Value -> Parser (m a)) -> Value -> m (Either String a)
parseExec f v = either (pure . Left) (Right <$>) $ parseEither f v

tagKey :: Text
tagKey = "?"

call :: Api m a -> Value -> Parser (m a)
call api =
  withObject "object" $
    \o ->
      case H.lookup tagKey o of
        Nothing -> fail $ "key " ++ show tagKey ++ " not present"
        Just v -> case v of
          String tag -> case lookup tag api of
            Nothing -> fail ("tag " ++ show tag ++ " not supported") <?> Key tagKey
            Just f -> f o
          _ -> typeMismatch "string" v <?> Key tagKey

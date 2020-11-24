
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}


-- |
-- Module      : System.Envy
-- Copyright   : (c) David Johnson 2015
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > module Main ( main ) where
-- >
-- > import System.Envy
-- > import GHC.Generics
-- >
-- > data PGConfig = PGConfig {
-- >   pgHost :: String -- "PG_HOST"
-- > , pgPort :: Int    -- "PG_PORT"
-- > } deriving (Generic, Show)
-- >
-- > -- Default instance used if environment variable doesn't exist
-- > instance DefConfig PGConfig where
-- >   defConfig = PGConfig "localhost" 5432
-- >
-- > instance FromEnv PGConfig
-- > -- Generically produces the following body (no implementation needed if using Generics):
-- > -- fromEnv = PGConfig <$> envMaybe "PG_HOST" .!= "localhost"
-- > --                    <*> envMaybe "PG_PORT" .!= 5432
-- >
-- > main :: IO ()
-- > main =
-- >   print =<< do decodeEnv :: IO (Either String PGConfig)
-- >  -- PGConfig { pgHost = "custom-pg-url", pgPort = 5432 }
--
module System.Envy
       ( -- * Classes
          FromEnv (..)
        , fromEnv
        , ToEnv   (..)
        , Var     (..)
        , EnvList
        , ParserT  (..)
        , Parser
        -- ** extract list of variables
        , Extract(..)
        -- * Functions
        , decodeEnv
        , decodeEnvT
        , decodeEnvIO
        , eitherToMaybe
        , decode
        , decodeIO
         -- ** utility functions
        , showEnv
        , setEnvironment
        , setEnvironment'
        , unsetEnvironment
        , unsetEnvironment'
        , makeEnv
          -- ** environment variable getters
        , env
        , envParserT
        , envParser
        , runEnv 
        , runEnvParserT 
        , runEnvParser 
        , envM
        , envIO
        , envMaybeM
        , envMaybe
        , envMaybeR
        , envMaybeIO
           -- ** operators
        , (.=)
        , (.!=)
          -- * Generics
        , DefConfig (..)
        , Option (..)
        , gFromEnvCustomT
        , gFromEnvCustom
          -- * run parsers
        , evalParser
        , evalParserT
        , evalParserIO
          -- * convenience re-exports
        , MonadError(..)
        , ExceptT(..)
        , Except
        , MonadIO(..)
        , MonadReader(..)
        , ReaderT(..)
       )
      where
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Except
import           Control.Exception
import           Data.Maybe
import           Data.Char
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           System.Environment
import           Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Word
import           Data.Int
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified Data.Map as M
import Data.Functor.Identity
import Control.Monad.Reader
--import Control.Monad.Trans

-- | map from strings to strings
type Env = M.Map String String

-- | Parser transformer for environment variable retrieval
newtype ParserT m a = ParserT { runParserT :: ReaderT Env (ExceptT String m) a }
  deriving ( Functor, Applicative, Monad, MonadError String
           , MonadIO, Alternative, MonadPlus, MonadReader Env )

-- | Parser for environment variable retrieval
type Parser a = ParserT Identity a


-- | Variable type, smart constructor for handling environment variables.
data EnvVar = EnvVar {
  variableName :: String,
  -- ^ The environment variable to set.
  variableValue :: String
  -- ^ The value to assign this variable to.
  }
  deriving (Show, Eq)

-- | Executes `Parser`
evalParserT :: ParserT m a -> Env -> m (Either String a)
evalParserT a env = runExceptT $ (`runReaderT` env) $ runParserT a

evalParser :: Parser a -> Env -> Either String a
evalParser a env = runIdentity $ evalParserT a env

-- | Execute parser in IO monad, looking up
-- environment variables.
-- 
-- > getPgConfig :: IO (Either String ConnectInfo)
-- > getPgConfig = evalParserIO $ gFromEnvCustom defOption
--
evalParserIO :: MonadIO m => Parser a -> m (Either String a)
evalParserIO a = do
  e <- M.fromList <$> liftIO getEnvironment
  let b = evalParser a e
  return b

env ::
  (MonadReader Env m, MonadError String m, Var b) => String -> m b
env key =
  ask >>= (`envM` key)

envParserT ::
  (Monad m, Var b) => String -> ParserT m b
envParserT = env

envParser :: (Var b) => String -> Parser b
envParser = envParserT

runEnv :: (MonadError String m, Var a) => String -> Env -> m a
runEnv key = runReaderT (env key)

runEnvParserT :: (Monad m, Var a) => String -> Env -> ParserT m a
runEnvParserT = runEnv

runEnvParser :: (Var a) => String -> Env -> Parser a
runEnvParser = runEnv


-- | like envIO, but with an explicitly passed in
-- environment, and in any MonadError, not just a Parser
envM ::
  (MonadError String m, Var a) => Env -> String -> m a
envM es key =
  fromVarError key $ M.lookup key es


-- Environment variable getter. Fails if the variable is not set or
-- fails to parse.
envIO ::
  (MonadIO m, Var a)
      => String   -- ^ Key to look up.
      -> m (Parser a) -- ^ Return a value of this type or throw an error.
envIO key =
  runEnvParser key . M.fromList <$> liftIO getEnvironment

-- | like fromVar, but throw an explanatory
-- error when a var could not be found or could
-- not be parsed.
fromVarError ::
  (MonadError String m, Var a) => String -> Maybe String -> m a
fromVarError key result =
  case result of
    Nothing -> throwError $ "Variable not found for: " ++ key
    Just dv ->
      case fromVar dv of
        Nothing -> throwError ("Parse failure: could not parse variable "
                                 ++ show key ++ " into type "
                                 ++ show (typeOf dv))
        Just x -> return x


envMaybeM :: (Monad m, Var a) => String -> Env -> m (Maybe a)
envMaybeM = runReaderT . envMaybeR

envMaybe :: Var a => String -> Env -> Maybe a
envMaybe = runReader . envMaybeR

-- | like envMaybeIO, but pure and with an explicitly passed in
-- list of variable bindings.
envMaybeR ::
  (MonadReader Env m, Var a) =>
  String -> m (Maybe a)
envMaybeR key = asks (maybe Nothing fromVar . M.lookup key)

-- | Environment variable getter returning `Maybe`
envMaybeIO :: (Var a, MonadIO m)
         => String           -- ^ Key to look up.
         -> m (Maybe a) -- ^ Return `Nothing` if variable isn't set.
envMaybeIO key =
  envMaybe key . M.fromList <$> liftIO getEnvironment


-- | For use with `envMaybe` for providing default arguments.
(.!=) :: Functor f =>
         f (Maybe a) -- ^ Parser that might fail.
      -> a                -- ^ Value to return if the parser fails.
      -> f a         -- ^ Parser that returns the default on failure.
(.!=) parser def  = fromMaybe def <$> parser

-- | Infix environment variable setter
-- Smart constructor for producing types of `EnvVar`
(.=) :: Var a
     => String -- ^ The variable name to set.
     -> a      -- ^ Object to set in the environment.
     -> EnvVar -- ^ Mapping of Variable to Value.
(.=) variableName value = EnvVar variableName (toVar value)


-- | `FromEnv` Typeclass w/ Generic default implementation
class FromEnv a where
  -- | A 'ParserT' you either (a) write yourself, or
  -- (b) if there are 'Generic' and 'DefConfig' instances
  -- of @a@, then one is magicked up for you.
  fromEnvT :: Monad m => ParserT m a
  default fromEnvT :: (DefConfig a, Generic a, GFromEnvT (Rep a), Monad m) => ParserT m a
  fromEnvT =  gFromEnvCustomT defOption

-- | 'fromEnvT' in the 'Identity' monad.
fromEnv :: (FromEnv a) => Parser a
fromEnv = fromEnvT

-- | Meant for specifying a custom `Option` for environment retrieval
--
-- > instance FromEnv PGConfig where
-- >   fromEnv = gFromEnvCustomT Option { dropPrefixCount = 8, customPrefix = "PG" }
--
gFromEnvCustomT :: forall a  m .
  (GFromEnvT (Rep a), Generic a, DefConfig a, Monad m) =>
  Option -> ParserT m a
gFromEnvCustomT opts = to <$> gFromEnvT (from (defConfig :: a)) opts

-- | 'gFromEnvCustomT' in the Identity monad.
gFromEnvCustom ::
  (GFromEnvT (Rep a), Generic a, DefConfig a) =>
  Option -> Parser a
gFromEnvCustom = gFromEnvCustomT

-- | `Generic` FromEnv
class GFromEnvT f where
  gFromEnvT :: Monad m => f a -> Option -> ParserT m (f a)

-- | Extract a list of the environment variable names a parser for
-- some type @a@ will look for.
class Extract a where
  extract :: a -> [String]
  default extract :: (GExtract (Rep a), Generic a) => a -> [String]
  extract x = gExtract (from x) defOption

class GExtract f where
  gExtract :: f a -> Option -> [String]

instance (GExtract a, GExtract b) => GExtract (a :*: b) where
  gExtract (a :*: b) opts = gExtract a opts <> gExtract b opts
instance GExtract a => GExtract (C1 i a) where
  gExtract (M1 x) = gExtract x
instance GExtract a => GExtract (D1 i a) where
  gExtract (M1 x) = gExtract x
instance (Selector s) => GExtract (S1 s (K1 i a)) where
  gExtract m@(M1 (K1 _def)) opts = [toEnvName opts $ selName m]

-- | convert some field name into a suitable
-- env var name.
toEnvName :: Option -> String -> String
toEnvName Option{..} xs =
  let name = snake (drop dropPrefixCount xs)
  in if customPrefix == mempty
       then name
       else map toUpper customPrefix ++ "_" ++ name

snake :: String -> String
snake = map toUpper . snakeCase
  where
      applyFirst :: (Char -> Char) -> String -> String
      applyFirst _ []     = []
      applyFirst f [x]    = [f x]
      applyFirst f (x:xs) = f x: xs

      snakeCase :: String -> String
      snakeCase = u . applyFirst toLower
        where u []                 = []
              u (x:xs) | isUpper x = '_' : toLower x : snakeCase xs
                       | otherwise = x : u xs


-- | Type class for objects which have a default configuration.
class DefConfig a where defConfig :: a

-- | For customizing environment variable generation
data Option = Option {
    dropPrefixCount :: Int  -- ^ Applied first
  , customPrefix :: String  -- ^ Converted toUpper
  } deriving Show

-- | Default `Option` for field modification
defOption :: Option
defOption = Option 0 mempty


-- | Products
instance (GFromEnvT a, GFromEnvT b) => GFromEnvT (a :*: b) where
  gFromEnvT  (a :*: b) opts  = liftA2 (:*:) (gFromEnvT a opts) (gFromEnvT b opts)

-- | Don't absorb meta data
instance GFromEnvT a => GFromEnvT (C1 i a) where
  gFromEnvT (M1 x) opts = M1 <$> gFromEnvT x opts

-- | Don't absorb meta data
instance GFromEnvT a => GFromEnvT (D1 i a) where
  gFromEnvT (M1 x) opts = M1 <$> gFromEnvT x opts

-- | Construct a `Parser` from a `selName` and `DefConfig` record field
instance (Selector s, Var a) => GFromEnvT (S1 s (K1 i a)) where
  gFromEnvT m@(M1 (K1 def)) opts =
      M1 . K1 <$> envMaybeR (toEnvName opts $ selName m) .!= def


-- | Type class for objects which can be converted to a set of
-- environment variable settings.
class ToEnv a where
  -- | Convert an object into a list of environment variable settings.
  toEnv :: a -> EnvList a

-- | List of environment variables. Captures a "phantom type" which
-- allows the type checker to detect the proper implementation of toEnv
-- to use.
newtype EnvList a = EnvList [EnvVar] deriving (Show)

-- | Smart constructor, environment creation helper.
makeEnv :: [EnvVar] -> EnvList a
makeEnv = EnvList

-- | Class for converting to / from an environment variable
class Typeable a => Var a where
  -- | Convert a value into an environment variable.
  toVar   :: a -> String
  -- | Parse an environment variable.
  fromVar :: String -> Maybe a

instance Var Text where toVar = T.unpack; fromVar = Just . T.pack
instance Var TL.Text where toVar = TL.unpack; fromVar = Just . TL.pack
instance Var BL8.ByteString where toVar = BL8.unpack; fromVar = Just . BL8.pack
instance Var B8.ByteString where toVar = B8.unpack; fromVar = Just . B8.pack
instance Var Int where toVar = show; fromVar = readMaybe
instance Var Int8 where toVar = show; fromVar = readMaybe
instance Var Int16 where toVar = show; fromVar = readMaybe
instance Var Int32 where toVar = show; fromVar = readMaybe
instance Var Int64 where toVar = show; fromVar = readMaybe
instance Var Integer where toVar = show; fromVar = readMaybe
instance Var UTCTime where toVar = show; fromVar = readMaybe
instance Var Day where toVar = show; fromVar = readMaybe
instance Var Word8 where toVar = show; fromVar = readMaybe
instance Var Bool where toVar = show; fromVar = readMaybe
instance Var Double where toVar = show; fromVar = readMaybe
instance Var Word16 where toVar = show; fromVar = readMaybe
instance Var Word32 where toVar = show; fromVar = readMaybe
instance Var Word64 where toVar = show; fromVar = readMaybe
instance Var String where toVar = id; fromVar = Just
instance Var () where toVar = const "()"; fromVar = const $ Just ()
instance Var a => Var (Maybe a) where
  toVar = maybe "" toVar
  fromVar "" = Nothing
  fromVar  s = Just <$> fromVar s

-- | Environment retrieval with failure info
decodeEnvT :: (Monad m, FromEnv a) => Env -> m (Either String a)
decodeEnvT = evalParserT fromEnvT

-- | Environment retrieval with failure info
decodeEnv :: FromEnv a => Env -> Either String a
decodeEnv = evalParser fromEnv

-- | Environment retrieval with failure info
decodeEnvIO :: (MonadIO m, FromEnv a) => m (Either String a)
decodeEnvIO = evalParserIO fromEnv

eitherToMaybe :: Either a1 a2 -> Maybe a2
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

-- | Environment retrieval (with no failure info)
decode :: FromEnv a => Env -> Maybe a
decode = eitherToMaybe . decodeEnv

-- | Environment retrieval (with no failure info)
decodeIO :: (MonadIO m, FromEnv a) => m (Maybe a)
decodeIO = eitherToMaybe <$> decodeEnvIO


-- | Catch an IO exception and return it in an Either.
wrapIOException :: IO a -> IO (Either String a)
wrapIOException action = try action >>= \case
  Left (ex :: IOException) -> return $ Left $ show ex
  Right x -> return $ Right x

-- | Set environment via a ToEnv constrained type
setEnvironment :: EnvList a -> IO (Either String ())
setEnvironment (EnvList envVars) = wrapIOException $ mapM_ set envVars
  where set var = setEnv (variableName var) (variableValue var)

-- | Set environment directly using a value of class ToEnv
setEnvironment' :: ToEnv a => a -> IO (Either String ())
setEnvironment' = setEnvironment . toEnv

-- | Unset Environment from a `ToEnv` constrained type
unsetEnvironment :: EnvList a -> IO (Either String ())
unsetEnvironment (EnvList envVars) = wrapIOException $ mapM_ unset envVars
  where unset var = unsetEnv (variableName var)

-- | Unset Environment using a value of class ToEnv
unsetEnvironment' :: ToEnv a => a -> IO (Either String ())
unsetEnvironment' = unsetEnvironment . toEnv

-- | Display all environment variables, for convenience
showEnv :: IO ()
showEnv = mapM_ print =<< getEnvironment


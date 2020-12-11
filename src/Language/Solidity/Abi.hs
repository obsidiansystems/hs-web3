{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      :  Data.Solidity.Abi.Json
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- JSON encoded contract ABI parsers.
--

module Language.Solidity.Abi
    (
    -- * Contract ABI declarations
      ContractAbi(..)
    , Declaration(..)
    , FunctionArg(..)
    , EventArg(..)
    , StateMutability(..)

    -- * Method/Event id encoder
    , signature
    , methodId
    , eventId

    -- * Solidity type parser
    , SolidityType(..)
    , parseSolidityFunctionArgType
    , parseSolidityEventArgType
    ) where

import           Control.Monad      (void)
import           Crypto.Hash        (Digest, Keccak_256, hash)
import           Data.Aeson         (FromJSON (parseJSON), Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
                                     SumEncoding (..),
                                     ToJSON (toJSON), defaultOptions)
import           Data.Aeson.TH      (deriveJSON)
import qualified Data.Char          as C (toLower)
import           Data.Text          (Text)
import qualified Data.Text          as T (dropEnd, intercalate, pack, take, unlines, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Text.Parsec        (ParseError, char, choice, digit, eof,
                                     lookAhead, many, many1, manyTill, optionMaybe,
                                     parse, string, try, (<|>))
import           Text.Parsec.Text   (Parser)

import           Data.String.Extra  (toLowerFirst)

-- | Method argument
data FunctionArg = FunctionArg
    { funArgName       :: Text
    -- ^ Argument name
    , funArgType       :: Text
    -- ^ Argument type
    , funArgComponents :: Maybe [FunctionArg]
    -- ^ Argument components for tuples
    }
    deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''FunctionArg)

-- | Event argument
data EventArg = EventArg
    { eveArgName    :: Text
    -- ^ Argument name
    , eveArgType    :: Text
    -- ^ Argument type
    , eveArgIndexed :: Bool
    -- ^ Argument is indexed (e.g. placed on topics of event)
    }
    deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''EventArg)

data StateMutability
  = SMPure
  | SMView
  | SMPayable
  | SMNonPayable
  deriving (Eq, Ord, Show)

-- | Elementary contract interface item
data Declaration = DConstructor
    { conInputs :: [FunctionArg]
    -- ^ Contract constructor
    }
    | DFunction
    { funName     :: Text
    , funStateMutability :: StateMutability
    , funInputs   :: [FunctionArg]
    , funOutputs  :: Maybe [FunctionArg]
    -- ^ Method
    }
    | DEvent
    { eveName      :: Text
    , eveInputs    :: [EventArg]
    , eveAnonymous :: Bool
    -- ^ Event
    }
    | DFallback
    { falPayable :: Bool
    -- ^ Fallback function
    }
    deriving Show

instance Eq Declaration where
    (DConstructor a) == (DConstructor b) = length a == length b
    (DFunction a _ _ _) == (DFunction b _ _ _) = a == b
    (DEvent a _ _) == (DEvent b _ _) = a == b
    (DFallback _) == (DFallback _) = True
    (==) _ _ = False

instance Ord Declaration where
    compare (DConstructor a) (DConstructor b) = compare (length a) (length b)
    compare (DFunction a _ _ _) (DFunction b _ _ _) = compare a b
    compare (DEvent a _ _) (DEvent b _ _) = compare a b
    compare (DFallback _) (DFallback _) = EQ

    compare DConstructor {} DFunction {} = LT
    compare DConstructor {} DEvent {} = LT
    compare DConstructor {} DFallback {} = LT

    compare DFunction {} DConstructor {} = GT
    compare DFunction {} DEvent {} = LT
    compare DFunction {} DFallback {} = LT

    compare DEvent {} DConstructor {} = GT
    compare DEvent {} DFunction {} = GT
    compare DEvent {} DFallback {} = LT

    compare DFallback {} DConstructor {} = GT
    compare DFallback {} DFunction {} = GT
    compare DFallback {} DEvent {} = GT

$(deriveJSON (defaultOptions {
    sumEncoding = TaggedObject "type" "contents"
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)

$(deriveJSON (defaultOptions {
    sumEncoding = TaggedObject "stateMutability" "contents"
  , constructorTagModifier = fmap C.toLower . drop 2 })
    ''StateMutability)


-- | Contract Abi is a list of method / event declarations
newtype ContractAbi = ContractAbi { unAbi :: [Declaration] }
  deriving (Eq, Ord)

instance FromJSON ContractAbi where
    parseJSON = fmap ContractAbi . parseJSON

instance ToJSON ContractAbi where
    toJSON = toJSON . unAbi

instance Show ContractAbi where
    show (ContractAbi c) = T.unpack $ T.unlines $
        [ "Contract:" ]
        ++ foldMap showConstructor c ++
        [ "\tEvents:" ]
        ++ foldMap showEvent c ++
        [ "\tMethods:" ]
        ++ foldMap showMethod c

showConstructor :: Declaration -> [Text]
showConstructor x = case x of
    DConstructor{} -> ["\tConstructor " <> signature x]
    _              -> []

showEvent :: Declaration -> [Text]
showEvent x = case x of
    DEvent{} -> ["\t\t" <> signature x]
    _        -> []

showMethod :: Declaration -> [Text]
showMethod x = case x of
    DFunction{} ->
        ["\t\t" <> methodId x <> " " <> signature x]
    _ -> []

componentsName :: [FunctionArg] -> Text
componentsName args = "(" <> (T.intercalate "," $ fmap componentName args) <> ")"
  where
    componentName :: FunctionArg -> Text
    componentName x = case funArgComponents x of
      Nothing   -> funArgType x
      Just cmps -> componentsName cmps

-- | Take a signature by given decl, e.g. foo(uint,string)
signature :: Declaration -> Text
signature (DConstructor inputs) = componentsName inputs
signature (DFallback _) = "()"
signature (DFunction name _ inputs _) = name <> componentsName inputs
signature (DEvent name inputs _) = name <> "(" <> args inputs <> ")"
  where
    args :: [EventArg] -> Text
    args = T.dropEnd 1 . foldMap (<> ",") . fmap eveArgType

-- | Locally compute Keccak-256 hash of given text
sha3 :: Text -> Text
{-# INLINE sha3 #-}
sha3 x = T.pack (show digest)
  where digest :: Digest Keccak_256
        digest = hash (encodeUtf8 x)

-- | Generate method selector by given method 'Delcaration'
methodId :: Declaration -> Text
{-# INLINE methodId #-}
methodId = ("0x" <>) . T.take 8 . sha3 . signature

-- | Generate event `topic0` hash by givent event 'Delcaration'
eventId :: Declaration -> Text
{-# INLINE eventId #-}
eventId = ("0x" <>) . sha3 . signature

-- | Solidity types and parsers
data SolidityType = SolidityBool
    | SolidityAddress
    | SolidityUint Int
    | SolidityInt Int
    | SolidityString
    | SolidityBytesN Int
    | SolidityBytes
    | SolidityTuple Int [SolidityType]
    | SolidityVector [Int] SolidityType
    | SolidityArray SolidityType
    deriving (Eq, Show)

numberParser :: Parser Int
numberParser = read <$> many1 digit

parseUint :: Parser SolidityType
parseUint = do
  _ <- string "uint"
  SolidityUint <$> numberParser

parseInt :: Parser SolidityType
parseInt = do
  _ <- string "int"
  SolidityInt <$> numberParser

parseBool :: Parser SolidityType
parseBool = string "bool" >>  pure SolidityBool

parseString :: Parser SolidityType
parseString = string "string" >> pure SolidityString

parseBytes :: Parser SolidityType
parseBytes = do
  _ <- string "bytes"
  mn <- optionMaybe numberParser
  pure $ maybe SolidityBytes SolidityBytesN mn

parseAddress :: Parser SolidityType
parseAddress = string "address" >> pure SolidityAddress

solidityBasicTypeParser :: Parser SolidityType
solidityBasicTypeParser =
    choice [ try parseUint
           , try parseInt
           , try parseAddress
           , try parseBool
           , try parseString
           , parseBytes
           ]

parseVector :: Parser SolidityType
parseVector = do
    s <- solidityBasicTypeParser
    ns <- many1Till lengthParser (lookAhead (void $ string "[]") <|> eof)
    pure $ SolidityVector ns s
  where
    many1Till :: Parser Int -> Parser () -> Parser [Int]
    many1Till p end = do
      a <- p
      as <- manyTill p end
      return (a : as)

    lengthParser = do
          _ <- char '['
          n <- numberParser
          _ <- char ']'
          pure n

parseArray :: Parser SolidityType
parseArray = do
  s <- try (parseVector <* string "[]") <|> (solidityBasicTypeParser <* string "[]")
  pure $ SolidityArray s


solidityTypeParser :: Parser SolidityType
solidityTypeParser =
    choice [ try parseArray
           , try parseVector
           , solidityBasicTypeParser
           ]

parseSolidityFunctionArgType :: FunctionArg -> Either ParseError SolidityType
parseSolidityFunctionArgType (FunctionArg _ typ mcmps) = case mcmps of
  Nothing -> parse solidityTypeParser "Solidity" typ
  Just cmps ->
    let base = SolidityTuple (length cmps) <$> mapM parseSolidityFunctionArgType cmps
    in case parse (string "tuple" *> many (string "[]")) "Solidity" typ of
      Left err -> Left err
      Right arr -> go arr
        where go [] = base
              go (_:xs) = SolidityArray <$> go xs

parseSolidityEventArgType :: EventArg -> Either ParseError SolidityType
parseSolidityEventArgType (EventArg _ typ _) = parse solidityTypeParser "Solidity" typ

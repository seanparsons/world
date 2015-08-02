{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module World.Countries.TH (
  createDeclarations
) where

import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit
import Data.Char
import Data.Csv
import Data.Either
import Data.List
import qualified Data.Vector as V
import Language.Haskell.TH

data CountryData = CountryData
                 { countryName :: String
                 , countryAlpha2Code :: String
                 }

data CountryWithSimpleName = CountryWithSimpleName
                           { countryData :: CountryData
                           , simpleName :: String
                           }

instance FromNamedRecord CountryData where
    parseNamedRecord m = CountryData <$>
                         m .: "Name" <*>
                         m .: "Code"

readWikipediaPage :: IO (V.Vector CountryData)
readWikipediaPage = do
  wikipediaContent <- simpleHttp "https://raw.githubusercontent.com/datasets/country-list/master/data.csv"
  decoded <- either fail return (decodeByName wikipediaContent)
  return $ snd decoded

createSimpleName :: CountryData -> CountryWithSimpleName
createSimpleName countryData =
  let name              = countryName countryData
      containsComma     = elem ',' name
      containsAnd       = isInfixOf " and " name
      containsOf        = isInfixOf " of" name
      capitalise []     = []
      capitalise (x:xs) = (toUpper x) : xs
      reordered         = (\(suffix, prefix) -> (tail prefix) ++ (' ' : suffix)) $ break (== ',') name
      multiName         = filter (not . (== ',')) name
      fixedName         = if containsAnd then multiName else (if containsComma then reordered else name)
      simplerName       = join $ fmap capitalise $ words fixedName
      result            = filter (\c -> not $ elem c ['.', '\'', '-']) $ takeWhile (not . (== '(')) simplerName
      trimmed           = dropWhileEnd isSpace $ dropWhile isSpace result
  in CountryWithSimpleName countryData trimmed

createData :: V.Vector CountryWithSimpleName -> Q [Dec]
createData countryData = do
  let names           = V.toList $ fmap ((flip normalC) []) $ fmap (mkName . simpleName) countryData
  let derivingNames   = fmap mkName ["Generic", "Ord", "Bounded", "Data", "Eq", "Typeable"]
  declaration         <- dataD (cxt []) (mkName "Country") [] names derivingNames
  return $ return declaration

createShow :: V.Vector CountryWithSimpleName -> Q [Dec]
createShow countryData = do
  let className           = mkName "Show"
  let fnName              = mkName "show"
  let clauses             = fmap (\(CountryWithSimpleName (CountryData countryName _ ) simpleName) -> return $ Clause [ConP (mkName simpleName) []] (NormalB $ LitE $ StringL countryName) []) $ V.toList countryData
  fmap return $ instanceD (return []) (appT (conT className) (conT $ mkName "Country")) [funD fnName clauses]

createAlphaMethod :: V.Vector CountryWithSimpleName -> Q [Dec]
createAlphaMethod countryData = undefined

createDeclarations :: Q [Dec]
createDeclarations = do
  countryData <- runIO readWikipediaPage
  let withSimpleNames = fmap createSimpleName countryData
  mainDecl <- createData withSimpleNames
  showDecl <- createShow withSimpleNames
  return $ mainDecl ++ showDecl

{-# LANGUAGE OverloadedStrings #-}

module Selectorsleuth.PropertySpec (spec) where

import Control.Monad (forM_)
import Prelude hiding (any)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Data.PQueue.Min as PQ
import Data.Text (Text)
import qualified Data.Text as T
import Minithesis (TestCase, any, integers, withTests)
import qualified Minithesis.Sydtest as MS
import Selectorsleuth.Generators
  ( DesiredData (..),
    balancedTagListOfSize,
    desiredText,
    similarHtml,
    tagTreeToXml,
    textWithQuotes,
    xpathExpr,
    xmlTreeToTagTree,
  )
import System.IO.Unsafe (unsafePerformIO)
import Test.Syd
import Text.HTML.Selector.Parser (xpathFromString)
import Text.HTML.Selector.Types
  ( EvalMode (..),
    XPathExpr,
    toXPathString,
    toXPathStringRel,
  )
import Text.HTML.Selector.XPath (matchNavTree)
import Text.HTML.SelectorSleuth (sleuth')
import Text.HTML.TagSoup (Tag (..))
import Text.HTML.TagSoup.Tree (TagTree (..), flattenTree, tagTree)
import Text.XML.HXT.Core
  ( ArrowList (constA),
    LA (runLA),
    XmlTree,
    readString,
    runX,
    withParseHTML,
    withRemoveWS,
    withWarnings,
    xshow,
    yes,
    no,
  )
import Text.XML.HXT.XPath.XPathEval (getXPath, parseXPathExpr)

spec :: Spec
spec =
  describe "Selectorsleuth properties" $ do
    MS.prop "desiredIsFoundOnce" $ withTests 200 desiredIsFoundOnce
    MS.prop "desiredIsFoundOnce in HXT" $ withTests 200 desiredIsFoundOnceHxt
    MS.prop "balanced" $ withTests 200 balancedTagListIsBalanced
    MS.prop "can match arbitrary text" $ withTests 200 canMatchArbitraryText
    MS.prop "sleuth can find it" $ withTests 100 sleuthCanFindIt
    MS.prop "XPathExpr roundtrip" $ withTests 200 xpathExprRoundtrip

-------------------------------------------------------------------------------
-- desiredIsFoundOnce

desiredIsFoundOnce :: TestCase -> IO ()
desiredIsFoundOnce tc = do
  trees <- any tc similarHtml
  forM_ (NE.toList trees) $ \(DesiredData desired, tagTree') -> do
    let occurrences =
          length $ filter (== TagText desired) (flattenTree [tagTree'])
    occurrences `shouldBe` 1

-------------------------------------------------------------------------------
-- desiredIsFoundOnce in HXT

desiredIsFoundOnceHxt :: TestCase -> IO ()
desiredIsFoundOnceHxt tc = do
  trees <- any tc similarHtml
  forM_ (NE.toList trees) $ \(DesiredData desired, tagTree') -> do
    let xmlTree = tagTreeToXml tagTree'
        reconstructed = xmlTreeToTagTree xmlTree
        occurrences =
          length $ filter (== TagText desired) (flattenTree [reconstructed])
    occurrences `shouldBe` 1

-------------------------------------------------------------------------------
-- balanced

balancedTagListIsBalanced :: TestCase -> IO ()
balancedTagListIsBalanced tc = do
  size <- fromIntegral <$> any tc (integers 1 100)
  tags <- any tc (balancedTagListOfSize size)
  tagTree tags `shouldSatisfy` ((== 1) . length)

-------------------------------------------------------------------------------
-- can match arbitrary text

canMatchArbitraryText :: TestCase -> IO ()
canMatchArbitraryText tc = do
  text <- any tc textWithQuotes
  let expr =
        XPathExpr $
          NE.fromList [XPathComponent XPCStar [XPPText text]]
      xml = parseXmlTreeUnsafe ("<html>" <> T.unpack text <> "</html>")
  case matchNavTree expr xml of
    Right matches -> matches `shouldSatisfy` ((== 1) . length)
    Left err -> expectationFailure $ "matchNavTree failed: " <> show err

-------------------------------------------------------------------------------
-- sleuth can find it

sleuthCanFindIt :: TestCase -> IO ()
sleuthCanFindIt tc = do
  htmls <- any tc similarHtml
  let pretty :: NonEmpty (DesiredData, TagTree Text, XmlTree)
      pretty = fmap (\(desired, tree) -> (desired, tree, tagTreeToXml tree)) htmls
      dataset :: NonEmpty (NonEmpty Text, XmlTree)
      dataset = fmap (\(target, _tree, xml) -> (pure (desiredText target), xml)) pretty
  case sleuth' 1 dataset of
    Left err -> expectationFailure $ "sleuth' failed: " <> show err
    Right queue ->
      case PQ.minView queue of
        Nothing -> expectationFailure "sleuth' did not yield any selectors"
        Just ((_, (selector, mode)), _) ->
          forM_ (NE.toList pretty) $ \(_, _, xml) ->
            length (getXPath (toXPathString mode selector) xml) `shouldBe` 1

-------------------------------------------------------------------------------
-- XPathExpr roundtrip

xpathExprRoundtrip :: TestCase -> IO ()
xpathExprRoundtrip tc = do
  expr <- any tc xpathExpr
  let rendered = toXPathStringRel expr
  parseXPathExpr rendered `shouldSatisfy` isRight
  xpathFromString rendered `shouldBe` Just expr

-------------------------------------------------------------------------------
-- Helpers

parseXmlTreeUnsafe :: String -> XmlTree
parseXmlTreeUnsafe input =
  singularise . unsafePerformIO $
    runX $
      readString
        [ withParseHTML yes,
          withWarnings no,
          withRemoveWS yes
        ]
        input

singularise :: [a] -> a
singularise [x] = x
singularise _ = error "singularise: expected singleton"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

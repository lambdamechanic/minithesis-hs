{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Selectorsleuth.Generators
  ( DesiredData (..),
    desiredText,
    similarHtml,
    balancedTagListOfSize,
    textWithQuotes,
    xpathExpr,
    tagTreeToXml,
    xmlTreeToTagTree,
  )
where

import Control.Monad (forM_, replicateM, unless)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Minithesis
  ( Strategy,
    integers,
    lists,
    mixOf,
    named,
  )
import Text.HTML.TagSoup (Tag (..))
import Text.HTML.TagSoup.Tree
  ( TagTree (..),
    flattenTree,
    parseTree,
    tagTree,
  )
import Text.HTML.Selector.Types
  ( XPathComponent (..),
    XPathExpr (..),
    XPathMatch (..),
    XPathPredicate (..),
  )
import Text.XML.HXT.Core
  ( ArrowList (constA),
    LA (runLA),
    XmlTree,
    xshow,
  )
import Text.XML.HXT.DOM.Interface (mkName)
import Text.XML.HXT.DOM.XmlNode (mkAttr, mkElement, mkText)

newtype DesiredData = DesiredData Text
  deriving (Eq, Show)

desiredText :: DesiredData -> Text
desiredText (DesiredData t) = t

-- | Strategy producing a balanced list of tags of roughly the requested size.
balancedTagListOfSize :: Int -> Strategy [Tag Text]
balancedTagListOfSize n =
  named ("balancedTagList(" ++ show n ++ ")") (const "<tags>") $
    (TagOpen "html" [] :) . (++ [TagClose "html"]) <$> go [] (max 0 n)
  where
    go :: [Text] -> Int -> Strategy [Tag Text]
    go stack 0 = pure $ closeAll stack
    go [] k = nestTag [] k
    go stack@(x : xs) k =
      frequency
        [ (10, do
              txt <- genTextTag
              rest <- go stack (k - 1)
              pure (TagOpen "armour" [] : TagText txt : TagClose "armour" : rest)
          ),
          (1, do
              rest <- go xs k
              pure (TagClose x : rest)
          ),
          (1, nestTag stack k)
        ]

    closeAll = foldr (\a acc -> TagClose a : acc) []

    nestTag stack k = do
      tag <- genConstrainedTagText
      attrs <- lists attributePair (Just 0) (Just 3)
      rest <- go (tag : stack) (k - 1)
      pure (TagOpen tag attrs : rest)

-- | Strategy yielding a non-empty list of desired data together with matching trees.
similarHtml :: Strategy (NonEmpty (DesiredData, TagTree Text))
similarHtml =
  named "similarHtml" (const "<similarHtml>") $
    do
      junkLen <- fmap fromIntegral $ integers 1 2
      desiredAndJunk <-
        fmap (fromMaybe (error "similarHtml: empty desired data") . NE.nonEmpty) $
          lists (desiredEntry junkLen) (Just 1) (Just 3)
      let uniqueDesiredAndJunk =
            case ensureUnique desiredAndJunk of
              Nothing -> error "similarHtml: failed to make desired data unique"
              Just ne -> ne
      builder <- genHtmlSkeleton 1 junkLen
      trees <-
        traverse
          ( \(desired, junk) ->
              builder [desired] junk
          )
          uniqueDesiredAndJunk
      let paired = NE.zip (fmap fst uniqueDesiredAndJunk) trees
      verify paired
      pure paired
  where
    ensureUnique :: NonEmpty (DesiredData, [TagTree Text]) -> Maybe (NonEmpty (DesiredData, [TagTree Text]))
    ensureUnique entries =
      NE.nonEmpty $
        zipWith
          ( \idx (DesiredData base, junk) ->
              let suffix = "_" <> T.pack (show idx)
               in (DesiredData (base <> suffix), junk)
          )
          [(0 :: Int) ..]
          (NE.toList entries)

    desiredEntry :: Int -> Strategy (DesiredData, [TagTree Text])
    desiredEntry junkLen = do
      desired <- DesiredData . ("target" <>) <$> genText
      junk <-
        replicateM
          junkLen
          (pure $ TagBranch "j" [] [TagLeaf $ TagText "junk"])
      pure (desired, junk)

    verify :: NonEmpty (DesiredData, TagTree Text) -> Strategy ()
    verify entries =
      forM_ entries $ \(DesiredData desired, tree) ->
        unless (TagText desired `elem` canonicalise (flattenTree [tree])) $
          error $
            "similarHtml: desired text not found in generated tree: "
              <> show (desired, tree)

-- | Strategy generating strings that contain quoting characters.
textWithQuotes :: Strategy Text
textWithQuotes =
  named "textWithQuotes" T.unpack $
    fmap T.pack $
      lists
        ( fmap (toEnum . fromIntegral) $
            mixOf
              [ integers 39 39,
                integers 34 34,
                integers 48 57,
                integers 65 90,
                integers 97 122
              ]
        )
        (Just 1)
        (Just 10)

xpathExpr :: Strategy XPathExpr
xpathExpr =
  named "xpathExpr" (const "<xpathExpr>") $ do
    count <- fmap fromIntegral $ integers 1 5
    comps <- replicateM count xpathComponent
    pure . XPathExpr $
      case NE.nonEmpty comps of
        Just ne -> ne
        Nothing -> XPathComponent XPCStar [] :| []

xpathComponent :: Strategy XPathComponent
xpathComponent = do
  match <- xpathMatch
  predicates <- lists xpathPredicate (Just 0) (Just 3)
  let pruned = case match of
        XPCAttr _ -> []
        _ -> predicates
  pure $ XPathComponent match pruned

xpathMatch :: Strategy XPathMatch
xpathMatch =
  mixOf
    [ pure XPCStar,
      XPCLiteral <$> genLiteral,
      XPCAttr <$> (XPCLiteral <$> genLiteral)
    ]

xpathPredicate :: Strategy XPathPredicate
xpathPredicate =
  mixOf
    [ XPPPosition . fromIntegral <$> integers 1 10,
      XPPAttr <$> fmap T.unpack genLiteral <*> fmap T.unpack genLiteral,
      XPPText <$> genLiteral
    ]

genLiteral :: Strategy Text
genLiteral = named "literal" T.unpack $ genTextWithBounds 1 10

genText :: Strategy Text
genText = named "text" T.unpack $ genTextWithBounds 1 10

genTextWithBounds :: Int -> Int -> Strategy Text
genTextWithBounds lo hi =
  fmap T.pack $
    lists
      ( fmap (toEnum . fromIntegral) $
          integers 97 122
      )
      (Just lo)
      (Just hi)

genTextTag :: Strategy Text
genTextTag = mixOf (map pure ["a", "b", "c"])

genConstrainedTagText :: Strategy Text
genConstrainedTagText = mixOf (map pure ["div", "p", "span"])

attributePair :: Strategy (Text, Text)
attributePair =
  (,) <$> genText <*> genText

frequency :: [(Int, Strategy a)] -> Strategy a
frequency [] = error "frequency: empty choices"
frequency options = do
  let total = sum (map fst options)
  pick <- integers 1 (fromIntegral total)
  select (fromIntegral pick) options
  where
    select _ [] = error "frequency: invalid selection"
    select n ((weight, strat) : rest)
      | n <= weight = strat
      | otherwise = select (n - weight) rest

-- | Shuffle a list of optional tag trees.
shuffle :: [Maybe (TagTree Text)] -> Strategy [Maybe (TagTree Text)]
shuffle [] = pure []
shuffle xs = do
  idx <- integers 0 (fromIntegral (length xs - 1))
  let (before, restList) = splitAt (fromIntegral idx) xs
  case restList of
    [] -> shuffle before
    chosen : after -> do
      rest <- shuffle (before <> after)
      pure (chosen : rest)

genTagTree :: Int -> Strategy (TagTree Text)
genTagTree n = do
  tags <- balancedTagListOfSize (max 1 n)
  case tagTree tags of
    [t] -> pure t
    other -> error $ "genTagTree: unexpected result " <> show other

genHtmlSkeleton ::
  Int ->
  Int ->
  Strategy ([DesiredData] -> [TagTree Text] -> Strategy (TagTree Text))
genHtmlSkeleton minDesired minJunk = do
  treeSize <-
    fmap fromIntegral $
      integers
        (fromIntegral (minDesired + minJunk))
        (fromIntegral (minDesired + minJunk + 10))
  tree <- genTagTree treeSize
  let (prefix, leaves) = separatedBy isLeaf $ flattenTree [tree]
      padding = length leaves - minDesired - minJunk
  if padding < 0
    then genHtmlSkeleton minDesired minJunk
    else do
      targetTag <- genConstrainedTagText
      pure $ \desired junk -> do
        let desiredNodes =
              zipWith
                (\idx (DesiredData t) ->
                   TagBranch
                     targetTag
                     [ ("data-target-index", T.pack (show (idx :: Int))),
                       ("data-target", t)
                     ]
                     [TagLeaf $ TagText t]
                )
                [0 ..]
                desired
            newLeaves =
              map Just desiredNodes
                <> map Just junk
                <> replicate padding Nothing
        shuffled <- shuffle newLeaves
        let rebuilt =
              canonicalise $
                concat $
                  zipWith replaceMaybe leaves shuffled
        case tagTree (prefix <> rebuilt) of
          [result] -> pure result
          other -> error $ "genHtmlSkeleton: unexpected rebuild " <> show other

replaceMaybe :: (Tag Text, [Tag Text]) -> Maybe (TagTree Text) -> [Tag Text]
replaceMaybe (leaf, siblings) Nothing = leaf : siblings
replaceMaybe (_, siblings) (Just tree) = flattenTree [tree] <> siblings

separatedBy :: (a -> Bool) -> [a] -> ([a], [(a, [a])])
separatedBy p xs = (prefix, gather rest)
  where
    (prefix, rest) = break p xs

    gather [] = []
    gather (y : ys) =
      let (nonLeaves, tailLeaves) = break p ys
       in (y, nonLeaves) : gather tailLeaves

canonicalise :: [Tag Text] -> [Tag Text]
canonicalise = go ""
  where
    go "" [] = []
    go acc [] = [TagText acc | not (T.null acc)]
    go acc (TagText t : xs) = go (acc <> t) xs
    go "" (x : xs) = x : go "" xs
    go acc (x : xs) = TagText acc : x : go "" xs

isLeaf :: Tag Text -> Bool
isLeaf = \case
  TagText _ -> True
  _ -> False

tagTreeToXml :: TagTree Text -> XmlTree
tagTreeToXml = \case
  TagBranch name attrs children ->
    mkElement (mkName $ T.unpack name) (map toAttr attrs) (map tagTreeToXml children)
  TagLeaf (TagText txt) ->
    mkText $ T.unpack txt
  TagLeaf _ ->
    mkText ""
  where
    toAttr (name, value) =
      mkAttr (mkName $ T.unpack name) [mkText $ T.unpack value]

xmlTreeToTagTree :: XmlTree -> TagTree Text
xmlTreeToTagTree xml =
  case parseTree (T.pack (xmlTreeToString xml)) of
    [t] -> t
    other -> error $ "xmlTreeToTagTree: unexpected parse result " <> show other

xmlTreeToString :: XmlTree -> String
xmlTreeToString xml = head $ runLA (xshow $ constA xml) ("" :: String)

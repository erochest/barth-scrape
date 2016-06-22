{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module BarthPar.Scrape.Read where


import           Control.Applicative
import           Control.Lens           hiding ((<|))
import           Control.Monad
import           Data.Bitraversable
import           Data.Char
import qualified Data.Text              as T
import           Data.Text.Read
import           Data.Tuple
import           Prelude                hiding (div, span)
import           Text.XML.Cursor        hiding (forceM)

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Utils


-- | CD Volume I,1 (§§ 1-12)
parseParagraphID :: Title -> T.Text -> PureScript ChapterLoc
parseParagraphID vtitle pageName =
    cloc <$> ( bisequenceA
             . (fromRomanPS `bimap` (fmap fst . decimal . T.drop 1))
             . T.break (== ',')
             . T.takeWhile (not . isSpace)
             $ T.drop 10 vtitle
             )
         <*> parsePageName pageName
    where
        cloc (v, s) = ChapterLoc v s

-- § 1 The Task of Dogmatics.
parsePageName :: T.Text -> PureScript Int
parsePageName = fmap fst . decimal . T.drop 2

-- readChildSections :: [Cursor] -> PureScript [Section]
-- readChildSections = undefined
{-
 - readChildSections = fmap (toList . thd) . foldlM step (0, Nothing, S.empty)
 -     where
 -         thd :: (a, b, c) -> c
 -         thd (_, _, x) = x
 - 
 -         step :: (Int, Maybe Header, S.Seq Section) -> Cursor
 -              -> PureScript (Int, Maybe Header, S.Seq Section)
 -         step (n, sh, sections) c = do
 -             s <- cleanSection <$> readSection n c
 -             let s' = s & over sectionHead (<|> sh)
 -             return ( succ n
 -                    , _sectionHead s'
 -                    , s' <| sections
 -                    )
 -}

-- makePage :: Title -> T.Text -> [Cursor] -> PureScript Page
-- makePage vtitle pageName (h:a:cs) = undefined
{-
 -     Page <$> first ("Error parsing volume ID: " ++)
 -          (   parseParagraphID vtitle pageName)
 -          <*> (normalize <$> forceM "Missing VolumeTitle"
 -          (  h $// (spanHead >=> child >=> content)))
 -          <*> forceM "Missing Abstract" a'
 -          <*> readChildSections cs'
 -     where
 -         (a', cs') = if length (h $/ span) == 1
 -                     then (buildNodes $ a $|  abstract, cs)
 -                     else (buildNodes $ h $// excursus, a:cs)
 - 
 -         buildNodes :: [Cursor] -> [T.Text]
 -         buildNodes = wrapL
 -                    . normalize
 -                    . TL.toStrict
 -                    . toLazyText
 -                    . foldMap (buildText . node)
 - 
 -         wrapL :: T.Text -> [T.Text]
 -         wrapL t | T.null t  = []
 -                 | otherwise = [t]
 -}

{-
 - makePage vtitle pageName _ =
 -     Left $ "Not enough nodes on " ++ T.unpack pageName ++ show vtitle
 -}

readHeader :: Cursor -> PureScript Header
readHeader c = fmap swap
             . sequenceA
             . ((normalize . T.drop 2) `bimap` anyNumberPS)
             . swap
             . T.break (=='.')
             . T.concat
             $ c $// content

-- readSection :: Int -> Cursor -> PureScript Section
-- readSection _n _c = undefined
    {-
     - SectionData n
     -             <$> (listToMaybe <$> mapM readHeader (c $/ spanHead))
     -             <*> pure (c $/ p >=> toListOf _Element . node)
     -             <*> (   pure
     -                 .   listToMaybe
     -                 $   c
     -                 $/  excursus
     -                 >=> toListOf _Element . node
     -                 )
     -}

-- TODO: Think I can use Control.Lens.Plated here
-- TODO: Replace Cursor navigation with lenses
-- cleanSection :: Section -> Section
-- cleanSection = undefined
{-
 -     over sectionContent (>>= filterEl f)
 -              . over sectionExcursus (>>= filterEl fEx)
 -     where
 -       center :: Element -> Bool
 -       center = (== "center") . view name
 - 
 -       noteLink :: Element -> Bool
 -       noteLink e = e ^. name == "a" && e ^. text == "[note]"
 - 
 -       f :: Element -> Bool
 -       f e = not $ center e || noteLink e
 - 
 -       fEx :: Element -> Bool
 -       fEx e = not $  center e
 -                   || noteLink e
 -                   || Just "hiddennote" == M.lookup "class" (elementAttributes e)
 -}

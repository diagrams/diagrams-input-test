module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.Char
import qualified Data.Text as T
import Diagrams.Backend.SVG
import Diagrams.Core.Types
import Diagrams.Core.Compile
import Diagrams.Prelude
import Diagrams.SVG.ReadSVG
import Diagrams.TwoD.Input
import Diagrams.TwoD.Size (dims2D)
import Filesystem.Path.CurrentOS (decodeString)
import Lucid.Svg
import Prelude hiding (FilePath)
import System.Directory
import System.FilePath

main = do
  resourceFiles <- getDirectoryContents "."
  let svgs = filter (\xs -> xs /= "."  && 
                            xs /= ".." && 
                            ( (lastN 4 xs) == ".svg" || (lastN 4 xs) == ".png" || (lastN 4 xs) == ".jpg" )) resourceFiles
  putStr $ show svgs
  images <- mapM loadImageEmbedded svgs
  let files = map ("out-" ++) svgs
  zipWithM_ (\f i -> renderSVG1 f (dims2D 400 400) i) files (map img images)

img :: Either String (Diagram SVG) -> Diagram SVG
img im = case im of Left err -> mempty
                    Right i -> i

-- renderSVG1 :: SVGFloat n => FilePath -> SizeSpec V2 n -> QDiagram SVG V2 n Any -> IO ()
renderSVG1 outFile spec = renderSVG1' outFile (SVGOptions spec Nothing (mkPrefix outFile))

-- renderSVG1' :: SVGFloat n => FilePath -> Options SVG V2 n -> QDiagram SVG V2 n Any -> IO ()
renderSVG1' outFile opts
  = BS.writeFile outFile
  . renderBS
  . renderDia1 SVG opts

renderDia1 b opts d = snd (renderDiaT1 b opts d)

renderDiaT1 b opts d = (g2o, renderRTree b opts' . showS . toRTree g2o $ d')
  where (opts', g2o, d') = adjustDia b opts d

showS :: RTree b v n Annotation -> RTree b v n Annotation
showS rtree = rtree -- Debug.Trace.trace (show rtree) rtree

mkPrefix :: FilePath -> T.Text
mkPrefix = T.filter isAlpha . T.pack . takeBaseName

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

instance Show (RNode b v n a) where
  show (RStyle _) = "rstyle"
  show (RAnnot _) = "RAnnot"
  show (RPrim _) = "RPrim"
  show (REmpty) = "REmpty"

module Main (main) where
import Maxwell
import Diagrams.Backend.SVG.CmdLine (mainWith)

main :: IO ()
main = mainWith myPicture

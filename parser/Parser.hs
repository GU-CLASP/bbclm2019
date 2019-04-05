module Main (main) where

import Data.Char
import Data.List
import Data.Foldable
import System.Environment
import Data.List.Split
import System.Process
import ParseLib


fixCaseWord :: [Char] -> [Char]
fixCaseWord "If" = "if_"
fixCaseWord ('(':cs) = '(':fixCaseWord cs
fixCaseWord (c:cs) = toLower c:cs
fixCaseWord [] = error "empty word"

fixCaseLine :: String -> String
fixCaseLine = unwords . map fixCaseWord . words

expressionWords :: String -> [String]
expressionWords = nub . words . filter (not . (`elem` "()"))

-- >>> expressionWords "(s1 (qNP every violinist) (bare (isA musician)))"
-- ["s1","qNP","every","violinist","bare","isA","musician"]

dropSuff :: Int -> [a] -> [a]
dropSuff n = reverse . drop n . reverse

-- | Extract vocabulary of a certain category from the GF abstract syntax
extractVocab :: String -> String -> [String]
extractVocab abstractSyntax category = map fixCaseWord ws
  where ls = filter ((": " ++ category ++ ";") `isSuffixOf`) $ lines abstractSyntax
        ws = concatMap (words . filter (not . (`elem` ":,")) . dropSuff (length category + 2)) ls

allVocabulary :: String -> ([String],[String],[String],[String])
allVocabulary abstractSyntax = ((f "CN" ++ f "VP") \\ ["person"], f "NP", f "A", f "Unit")
  where f = extractVocab abstractSyntax

-- >>> allVocabulary <$> readFile "PNLS.gf"
-- (["linguist","baldMan","toupeeWearer","person","violinist","musician","neuralNetwork","basketballPlayer","cricketPlayer","baseballPlayer","stonesFan","interLogicStudent","introLogicStudent","advanLogicStudent","jazzGuitarist","guitarist","rockGuitarist","turkishCoffeeDrinker","homeRun","prologProgrammer","physicist","logician","conservative","knowFLT","dislikeExperimentalWork","listenToOudMusic","enjoyArak","playInMF","playForTheLeafs","preferTheDoorsToTheBeatles","playTheStones","tryHairTransplantTreatment","readMusic","hitRun","canProveFOL","useTailRec","eatHumus","enjoyTabouli","insistOnMintTeaWithFood","supportFreeUniversityEducation"],["mary","john","sam","molly"])

renderProblem :: ([String],[String],[String],[String]) -> [String] -> [String]
renderProblem _vocab@(preds,inds,adjs,units) (parsed@(_:_)) =
  ("problem = do") : map ("  "++)
   ([x ++ " <- " ++ alloc
    | (vs,alloc) <- [(preds,"newPred"),(inds,"PN <$> newInd"),(adjs,"newMeasure"),(units,"newUnit")],
      x <- intersect vs ws]
    ++ ["canPlayChoords <- mkCanPlayChoords" | "canPlayChoords" `elem` ws]
    ++ ["observe $ " ++ premiss | premiss <- ps]
    ++ ["return $ " ++ h]
   )
  where ws :: [String]
        ws = concatMap expressionWords parsed -- all (abstract) words in the problem
        h = last parsed
        ps = init parsed

doit :: FilePath -> FilePath -> IO ()
doit suitepath parserPath = do
  vocab <- allVocabulary <$> readFile (parserPath ++ "PNLS.gf")
  rawInput <-  readFile (suitepath ++ "tests.org")
  let problems :: [[String]]
      problems = (filter (not . null) . splitWhen null . preParse) rawInput
  gfo <- readProcess "gf" [parserPath ++ "PNLSEng.gf"] (unlines ["p " ++ show l | l <- concat problems])
  let parsed = (map fixCaseLine . takeWhile (/="See you.") . map (drop 6) . filter ("PNLS>" `isPrefixOf`) $ lines gfo)
      parsedProblems :: [[String]]
      parsedProblems = filter (not . null) $ splitPlaces (map length problems) parsed -- split the output back into problems
      readyProblems = zip3 problems parsedProblems [(1::Int)..]
  forM_ readyProblems $ \(input,prob,n) -> do
    writeFile (suitepath ++ "Problem" ++ show n ++ ".hs") $ unlines $
      ["import Prelude hiding (all,and,not,or)"
      ,"import Lib2"] ++
      renderProblem vocab prob ++
      ["main = do"
      ,"  mapM putStrLn " ++ show input
      ,"  run problem"]
  writeFile (suitepath ++ "runAll") $ unlines $
    concat [["echo Problem number " ++ show n
            ,"./runmodel Problem" ++ show n ++ ".hs"]
           | n <- [1..length readyProblems]]

main :: IO ()
main = do
  [inp,pp] <- getArgs
  doit inp pp

-- >>> doit "../testsuite/" "./"

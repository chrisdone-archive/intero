{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Test that various commands work properly.

module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad (forM_)
import Data.Char
import System.IO
import System.IO.Temp
import System.FilePath ((</>))
import System.Process
import System.Info (os)
import Test.Hspec
import Text.Regex

-- | Main entry point.
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
-- Test suite specification

-- | Test suite.
spec :: Spec
spec = do
  argsparser
  basics
  load
  types
  alltypes
  use
  definition
  bytecode
  completion
  it "Support GHC 8.2 better" pending

-- | Argument parsing should be user-friendly.
argsparser :: Spec
argsparser =
  describe
    "Arguments parser"
    (do issue
          ":type-at \"Foo Bar.hs\" 1 1 1 1"
          "https://github.com/commercialhaskell/intero/issues/25"
          (atFile
             ":type-at"
             "Foo Bar.hs"
             "x = 'a'"
             (1, 1, 1, 1, "x")
             id
             "x :: Char\n")
        issue
          ":type-at"
          "https://github.com/commercialhaskell/intero/issues/28"
          (eval
             ":type-at"
             "\n<no location info>: Expected a span: \"<module-name/filepath>\" <start line> <start column> <end line> <end column> \"<sample string>\"\n"))

-- | Basic commands that should work out of the box.
basics :: Spec
basics =
  describe
    "Basics"
    (do it ":t 1" (eval ":t 1 :: Num a => a" "1 :: Num a => a :: Num a => a\n")
        it
          ":i Nothing"
          (do reply <- withIntero [] (\_ repl -> repl ":i Nothing")
              shouldBe
                (subRegex (mkRegex "Data.Maybe") reply "GHC.Base")
                ("data Maybe a = Nothing | ... \t-- Defined in " ++ (quote "GHC.Base") ++ "\n"))
        it ":k Just" (eval ":k Maybe" "Maybe :: * -> *\n"))
  where
    quote s = opQuote : s ++ [clQuote]
    opQuote = case os of
        "mingw32" -> '`'
        _ -> '‘'
    clQuote = case os of
        "mingw32" -> '\''
        _ -> '’'

-- | Loading files and seeing the results.
load :: Spec
load =
  describe
    "Load"
    (do it
          ":l X.hs"
          (do result <-
                withIntero
                  []
                  (\dir repl -> do
                     writeFile (dir ++ "/X.hs") "x = 'a'"
                     repl (":l X.hs"))
              shouldBe
                result
                (unlines
                   [ "[1 of 1] Compiling Main             ( X.hs, interpreted )"
                   , "Ok, modules loaded: Main."
                   , "Collecting type info for 1 module(s) ... "]))
        it
          ":l X.hs; :extensions X"
          (do result <-
                withIntero
                  []
                  (\dir repl -> do
                     writeFile (dir ++ "/X.hs") "{-# LANGUAGE ScopedTypeVariables #-}\nmodule X where\nx = 'a'"
                     _ <- repl (":l X.hs")
                     repl (":extensions X"))
              shouldBe
                (filter (== "ScopedTypeVariables") (words result))
                ["ScopedTypeVariables"])
        it
          ":l NonExistent.hs"
          (do result <- withIntero [] (\_ repl -> repl (":l NonExistent.hs"))
              shouldBe
                (stripError result)
                (unlines
                   [ "Failed, modules loaded: none."
                   , ""
                   , "<no location info>: can't find file: NonExistent.hs"])))
  where
    stripError = \i -> subRegex (mkRegex "error: ") i ""

-- | Check things when in -fbyte-code mode.
bytecode :: Spec
bytecode =
  describe
    "Bytecode"
    (do it
          ":set -fobject-code ; :l X.hs; :set -byte-code; :l X.hs"
          (do result <-
                withIntero
                  []
                  (\dir repl -> do
                     _ <- repl (":set -fobject-code")
                     writeFile (dir ++ "/X.hs") "x = 'a'"
                     _ <- repl (":l X.hs")
                     _ <- repl (":set -fbyte-code")
                     writeFile (dir ++ "/X.hs") "x = 123"
                     repl (":l X.hs"))
              shouldBe
                (unlines (reverse (take 2 (reverse (lines result)))))
                (unlines
                   [ "Ok, modules loaded: Main."
                   , "Collecting type info for 1 module(s) ... "])))

-- | Get type information of file contents.
types :: Spec
types =
  describe
    "Types"
    (do it
          ":type-at X.hs 1 1 1 1 x -- Char"
          (typeAt "x = 'a'" (1, 1, 1, 1, "x") "x :: Char\n")
        it
          ":type-at X.hs 1 1 1 1 -- Char (string omitted)"
          (typeAt "x = 'a'" (1, 1, 1, 1, "") " :: Char\n")
        it
          ":type-at X.hs 1 1 1 1 x -- [Char]"
          (typeAt "x = 'a' : x" (1, 1, 1, 1, "x") "x :: [Char]\n")
        it
          ":type-at X.hs 1 11 1 12 x -- [Char]"
          (typeAt "x = 'a' : x" (1, 11, 1, 12, "x") "x :: [Char]\n")
        it
          ":type-at X.hs 1 11 1 12 y -- [Char] (internal variable)"
          (typeAt "x = 'a' : y where y = x" (1, 11, 1, 12, "y") "y :: [Char]\n")
        issue
          ":type-at X.hs 1 1 1 1 f -- Num a => a"
          "https://github.com/commercialhaskell/intero/issues/14"
          (typeAt "f x = x * 2" (1, 1, 1, 2, "f") "f :: Num a => a -> a\n")
        issue
          ":type-at X.hs 1 1 1 1 x -- Char (oddly bounded selection)"
          "https://github.com/commercialhaskell/intero/issues/29"
          (typeAt "foo = 'a'" (1, 1, 1, 1, "f") "f :: Char\n")
        issue
          ":type-at half of 2 arguments within function call"
          "https://github.com/commercialhaskell/intero/issues/29"
          (typeAt testFile (1, 29, 1, 32, "\" \"") "\" \" :: [Char] -> [Char]\n")
        issue
          ":type-at funtion + half of its first argument"
          "https://github.com/commercialhaskell/intero/issues/29"
          (typeAt
             testFile
             (1, 18, 1, 28, "concat3 \"a")
             "concat3 \"a :: [Char] -> [Char] -> [Char]\n")
        issue
          ":type-at 2 arguments within a function call"
          "https://github.com/commercialhaskell/intero/issues/29"
          (typeAt
             testFile
             (1, 26, 1, 35, "\"aa\" \"bb\"")
             "\"aa\" \"bb\" :: [Char] -> [Char]\n")
        issue
          ":type-at 2 lines within a do bloc"
          "https://github.com/commercialhaskell/intero/issues/29"
          (typeAt
             testFile
             (4, 8, 5, 10, "{{multiline}}")
             "{{multiline}} :: IO ()\n")
        issue
          ":type-at part of a line within a do bloc (1)"
          "https://github.com/commercialhaskell/intero/issues/29"
          (typeAt testFile (4, 8, 4, 10, " 1") " 1 :: IO ()\n")
        issue
          ":type-at part of a line within a do block (2)"
          "https://github.com/commercialhaskell/intero/issues/29"
          (typeAt testFile (4, 9, 4, 10, "1") "1 :: Integer\n")
        issue
          ":type-at with operator resolution"
          "https://github.com/commercialhaskell/intero/issues/47"
          (typeAt
             (unlines
                [ "data X = X"
                , "instance Show X where"
                , "  show _ = show (1::Int,())"
                , "p s = id s * s == id s * s"])
             (3, 18, 3, 19, "1")
             "1 :: Int\n"))
  where
    testFile :: String
    testFile =
      unlines
        [ "test = putStrLn (concat3 \"aa\" \"bb\" \"cc\")"
        , "concat3 a b c = a ++ b ++ c"
        , "foo = do"
        , "  print 1"
        , "  print 2"
        , "  print 3"
        , ""]

-- | List all types in all modules loaded.
alltypes :: Spec
alltypes =
  describe
    "All Types"
    (do it
          ":all-types"
          (do result <-
                withIntero
                  []
                  (\dir repl -> do
                     writeFile (dir ++ "/X.hs") "x = 123\ny = show 'c'"
                     _ <- repl (":l X.hs")
                     repl ":all-types")
              shouldBe
                result
                (unlines
                   [ "X.hs:(2,1)-(2,2): String"
                   , "X.hs:(1,1)-(1,2): Integer"
                   , "X.hs:(2,5)-(2,9): Char -> String"
                   , "X.hs:(2,10)-(2,13): Char"
                   , "X.hs:(2,5)-(2,13): String"
                   , "X.hs:(1,5)-(1,8): Integer"])))

-- | Are we on ghc8_2 or above?
ghc8_2 :: Bool
#if __GLASGOW_HASKELL__ >= 802
ghc8_2 = True
#else
ghc8_2 = False
#endif

-- | Find uses of a variable.
use :: Spec
use =
  describe
    "Uses"
    (do it
          ":uses X.hs 1 1 1 1 x -- from definition site"
          (uses
             "x = 'a' : x"
             (1, 1, 1, 1, "x")
             id
             (unlines
                ["X.hs:(1,1)-(1,2)", "X.hs:(1,1)-(1,2)", "X.hs:(1,11)-(1,12)"]))
        it
          ":uses X.hs 1 11 1 12 x -- from use site"
          (uses
             "x = 'a' : x"
             (1, 11, 1, 12, "x")
             id
             (if ghc8_2
                 then unlines ["X.hs:(1,1)-(1,2)","X.hs:(1,1)-(1,2)","X.hs:(1,11)-(1,12)"]
                 else unlines ["X.hs:(1,1)-(1,2)", "X.hs:(1,11)-(1,12)"]))
        it
          ":uses X.hs 1 5 1 6 id -- package definition"
          (uses
             "x = id"
             (1, 5, 1, 6, "id")
             (\i -> subRegex (mkRegex "-[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+") i "")
             (unlines ["base:GHC.Base"]))
        it
          ":uses X.hs 1 5 1 6 id -- shadowed package definition"
          (uses
             "x = id where id = ()"
             (1, 5, 1, 7, "id")
             id
             (unlines
                ["X.hs:(1,14)-(1,16)", "X.hs:(1,14)-(1,16)", "X.hs:(1,5)-(1,7)"]))
        issue
          ":uses on type constructor (in data decl)"
          "https://github.com/commercialhaskell/intero/issues/3"
          (uses
             (unlines ["data X = X", "foo :: X -> X", "foo x = X"])
             (1, 6, 1, 7, "X")
             lines
             ["X.hs:(1,1)-(1,11)"])
        issue
          ":uses on type constructor (in sig)"
          "https://github.com/commercialhaskell/intero/issues/3"
          (uses
             (unlines ["data X = X", "foo :: X -> X", "foo x = X"])
             (2, 8, 2, 9, "X")
             lines
             ["X.hs:(1,1)-(1,11)"])
        issue
          ":uses on data constructor (in expression)"
          "https://github.com/commercialhaskell/intero/issues/3"
          (uses
             (unlines ["data X = X", "foo :: X -> X", "foo x = X"])
             (3, 9, 3, 10, "X")
             lines
             (if ghc8_2
                 then ["X.hs:(1,1)-(1,11)"]
                 else ["X.hs:(1,10)-(1,11)", "X.hs:(3,9)-(3,10)"])))

-- | Find loc-ats of a variable.
definition :: Spec
definition =
  describe
    "Definition location"
    (do it
          "From definition site"
          (locAt "x = 'a' : x" (1, 1, 1, 1, "x") (unlines ["X.hs:(1,1)-(1,2)"]))
        it
          "From use site"
          (locAt
             "x = 'a' : x"
             (1, 11, 1, 12, "x")
             (unlines (if ghc8_2
                          then ["X.hs:(1,1)-(1,2)"]
                          else ["X.hs:(1,1)-(1,12)"])))
        it
          "To function argument"
          (locAt
             "f x = 'a' : x"
             (1, 13, 1, 14, "x")
             (unlines ["X.hs:(1,3)-(1,4)"]))
        it
          "To pattern match"
          (locAt
             "f (Just x) = 'a' : x"
             (1, 20, 1, 21, "x")
             (unlines ["X.hs:(1,9)-(1,10)"]))
        it
          "To other module"
          (locAtMultiple
             [("X.hs", "import Y"), ("Y.hs", "module Y where")]
             (1, 8, 1, 9, "Y")
             (unlines ["." </> "Y.hs:(1,8)-(1,9)"]))
        issue
          "To unexported thing"
          "https://github.com/commercialhaskell/intero/issues/98"
          (locAt
             (unlines
                [ "module X () where"
                , "data MyType = MyCons"
                , "t :: MyType"
                , "t = MyCons :: MyType"
                ])
             (3, 6, 3, 12, "MyType")
             (unlines ["X.hs:(2,1)-(2,21)"])))

-- | Test interactive completions.
completion :: Spec
completion = do
  describe
    "Completion in REPL"
    (do issue
          ":complete repl \"put\""
          "https://github.com/commercialhaskell/intero/issues/34"
          (eval
             ":complete repl \"put\""
             (unlines ["3 3 \"\"", "\"putChar\"", "\"putStr\"", "\"putStrLn\""]))
        issue
          ":complete repl \"sor\""
          "https://github.com/commercialhaskell/intero/issues/34"
          (do reply <-
                withIntero
                  []
                  (\_ repl -> do
                     let req = ":complete repl \"sor\""
                     reply <- repl req
                     if reply == "0 0 \"\"\n"
                       then do
                         _ <- repl "import Data.List"
                         repl req
                       else return ("First step failed: " ++ reply))
              shouldBe
                (filter (/= "\"sortOn\"") (drop 1 (lines reply)))
                (["\"sort\"", "\"sortBy\""])))
  describe
    "Completion in module context"
    (do it
          ":complete-at for put*"
          (atFile
             ":complete-at"
             "X.hs"
             "module X () where\nx = undefined"
             (4, 5, 0, 0, "put")
             lines
             ["putChar", "putStr", "putStrLn"])
        it
          ":complete-at for locally imported"
          (atFile
             ":complete-at"
             "X.hs"
             "module X () where\nimport Data.List\nx = undefined"
             (3, 5, 0, 0, "sor")
             (take 2 . lines)
             ["sort", "sortBy"])
        it
          ":complete-at for module-locally defined"
          (atFile
             ":complete-at"
             "X.hs"
             "module X () where\nx = undefined\nmodlocal = ()"
             (2, 5, 0, 0, "modl")
             lines
             ["modlocal"])
        it
          ":complete-at for definition-locally defined"
          (atFile
             ":complete-at"
             "X.hs"
             "module X () where\nx = undefined where locally = let p = 123 in p"
             (2, 5, 0, 0, "loc")
             lines
             ["locally"]))

--------------------------------------------------------------------------------
-- Combinators for running and interacting with intero

-- | Find the definition for the thing at point.
locAtMultiple :: [(String, String)] -> (Int, Int, Int, Int, String) -> String -> Expectation
locAtMultiple files (line,col,line',col',name) expected = do
  result <-
    withIntero
      []
      (\dir repl -> do
         forM_ files $ \(fileName, fileContents) ->
           writeFile (dir ++ "/" ++ fileName) fileContents
         _ <- repl (":l " ++ fst (head files))
         repl
           (":loc-at " ++ fst (head files) ++ " " ++
            unwords (map show [line, col, line', col']) ++ " " ++ name))
  shouldBe result expected
  let x = return ()
  x

locAt :: String -> (Int, Int, Int, Int, String) -> String -> Expectation
locAt file = locAtMultiple [("X.hs", file)]

-- | Find use-sites for the given place.
uses
  :: (Eq a, Show a)
  => String -> (Int, Int, Int, Int, String) -> (String -> a) -> a -> Expectation
uses file (line,col,line',col',name) preprocess expected = do
  result <-
    withIntero
      []
      (\dir repl -> do
         writeFile (dir ++ "/X.hs") file
         _ <- repl (":l X.hs")
         repl
           (":uses X.hs " ++
            unwords (map show [line, col, line', col']) ++ " " ++ name))
  shouldBe (preprocess result) expected

-- | Test the type at the given place.
typeAt :: String -> (Int, Int, Int, Int, String) -> String -> Expectation
typeAt a b c = do
  atFile ":type-at" "X.hs" a b id c

-- | Test the type at the given place (with the given filename).
atFile
  :: (Eq a, Show a)
  => String
  -> String
  -> String
  -> (Int, Int, Int, Int, String)
  -> (String -> a)
  -> a
  -> Expectation
atFile cmd fname file (line,col,line',col',name) preprocess expected = do
  result <-
    withIntero
      []
      (\dir repl -> do
         writeFile (dir ++ "/" ++ fname) file
         _ <- repl (":l " ++ show fname)
         repl
           (cmd ++
            " " ++
            (if any isSpace fname
               then show fname
               else fname) ++
            " " ++
            unwords (map show [line, col, line', col']) ++
            (if null name
               then ""
               else " " ++ show name)))
  shouldBe (preprocess result) expected

-- | Make a quick interaction with intero.
eval :: String -- ^ Input.
         -> String -- ^ Expected output.
         -> Expectation
eval send recv = do
  reply <- withIntero [] (\_ repl -> repl send)
  shouldBe reply recv

-- | Launch an interactive intero process. Creates a temporary
-- directory in which the computation can work.
withIntero
  :: MonadIO m
  => [String] -> (FilePath -> (String -> IO String) -> IO a) -> m a
withIntero arguments cont =
  liftIO
    (withSystemTempDirectory
       "withIntero"
       (\dir -> do
          (inp,out,err,pid) <-
            catch
              (runInteractiveProcess
                 "intero"
                 ("-ignore-dot-ghci" : arguments)
                 (Just dir)
                 Nothing)
              (\(_ :: IOException) -> error "Couldn't launch intero process.")
          hSetBuffering inp NoBuffering
          hSetBuffering out NoBuffering
          hSetBuffering err NoBuffering
          let repl instr = do
                catch
                  (do hPutStrLn inp instr
                      let getReply = do
                            mc <-
                              catch
                                (fmap Just (hGetChar out))
                                (\(_ :: IOException) -> return Nothing)
                            case mc of
                              Nothing -> hGetAvailable err
                              Just '\4' -> hGetAvailable err
                              Just c -> do
                                cs <- getReply
                                return (c : cs)
                      getReply)
                  (\(_ :: IOException) -> return "")
          _ <- repl ":set prompt \"\\4\""
          finally
            (cont dir repl)
            (do ignored (hClose inp)
                ignored (hClose out)
                ignored (hClose err)
                ignored (terminateProcess pid))))
  where
    ignored m = catch m (\(_ :: IOException) -> return ())
    hGetAvailable h = do
      available <- catch (hReady h) (\(_ :: IOException) -> return False)
      if available
        then catch
               (do c <- hGetChar h
                   cs <- hGetAvailable h
                   return (c : cs))
               (\(_ :: IOException) -> return [])
        else return []

--------------------------------------------------------------------------------
-- Spec combinators

-- | Specify an issue that needs to be regression tested.
issue
  :: Example a
  => String -> t -> a -> SpecWith (Arg a)
issue label _link expectation = it label expectation

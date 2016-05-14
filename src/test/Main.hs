{-# LANGUAGE ScopedTypeVariables #-}
-- | Test that various commands work properly.

module Main where

import Control.Exception
import Control.Monad.IO.Class
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec

-- | Main entry point.
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
-- Test suite specification

-- | Test suite.
spec :: Spec
spec =
  do basics
     load
     types
     alltypes
     use
     definition
     bytecode

-- | Basic commands that should work out of the box.
basics :: Spec
basics =
  describe "Basics"
           (do it ":t 1" (eval ":t 1" "1 :: Num a => a\n")
               it ":i Nothing"
                  (eval ":i Nothing" "data Maybe a = Nothing | ... \t-- Defined in ‘GHC.Base’\n")
               it ":k Just" (eval ":k Maybe" "Maybe :: * -> *\n"))

-- | Loading files and seeing the results.
load :: Spec
load =
  describe "Load"
           (do it ":l X.hs"
                  (do result <-
                        withIntero
                          []
                          (\dir repl ->
                             do writeFile (dir ++ "/X.hs") "x = 'a'"
                                repl (":l X.hs"))
                      shouldBe result
                               (unlines ["[1 of 1] Compiling Main             ( X.hs, interpreted )"
                                        ,"Ok, modules loaded: Main."
                                        ,"Collecting type info for 1 module(s) ... "]))
               it ":l NonExistent.hs"
                  (do result <-
                        withIntero []
                                   (\_ repl -> repl (":l NonExistent.hs"))
                      shouldBe result (unlines ["Failed, modules loaded: none."
                                               ,""
                                               ,"<no location info>: can't find file: NonExistent.hs"])))

-- | Check things when in -fbyte-code mode.
bytecode :: Spec
bytecode =
  describe "Bytecode"
           (do it ":set -fobject-code ; :l X.hs; :set -byte-code; :l X.hs"
                  (do result <-
                        withIntero
                          []
                          (\dir repl ->
                             do _ <- repl (":set -fobject-code")
                                writeFile (dir ++ "/X.hs") "x = 'a'"
                                _ <- repl (":l X.hs")
                                _ <- repl (":set -fbyte-code")
                                writeFile (dir ++ "/X.hs") "x = 123"
                                repl (":l X.hs"))
                      shouldBe (unlines (reverse (take 2 (reverse (lines result)))))
                               (unlines ["Ok, modules loaded: Main."
                                        ,"Collecting type info for 1 module(s) ... "])))

-- | Get type information of file contents.
types :: Spec
types =
  describe "Types"
           (do it ":type-at X.hs 1 1 1 1 x -- Char"
                  (typeAt "x = 'a'" (1,1,1,1,"x") "x :: Char\n")
               it ":type-at X.hs 1 1 1 1 x -- [Char]"
                  (typeAt "x = 'a' : x" (1,1,1,1,"x") "x :: [Char]\n")
               it ":type-at X.hs 1 11 1 12 x -- [Char]"
                  (typeAt "x = 'a' : x" (1,11,1,12,"x") "x :: [Char]\n")
               it ":type-at X.hs 1 11 1 12 y -- [Char] (internal variable)"
                  (typeAt "x = 'a' : y where y = x" (1,11,1,12,"y") "y :: [Char]\n")
               issue ":type-at X.hs 1 1 1 1 f -- Num a => a"
                     "https://github.com/chrisdone/intero/issues/14"
                     (typeAt "f x = x * 2" (1,1,1,2,"f") "f :: Num a => a -> a\n")
               issue ":type-at X.hs 1 1 1 1 x -- Char (oddly bounded selection)"
                     "https://github.com/chrisdone/intero/issues/29"
                     (const (pendingWith "TODO")
                            (typeAt "foo = 'a'" (1,1,1,1,"f") "f :: Char\n")))

-- | List all types in all modules loaded.
alltypes :: Spec
alltypes =
  describe "All Types"
           (do it ":all-types"
                  (do result <-
                        withIntero
                          []
                          (\dir repl ->
                             do writeFile (dir ++ "/X.hs") "x = 123\ny = show 'c'"
                                _ <- repl (":l X.hs")
                                repl ":all-types")
                      shouldBe result
                               (unlines ["X.hs:(2,1)-(2,2): String"
                                        ,"X.hs:(1,1)-(1,2): Integer"
                                        ,"X.hs:(2,5)-(2,9): Char -> String"
                                        ,"X.hs:(2,10)-(2,13): Char"
                                        ,"X.hs:(2,5)-(2,13): String"
                                        ,"X.hs:(1,5)-(1,8): Integer"])))

-- | Find uses of a variable.
use :: Spec
use =
  describe "Uses"
           (do it ":uses X.hs 1 1 1 1 x -- from definition site"
                  (uses "x = 'a' : x"
                        (1,1,1,1,"x")
                        (unlines ["X.hs:(1,1)-(1,2)"
                                 ,"X.hs:(1,1)-(1,2)"
                                 ,"X.hs:(1,11)-(1,12)"]))
               it ":uses X.hs 1 11 1 12 x -- from use site"
                  (uses "x = 'a' : x"
                        (1,11,1,12,"x")
                        (unlines ["X.hs:(1,1)-(1,2)","X.hs:(1,11)-(1,12)"]))
               it ":uses X.hs 1 5 1 6 id -- package definition"
                  (uses "x = id"
                        (1,5,1,6,"id")
                        (unlines ["base-4.8.2.0:GHC.Base"]))
               it ":uses X.hs 1 5 1 6 id -- shadowed package definition"
                  (uses "x = id where id = ()"
                        (1,5,1,7,"id")
                        (unlines ["X.hs:(1,14)-(1,16)"
                                 ,"X.hs:(1,14)-(1,16)"
                                 ,"X.hs:(1,5)-(1,7)"])))

-- | Find loc-ats of a variable.
definition :: Spec
definition =
  describe "Definition location"
           (do it ":loc-at X.hs 1 1 1 1 x -- from definition site"
                  (locAt "x = 'a' : x"
                         (1,1,1,1,"x")
                         (unlines ["X.hs:(1,1)-(1,2)"]))
               it ":loc-at X.hs 1 11 1 12 x -- from use site"
                  (locAt "x = 'a' : x"
                         (1,11,1,12,"x")
                         (unlines ["X.hs:(1,1)-(1,12)"]))
               it ":loc-at X.hs 1 11 1 12 x -- to function argument"
                  (locAt "f x = 'a' : x"
                         (1,13,1,14,"x")
                         (unlines ["X.hs:(1,3)-(1,4)"]))
               it ":loc-at X.hs 1 11 1 12 x -- to pattern match"
                  (locAt "f (Just x) = 'a' : x"
                         (1,20,1,21,"x")
                         (unlines ["X.hs:(1,9)-(1,10)"])))

--------------------------------------------------------------------------------
-- Combinators for running and interacting with intero

-- | Find the definition for the thing at point.
locAt
  :: String -> (Int,Int,Int,Int,String) -> String -> Expectation
locAt file (line,col,line',col',name) expected =
  do result <-
       withIntero
         []
         (\dir repl ->
            do writeFile (dir ++ "/X.hs") file
               _ <- repl (":l X.hs")
               repl (":loc-at X.hs " ++
                     unwords (map show [line,col,line',col']) ++ " " ++ name))
     shouldBe result expected

-- | Find use-sites for the given place.
uses
  :: String -> (Int,Int,Int,Int,String) -> String -> Expectation
uses file (line,col,line',col',name) expected =
  do result <-
       withIntero
         []
         (\dir repl ->
            do writeFile (dir ++ "/X.hs") file
               _ <- repl (":l X.hs")
               repl (":uses X.hs " ++
                     unwords (map show [line,col,line',col']) ++ " " ++ name))
     shouldBe result expected

-- | Test the type at the given place.
typeAt
  :: String -> (Int,Int,Int,Int,String) -> String -> Expectation
typeAt file (line,col,line',col',name) expected =
  do result <-
       withIntero
         []
         (\dir repl ->
            do writeFile (dir ++ "/X.hs") file
               _ <- repl (":l X.hs")
               repl (":type-at X.hs " ++
                     unwords (map show [line,col,line',col']) ++ " " ++ name))
     shouldBe result expected

-- | Make a quick interaction with intero.
eval :: String -- ^ Input.
         -> String -- ^ Expected output.
         -> Expectation
eval send recv =
  do reply <-
       withIntero []
                  (\_ repl -> repl send)
     shouldBe reply recv

-- | Launch an interactive intero process. Creates a temporary
-- directory in which the computation can work.
withIntero :: MonadIO m => [String] -> (FilePath -> (String -> IO String) -> IO a) -> m a
withIntero arguments cont =
  liftIO (withSystemTempDirectory
            "withIntero"
            (\dir ->
               do (inp,out,err,pid) <-
                    catch (runInteractiveProcess
                             "intero"
                             ("-ignore-dot-ghci" : arguments)
                             (Just dir)
                             Nothing)
                          (\(_ :: IOException) ->
                             error "Couldn't launch intero process.")
                  hSetBuffering inp NoBuffering
                  hSetBuffering out NoBuffering
                  hSetBuffering err NoBuffering
                  let repl instr =
                        do catch (do hPutStrLn inp instr
                                     let getReply =
                                           do mc <-
                                                catch (fmap Just (hGetChar out))
                                                      (\(_ :: IOException) ->
                                                         return Nothing)
                                              case mc of
                                                Nothing -> hGetAvailable err
                                                Just '\4' -> hGetAvailable err
                                                Just c ->
                                                  do cs <- getReply
                                                     return (c : cs)
                                     getReply)
                                 (\(_ :: IOException) -> return "")
                  _ <- repl ":set prompt \"\\4\""
                  finally (cont dir repl)
                          (do ignored (hClose inp)
                              ignored (hClose out)
                              ignored (hClose err)
                              ignored (terminateProcess pid))))
  where ignored m = catch m (\(_ :: IOException) -> return ())
        hGetAvailable h =
          do available <-
               catch (hReady h)
                     (\(_ :: IOException) -> return False)
             if available
                then catch (do c <- hGetChar h
                               cs <- hGetAvailable h
                               return (c : cs))
                           (\(_ :: IOException) -> return [])
                else return []

--------------------------------------------------------------------------------
-- Spec combinators

-- | Specify an issue that needs to be regression tested.
issue :: Example a => String -> t -> a -> SpecWith (Arg a)
issue label _link expectation = it label expectation

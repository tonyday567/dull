{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
 
import Protolude hiding (race, race_, runConcurrently, putStrLn)
import Box
import Game.Chess.UCI hiding (outputStrLn)
import Game.Chess
import qualified Streaming.Prelude as S
import Control.Concurrent.Classy.Async
import System.IO
import System.Process
import GHC.Base (String)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Control.Concurrent.STM.TChan

startE :: FilePath -> [GHC.Base.String] -> IO (Handle, Handle, ProcessHandle)
startE cmd args = do
  (Just inH, Just outH, Nothing, procH) <- createProcess (proc cmd args) {
      std_in = CreatePipe, std_out = CreatePipe
    }
  hSetBuffering inH LineBuffering
  pure (inH, outH, procH)

mkStockfishBox :: Engine -> Cont IO (Box STM Text Text)
mkStockfishBox e =
  Box <$>
  (Box.putLine (inH e) & commitPlug) <*>
  (Box.getLine (outH e) & emitPlug)

-- > e <- start'' putStrLn "stockfish" []
-- > let stockc = (Box.putLine (inH e) & commitPlug)
-- > let stocke = (Box.getLine (outH e) & emitPlug)
-- > with (emit . liftE <$> stocke) (print =<<)
-- Just "Stockfish 10 64 BMI2 by T. Romstad, M. Costalba, J. Kiiski, G. Linscott"
-- with (liftC <$> stockc) (\c -> void $ commit c "uci")

start'' :: (String -> IO ()) -> String -> [String] -> IO Engine
start'' outputStrLn cmd args = do
  (Just inH, Just outH, Nothing, procH) <- createProcess (proc cmd args) {
      std_in = CreatePipe, std_out = CreatePipe
    }
  hSetBuffering inH LineBuffering
  Engine inH outH procH outputStrLn Nothing Nothing Nothing HashMap.empty <$>
       newEmptyMVar <*> newIORef False <*>
       newBroadcastTChanIO <*> newBroadcastTChanIO <*>
       newIORef (startpos, [])

main :: IO ()
main = do
  (i,o,p) <- startE "/Users/tonyday/haskell/dull/other/stockfish-10-bmi2" []
  putStrLn ":engine started"
  let echoq = Transducer $ \s -> s & S.takeWhile (/="q") --  & S.map ("echo: " <>)
  let stockc = Box.putLine i & commitPlug
  let wireIn = Box <$> stockc <*> eStdin'
  let stocke = Box.getLine o & emitPlug
  let wireOut = Box <$> cStdout' <*> stocke
  let fileOut = commitLines "dull.log"
  putStrLn ":adding wires"
  Protolude.print =<< race
    (etc () echoq wireIn >> putStrLn ":wireIn terminated")
    (etc () echoq (wireOut <> (Box <$> fileOut <*> mempty)) >> putStrLn ":wireOut terminated")
  putStrLn ":wires terminated"
  terminateCarefully p

-- https://stackoverflow.com/questions/8820903/haskell-how-to-timeout-a-function-that-runs-an-external-command
-- https://stackoverflow.com/questions/29944344/timeout-for-parallel-running-command-calls-with-a-worker-pool-in-haskell

-- terminate external process on exception, ignore if already dead.
terminateCarefully :: ProcessHandle -> IO ()
terminateCarefully pHandle = 
    catch (terminateProcess pHandle) (\(_::IOException) -> return ())

safeExec :: CreateProcess -> IO ExitCode
safeExec cp =
    bracketOnError 
        (createProcess cp {std_out = CreatePipe})
        (\(_,_,_,pHandle) -> terminateCarefully pHandle)  
        (\(_,_,_,pHandle) -> do
            -- Workaround for a Windows issue.
            latch <- newEmptyMVar
            res <- race
               (do -- IO actions are uninterruptible on Windows :(
                  takeMVar latch 
                  waitForProcess pHandle
                  )
               -- Dummy interruptible action that   
               -- receives asynchronous exceptions first
               -- and helps to end the other action.
               (onException 
                   (do 
                      putMVar latch () 
                      -- runs forever unless interrupted
                      runConcurrently empty)
                   (terminateCarefully pHandle))
            pure $ either identity (const ExitSuccess) res)

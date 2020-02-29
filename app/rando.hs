{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

import Box
-- import Game.Chess

-- import qualified Data.HashMap.Strict as HashMap
-- import Data.IORef
-- import Control.Concurrent.STM.TChan

import Box.Control
import Control.Monad.Conc.Class as C
import qualified Data.Attoparsec.Text as A
import Data.Functor.Contravariant
import qualified Data.Text as Text
-- import Game.Chess.UCI hiding (outputStrLn)
import Protolude hiding (putStrLn, race, race_, runConcurrently)
import qualified Streaming.Prelude as S
import System.Process.Typed as T

cannedStockfish :: ProcessConfig Handle Handle ()
cannedStockfish =
  setStdin T.createPipe
    $ setStdout T.createPipe
    $ setStderr
      closed
      "/Users/tonyday/haskell/dull/other/stockfish-10-bmi2"

t1 :: IO ()
t1 =
  Box.with
    (Box <$> showStdout <*> readStdin)
    (controlBoxProcess defaultControlConfig cannedStockfish)

t2 :: IO ()
t2 =
  Box.with
    controlConsole
    (controlBoxProcess defaultControlConfig cannedStockfish)

-- | a canned emitter with delays
timedEmissions ::
  (MonadConc m) =>
  [(a, Double)] ->
  Cont m (Emitter (C.STM m) a)
timedEmissions xs = toEmit $ foldr (>>) (pure ()) $ (\(a, t) -> lift (sleep t) >> S.yield a) <$> xs

t3 :: [(Either ControlRequest Text, Double)] -> IO ()
t3 es =
  Box.with
    ( Box
        <$> ( contramap (either (("Response: " <>) . Text.pack . Prelude.show) id)
                <$> (cStdout 1000 :: Cont IO (Committer (C.STM IO) Text))
            )
        <*> timedEmissions es
    )
    (controlBoxProcess defaultControlConfig cannedStockfish)

t3' :: [(Either ControlRequest Text, Double)] -> IO ()
t3' es =
  Box.with
    ( Box
        <$> ( (contramap Protolude.show <$> commitLines "dull.log") <>
              (cmap (either (const (pure Nothing)) (pure . Just))
                <$> (cStdout 1000 :: Cont IO (Committer (C.STM IO) Text)))
            )
        <*> timedEmissions es
    )
    (controlBoxProcess defaultControlConfig cannedStockfish)

t4 :: IO ()
t4 = t3'
  [ (Left Start, 0.1)
  , (Right "uci", 0.1)
  , (Right "isready", 0.1)
  , (Right "ucinewgame", 0.1)
  , (Right "position startpos moves g2g4", 0.1)
  , (Right "go infinite", 0.1)
  , (Right "stop", 3)
  , (Left Quit, 1)
  ]

tLogger :: [(Either ControlRequest Text, Double)] -> IO ()
tLogger es =
  Box.with
    ( Box
        <$> ( (contramap Protolude.show <$> commitLines "dull.log") <>
              (cmap (either (const (pure Nothing)) (pure . Just))
                <$> (cStdout 1000 :: Cont IO (Committer (C.STM IO) Text)))
            )
        <*> 
        (fmap (either (Right . ("parse error: " <>)) id)
            . eParse (parseControlRequest A.takeText) <$> eStdin 1000)
    )
    (controlBoxProcess (ControlConfig 1 True Nothing False) cannedStockfish)

tLog :: IO ()
tLog = tLogger
  [ (Left Start, 0.1)
  , (Right "uci", 0.1)
  , (Right "isready", 0.1)
  , (Right "ucinewgame", 0.1)
  , (Right "position startpos moves g2g4", 0.1)
  , (Right "go infinite", 0.1)
  , (Right "stop", 3)
  , (Left Quit, 20)
  ]

main :: IO ()
main = tLogger [(Left Start, 0.1), (Left Quit, 20)]

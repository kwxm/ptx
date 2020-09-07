{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports  #-}
{-# OPTIONS -fplugin Language.PlutusTx.Plugin -fplugin-opt Language.PlutusTx.Plugin:defer-errors -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

module Main (main) where

import qualified Lib
    
import           Data.Char (isSpace)
import           Control.Monad
import           System.Environment

import qualified Language.PlutusCore.Pretty as PLC
import qualified Language.PlutusTx          as Tx
import           Language.PlutusTx.Prelude  as Tx hiding (init, last)

main :: IO ()
main = do
  let code = Tx.getPlc $ $$(Tx.compile [|| Lib.incrThing ||])    -- Simple example.
--  let code = Tx.getPlc $ $$(Tx.compile [|| Lib.deleteFirst ||])   -- More complicated example.
  print . PLC.prettyPlcClassicDebug $ code




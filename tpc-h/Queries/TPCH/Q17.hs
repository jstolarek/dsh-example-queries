{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
    
-- TPC-H Q17

module Queries.TPCH.Q17
    ( q17
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

q17 :: Q Double
q17 =
  let prices = [ l_extendedpriceQ l
               | l <- lineitems
    	       , p <- parts
               , p_partkeyQ p == l_partkeyQ l
	       , p_brandQ p == "Brand#23"
               , p_containerQ p == "MED BOX"
               , l_quantityQ l < 0.2 * avg [ l_quantityQ l2 
    			                   | l2 <- lineitems
  			                   , l_partkeyQ l2 == p_partkeyQ p
  			                   ]
               ]
  in sum prices / 7.0

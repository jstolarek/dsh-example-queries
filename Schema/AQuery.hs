{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Schema.AQuery where

import           Data.List.NonEmpty (NonEmpty ((:|)))

import           Database.DSH

data Trade = Trade
    { t_amount    :: Double
    , t_price     :: Double
    , t_tid       :: Integer
    , t_timestamp :: Integer
    , t_tradeDate :: Day
    }

deriveDSH ''Trade
deriveTA ''Trade
generateTableSelectors ''Trade

data Portfolio = Portfolio
    { po_pid         :: Integer
    , po_tid         :: Integer
    , po_tradedSince :: Integer
    }

deriveDSH ''Portfolio
deriveTA ''Portfolio
generateTableSelectors ''Portfolio

trades :: Q [Trade]
trades = table "trades"
               ("amount" :|
                [ "price"
                , "tid"
                , "ts"
                , "tradeDate"])
               t_tidQ
               (TableHints (pure $ Key ("tid" :| ["ts"])) NonEmpty NoProvenance)

portfolios :: Q [Portfolio]
portfolios = table "portfolio"
                   ("po_pid" :| ["po_tid", "po_tradedSince"])
                   po_pidQ
                   (TableHints (pure $ Key (pure "po_pid")) NonEmpty
                               NoProvenance)

data Packet = Packet
    { p_dest :: Integer
    , p_len  :: Integer
    , p_pid  :: Integer
    , p_src  :: Integer
    , p_ts   :: Integer
    }

deriveDSH ''Packet
deriveTA ''Packet
generateTableSelectors ''Packet

packets :: Q [Packet]
packets = table "packets"
                ("dst" :|
                 [ "len"
                 , "pid"
                 , "src"
                 , "ts"
                 ])
                p_pidQ
                (TableHints (pure $ Key (pure "pid")) NonEmpty NoProvenance)

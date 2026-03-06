{-# LANGUAGE GADTs #-}

module Plan.Monad where

data PlanM next where
    Read :: FilePath -> (PlanM next)
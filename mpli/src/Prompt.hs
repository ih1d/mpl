module Prompt where

import Control.Monad.State
import Syntax (Expr)

type History = [Expr]

newtype PromptM a = PromptM { runPromptM :: StateT History IO a }
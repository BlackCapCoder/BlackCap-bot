module Bot
        ( bot
        )
    where

import Vindinium
import BotHelper

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

bot :: Bot
bot = const $ return Stay


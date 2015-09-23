module BarthPar.Lens where


import           Control.Lens

import           BarthPar.Types


inputTopicId :: Lens' InputRow TopicId
inputTopicId = _1

inputWord :: Lens' InputRow Token
inputWord = _2

inputWeight :: Lens' InputRow Weight
inputWeight = _3

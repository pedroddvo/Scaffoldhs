module Error where

import Data.Data (Data, Typeable)
import Data.Text (Text)
import qualified Data.Text as T

data Pos = Pos
    { pos_start :: {-# UNPACK #-} !Int
    , pos_end :: {-# UNPACK #-} !Int
    }
    deriving (Data, Typeable, Eq)

instance Semigroup Pos where
    (Pos start1 end1) <> (Pos start2 end2) = 
        Pos (min start1 start2) (max end1 end2)

instance Show Pos where
    show (Pos start end) = show start ++ ".." ++ show end

data Error = Error
    { error_msg :: String
    , error_pos :: Pos
    }

errorFinish :: Text -> Error -> String
errorFinish src (Error msg (Pos start end)) =
    let (startOffset, startToEnd) = T.splitAt start src
        lineCount = T.count "\n" startOffset + 1
        lineStart = T.takeWhileEnd (/='\n') startOffset
        lineEnd = T.takeWhile (/='\n') startToEnd
        line = lineStart <> lineEnd
        lineStartPos = T.length startOffset - T.length lineStart
        padding = replicate (start - lineStartPos) ' '
        area = replicate (end - start) '~'
    in "error on line " ++ show lineCount ++ ": " ++ msg ++ "\n" ++ T.unpack line ++ "\n" ++ padding ++ area
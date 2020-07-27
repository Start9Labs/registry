module Lib.Types.FileSystem where

    import           Startlude

    data FileExistence = Existent | NonExistent
        deriving (Eq, Show)
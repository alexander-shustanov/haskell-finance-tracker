module Lib
    ( entryPoint
    ) where
import Server (runServer)



entryPoint :: IO ()
entryPoint = runServer

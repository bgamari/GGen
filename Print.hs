import System.Directory (doesFileExist)
import System.Posix.Signals (Handler(..), installHandler, keyboardSignal)
import System.Hardware.Serialport
import Data.Char (isAlpha, ord)
import Data.List (isPrefixOf)
import Control.Monad (liftM, forM_, when)
import Control.Monad.Trans (lift)
import Data.Maybe (mapMaybe)
import Text.Printf
import qualified Data.Text as T
import System.Environment (getArgs)
import Data.Bits
import System.Console.Haskeline

port = "/dev/ttyUSB0"
settings = defaultSerialSettings { commSpeed=CS115200
                                 , flowControl=Software 
                                 , timeout=50 }

type Cmd = T.Text
filterCmd :: Cmd -> Maybe Cmd
filterCmd c
        | not $ isAlpha (T.head c) = Nothing
        | otherwise = Just $ T.strip $ T.takeWhile (/=';') c

printFile :: SerialPort -> FilePath -> IO ()
printFile port file = 
        do cmds <- liftM (T.lines . T.pack) $ readFile file
           sendCmds port cmds

checksum :: Cmd -> Int
checksum s = 0xff .&. T.foldl' (\s c->s `xor` ord c) 0 s

appendChecksum :: Cmd -> Cmd
appendChecksum c = c `T.snoc` '*' `T.append` T.pack (show $ checksum c)

recvLine :: SerialPort -> IO String
recvLine port = 
        do c <- recvChar port
           case c of
                Nothing   -> error "Response timed out"
                Just '\n' -> return []
                Just c    -> do rest <- recvLine port
                                return (c:rest)

sendCmds :: SerialPort -> [Cmd] -> IO ()
sendCmds port cmds =
       let cmds' = mapMaybe filterCmd cmds
           ncmds = length cmds'
       in forM_ (zip [1..] cmds') $ \(n,c) ->
               do printf "%4d / %4d :  %s\n" (n::Integer) ncmds (T.unpack $ T.strip c)
                  case filterCmd c of
                       Just cmd -> do reply <- sendCmd port c
                                      when (not $ "OK" `isPrefixOf` reply) (error $ "Invalid response: "++reply)
                                      return ()
                       Nothing -> return ()
          
sendCmd :: SerialPort -> Cmd -> IO String
sendCmd port cmd =
        let cmd' = appendChecksum cmd
        in do sendString port (T.unpack cmd)
              sendChar port '\n'
              reply <- recvLine port
              return reply

magicCmd :: SerialPort -> String -> InputT IO ()
magicCmd port cmd
        | "print" `isPrefixOf` cmd =
                let path = drop 6 cmd
                in do exists <- lift $ doesFileExist path
                      if exists then lift $ printFile port path
                                else outputStrLn "File doesn't exist"

cmdLoop :: SerialPort -> IO ()
cmdLoop port = runInputT defaultSettings loop
        where loop :: InputT IO ()
              loop = do
                      minput <- getInputLine "> "
                      case minput of
                           Nothing         -> return ()
                           Just "quit"     -> return ()
                           Just "%quit"    -> return ()
                           Just ('%':cmd)  -> do magicCmd port cmd
                                                 loop
                           Just input      -> do reply <- lift $ sendCmd port (T.pack input)
                                                 outputStrLn reply
                                                 loop

main = withSerial port settings f
        where f port =
                do installHandler keyboardSignal (Catch (cmdLoop port)) Nothing
                   cmdLoop port


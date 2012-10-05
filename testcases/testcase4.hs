-- to be found logM
-- to be found debugM
-- to be found infoM
-- to be found noticeM
-- to be found warningM
-- to be found errorM
-- to be found criticalM
-- to be found alertM
-- to be found emergencyM
-- to be found traplogging
-- to be found logL
-- to be found getLogger
-- to be found getRootLogger
-- to be found rootLoggerName
-- to be found addHandler
-- to be found setHandlers
-- to be found getLevel
-- to be found setLevel
-- to be found saveGlobalLogger
-- to be found updateGlobalLogger


{-# OPTIONS -fglasgow-exts #-}
{- arch-tag: Logger main definition
Copyright (C) 2004-2006 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

{- |
   Module     : System.Log.Logger
   Copyright  : Copyright (C) 2004-2006 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Haskell Logging Framework, Primary Interface

Written by John Goerzen, jgoerzen\@complete.org

Welcome to the error and information logging system for Haskell.

This system is patterned after Python\'s @logging@ module,
<http://www.python.org/doc/current/lib/module-logging.html> and some of
the documentation here was based on documentation there.

To log a message, you perform operations on 'Logger's.  Each 'Logger' has a
name, and they are arranged hierarchically.  Periods serve as separators.
Therefore, a 'Logger' named \"foo\" is the parent of loggers \"foo.printing\",
\"foo.html\", and \"foo.io\".  These names can be anything you want.  They're
used to indicate the area of an application or library in which a logged
message originates.  Later you will see how you can use this concept to 
fine-tune logging behaviors based on specific application areas.

You can also tune logging behaviors based upon how important a message is.
Each message you log will have an importance associated with it.  The different
importance levels are given by the 'Priority' type.  I've also provided
some convenient functions that correspond to these importance levels:
'debugM' through 'emergencyM' log messages with the specified importance.

Now, an importance level (or 'Priority') 
is associated not just with a particular message but also
with a 'Logger'.  If the 'Priority' of a given log message is lower than
the 'Priority' configured in the 'Logger', that message is ignored.  This
way, you can globally control how verbose your logging output is.

Now, let's follow what happens under the hood when you log a message.  We'll
assume for the moment that you are logging something with a high enough
'Priority' that it passes the test in your 'Logger'.  In your code, you'll
call 'logM' or something like 'debugM' to log the message.  Your 'Logger'
decides to accept the message.  What next?

Well, we also have a notion of /handlers/ ('LogHandler's, to be precise).
A 'LogHandler' is a thing that takes a message and sends it somewhere.
That \"somewhere\" may be your screen (via standard error), your system's
logging infrastructure (via syslog), a file, or other things.  Each
'Logger' can have zero or more 'LogHandler's associated with it.  When your
'Logger' has a message to log, it passes it to every 'LogHandler' it knows
of to process.  What's more, it is also passed to /all handlers of all
ancestors of the Logger/, regardless of whether those 'Logger's would
normally have passed on the message.

To give you one extra little knob to turn, 'LogHandler's can also have
importance levels ('Priority') associated with them in the same way
that 'Logger's do.  They act just like the 'Priority' value in the
'Logger's -- as a filter.  It's useful, for instance, to make sure that
under no circumstances will a mere 'DEBUG' message show up in your syslog.

There are three built-in handlers given in two built-in modules:
"System.Log.Handler.Simple" and "System.Log.Handler.Syslog".

There is a special logger known as the /root logger/ that sits at the top
of the logger hierarchy.  It is always present, and handlers attached
there will be called for every message.  You can use 'getRootLogger' to get
it or 'rootLoggerName' to work with it by name.

Here's an example to illustrate some of these concepts:

> import System.Log.Logger
> import System.Log.Handler.Syslog
> 
> -- By default, all messages of level WARNING and above are sent to stderr.
> -- Everything else is ignored.
> 
> -- "MyApp.Component" is an arbitrary string; you can tune
> -- logging behavior based on it later.
> main = do
>        debugM "MyApp.Component"  "This is a debug message -- never to be seen"
>        warningM "MyApp.Component2" "Something Bad is about to happen."
> 
>        -- Copy everything to syslog from here on out.
>        s <- openlog "SyslogStuff" [PID] USER DEBUG
>        updateGlobalLogger rootLoggerName (addHandler s)
>       
>        errorM "MyApp.Component" "This is going to stderr and syslog."
>
>        -- Now we'd like to see everything from BuggyComponent
>        -- at DEBUG or higher go to syslog and stderr.
>        -- Also, we'd like to still ignore things less than
>        -- WARNING in other areas.
>        -- 
>        -- So, we adjust the Logger for MyApp.Component.
>
>        updateGlobalLogger "MyApp.BuggyComponent"
>                           (setLevel DEBUG)
>
>        -- This message will go to syslog and stderr
>        debugM "MyApp.BuggyComponent" "This buggy component is buggy"
> 
>        -- This message will go to syslog and stderr too.
>        warningM "MyApp.BuggyComponent" "Still Buggy"
> 
>        -- This message goes nowhere.
>        debugM "MyApp.WorkingComponent" "Hello"

-}

module System.Log.Logger(
                               -- * Basic Types
                               Logger,
                               -- ** Re-Exported from System.Log
                               Priority(..),
                               -- * Logging Messages
                               -- ** Basic
                               logM,
                               -- ** Utility Functions
                               -- These functions are wrappers for 'logM' to
                               -- make your job easier.
                               debugM, infoM, noticeM, warningM, errorM,
                               criticalM, alertM, emergencyM,
                               traplogging,
                               -- ** Logging to a particular Logger by object
                               logL,
                               -- * Logger Manipulation
{- | These functions help you work with loggers.  There are some
special things to be aware of.

First of all, whenever you first access a given logger by name, it
magically springs to life.  It has a default 'Priority' of 'DEBUG'
and an empty handler list -- which means that it will inherit whatever its
parents do.
-}
                               -- ** Finding \/ Creating Loggers
                               getLogger, getRootLogger, rootLoggerName,
                               -- ** Modifying Loggers
{- | Keep in mind that \"modification\" here is modification in the Haskell
sense.  We do not actually cause mutation in a specific 'Logger'.  Rather,
we return you a new 'Logger' object with the change applied.

Also, please note that these functions will not have an effect on the
global 'Logger' hierarchy.  You may use your new 'Logger's locally,
but other functions won't see the changes.  To make a change global,
you'll need to use 'updateGlobalLogger' or 'saveGlobalLogger'.
-}
                               addHandler, setHandlers,
                               getLevel, setLevel,
                               -- ** Saving Your Changes
{- | These functions commit changes you've made to loggers to the global
logger hierarchy. -}
                               saveGlobalLogger,
                               updateGlobalLogger
                               ) where
import System.Log
import System.Log.Handler(LogHandler)
import qualified System.Log.Handler(handle)
import System.Log.Handler.Simple
import System.IO
import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.List(map, isPrefixOf)
import qualified Data.Map as Map
import qualified Control.Exception
import Control.Monad.Error
---------------------------------------------------------------------------
-- Basic logger types
---------------------------------------------------------------------------
data HandlerT = forall a. LogHandler a => HandlerT a

data Logger = Logger { level :: Priority,
                       handlers :: [HandlerT],
                       name :: String}


type LogTree = Map.Map String Logger

{- | This is the base class for the various log handlers.  They should
all adhere to this class. -}


---------------------------------------------------------------------------
-- Utilities
---------------------------------------------------------------------------

-- | The name of the root logger, which is always defined and present
-- on the system.
rootLoggerName = ""

{- | Placeholders created when a new logger must be created.  This is used
only for the root logger default for now, as all others crawl up the tree
to find a sensible default. -}
placeholder :: Logger
placeholder = Logger {level = WARNING, handlers = [], name = ""}

---------------------------------------------------------------------------
-- Logger Tree Storage
---------------------------------------------------------------------------

-- | The log tree.  Initialize it with a default root logger 
-- and (FIXME) a logger for MissingH itself.

{-# NOINLINE logTree #-}

logTree :: MVar LogTree
-- note: only kick up tree if handled locally
logTree = 
    unsafePerformIO $ do
                      h <- streamHandler stderr DEBUG
                      newMVar (Map.singleton rootLoggerName (Logger 
                                                   {level = WARNING,
                                                    name = "",
                                                    handlers = [HandlerT h]}))

{- | Given a name, return all components of it, starting from the root.
Example return value: 

>["", "MissingH", "System.Cmd.Utils", "System.Cmd.Utils.pOpen"]

-}
componentsOfName :: String -> [String]
componentsOfName name =
    let joinComp [] _ = []
        joinComp (x:xs) [] = x : joinComp xs x
        joinComp (x:xs) accum =
            let newlevel = accum ++ "." ++ x in
                newlevel : joinComp xs newlevel
        in
        rootLoggerName : joinComp (split "." name) []

---------------------------------------------------------------------------
-- Logging With Location
---------------------------------------------------------------------------

{- | Log a message using the given logger at a given priority. -}

logM :: String                           -- ^ Name of the logger to use
     -> Priority                         -- ^ Priority of this message
     -> String                           -- ^ The log text itself
     -> IO ()

logM logname pri msg = do
                       l <- getLogger logname
                       logL l pri msg

---------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------

{- | Log a message at 'DEBUG' priority -}
debugM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
debugM s = logM s DEBUG

{- | Log a message at 'INFO' priority -}
infoM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
infoM s = logM s INFO

{- | Log a message at 'NOTICE' priority -}
noticeM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
noticeM s = logM s NOTICE

{- | Log a message at 'WARNING' priority -}
warningM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
warningM s = logM s WARNING

{- | Log a message at 'ERROR' priority -}
errorM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
errorM s = logM s ERROR

{- | Log a message at 'CRITICAL' priority -}
criticalM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
criticalM s = logM s CRITICAL

{- | Log a message at 'ALERT' priority -}
alertM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
alertM s = logM s ALERT

{- | Log a message at 'EMERGENCY' priority -}
emergencyM :: String                         -- ^ Logger name
      -> String                         -- ^ Log message
      -> IO ()
emergencyM s = logM s EMERGENCY

---------------------------------------------------------------------------
-- Public Logger Interaction Support
---------------------------------------------------------------------------

-- | Returns the logger for the given name.  If no logger with that name
-- exists, creates new loggers and any necessary parent loggers, with
-- no connected handlers.

getLogger :: String -> IO Logger
getLogger lname = modifyMVar logTree $ \lt ->
    case Map.lookup lname lt of
         Just x ->  return (lt, x) -- A logger exists; return it and leave tree
         Nothing -> do
                    -- Add logger(s).  Then call myself to retrieve it.
                    let newlt = createLoggers (componentsOfName lname) lt
                    result <- Map.lookup lname newlt
                    return (newlt, result)
    where createLoggers :: [String] -> LogTree -> LogTree
          createLoggers [] lt = lt -- No names to add; return tree unmodified
          createLoggers (x:xs) lt = -- Add logger to tree
              if Map.member x lt
                 then createLoggers xs lt
                 else createLoggers xs 
                          (Map.insert x ((modellogger lt) {name=x}) lt)
          modellogger :: LogTree -> Logger
          -- the modellogger is what we use for adding new loggers
          modellogger lt =
              findmodellogger lt (reverse $ componentsOfName lname)
          findmodellogger _ [] = error "findmodellogger: root logger does not exist?!"
          findmodellogger lt (x:xs) =
              case Map.lookup x lt of
                Left (_::String) -> findmodellogger lt xs
                Right logger -> logger {handlers = []}

-- | Returns the root logger.

getRootLogger :: IO Logger
getRootLogger = getLogger rootLoggerName

-- | Log a message, assuming the current logger's level permits it.
logL :: Logger -> Priority -> String -> IO ()
logL l pri msg = handle l (pri, msg)

-- | Handle a log request.
handle :: Logger -> LogRecord -> IO ()
handle l (pri, msg) = 
    let parentHandlers [] = return []
        parentHandlers name =
            let pname = (head . drop 1 . reverse . componentsOfName) name
                in
                do 
                --putStrLn (join "," foo)
                --putStrLn pname
                --putStrLn "1"
                parent <- getLogger pname
                --putStrLn "2"
                next <- parentHandlers pname
                --putStrLn "3"
                return ((handlers parent) ++ next)
        in
        if pri >= (level l)
           then do 
                ph <- parentHandlers (name l)
                sequence_ (handlerActions (ph ++ (handlers l)) (pri, msg)
                                          (name l))
           else return ()


-- | Call a handler given a HandlerT.
callHandler :: LogRecord -> String -> HandlerT -> IO ()
callHandler lr loggername ht =
    case ht of
            HandlerT x -> System.Log.Handler.handle x lr loggername

-- | Generate IO actions for the handlers.
handlerActions :: [HandlerT] -> LogRecord -> String -> [IO ()]
handlerActions h lr loggername = map (callHandler lr loggername ) h
                         
-- | Add handler to 'Logger'.  Returns a new 'Logger'.
addHandler :: LogHandler a => a -> Logger -> Logger
addHandler h l= l{handlers = (HandlerT h) : (handlers l)}

-- | Set the 'Logger'\'s list of handlers to the list supplied.
-- All existing handlers are removed first.
setHandlers :: LogHandler a => [a] -> Logger -> Logger
setHandlers hl l = 
    l{handlers = map (\h -> HandlerT h) hl}

-- | Returns the "level" of the logger.  Items beneath this
-- level will be ignored.

getLevel :: Logger -> Priority
getLevel l = level l

-- | Sets the "level" of the 'Logger'.  Returns a new
-- 'Logger' object with the new level.

setLevel :: Priority -> Logger -> Logger
setLevel p l = l{level = p}

-- | Updates the global record for the given logger to take into
-- account any changes you may have made.

saveGlobalLogger :: Logger -> IO ()
saveGlobalLogger l = modifyMVar_ logTree 
                     (\lt -> return $ Map.insert (name l) l lt)

{- | Helps you make changes on the given logger.  Takes a function
that makes changes and writes those changes back to the global
database.  Here's an example from above (\"s\" is a 'LogHandler'):

> updateGlobalLogger "MyApp.BuggyComponent"
>                    (setLevel DEBUG . setHandlers [s])
-}

updateGlobalLogger :: String            -- ^ Logger name
                      -> (Logger -> Logger) -- ^ Function to call
                      -> IO ()
updateGlobalLogger ln func =
    do l <- getLogger ln
       saveGlobalLogger (func l)

{- | Traps exceptions that may occur, logging them, then passing them on.

Takes a logger name, priority, leading description text (you can set it to
@\"\"@ if you don't want any), and action to run.
-}

traplogging :: String                   -- Logger name
            -> Priority                 -- Logging priority
            -> String                   -- Descriptive text to prepend to logged messages
            -> IO a                     -- Action to run
            -> IO a                     -- Return value
traplogging logger priority desc action =
    let realdesc = case desc of
                             "" -> ""
                             x -> x ++ ": "
        handler e = do
                    logM logger priority (realdesc ++ (show e))
                    Control.Exception.throw e             -- Re-raise it
        in
        Control.Exception.catch action handler
    
{- This function pulled in from MissingH to avoid a dep on it -}
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)

-- This function also pulled from MissingH
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

-- This function also pulled from MissingH
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs


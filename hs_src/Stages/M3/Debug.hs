module Stages.M3.Debug where
-- Typechecks
{-
   Functionality for managing debugging statements and 'hidden' functionality
   in DBToaster.  Accessed through dbtoaster's -d flag.
   
   The debug module keeps track of a list of globally 'active' debug modes.  
   Each mode is identified in the string, and referenced within the code via 
   the Debug.exec, Debug.print, and Debug.active functions.  
   
   Debug modes can be explicitly managed in the code using Debug.activate or 
   Debug.deactivate.  Users can activate debug modes in the dbtoaster binary by
   passing -d [mode name].  A list of available Debug modes is being maintained 
   on the Assembla page.
-}
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef
import System.IO.Unsafe

type DebugMode = String
-----
{-module StringSet = Set.Make(String)-}

{-module DebugInternal =-}
{-struct-}
   {-let debug_modes = ref StringSet.empty;-}
{-end-}

debug_modes :: IORef (Set DebugMode)
debug_modes = unsafePerformIO $ newIORef $ Set.empty

-----

{-
   Overwrite the list of active debug modes.
   ++param new_modes The new list of active debug modes.
-}
set_modes new_modes = unsafePerformIO $ writeIORef debug_modes new_modes

{-
   Activate the indicated debug mode
   ++param mode The name of the mode to activate
-}
activate mode = unsafePerformIO $ do
    r <- readIORef debug_modes
    writeIORef debug_modes $ Set.insert mode r

{-
   Deactivate the indicated debug mode
   ++param mode The name of the mode to deactivate
-}
deactivate mode = unsafePerformIO $ do
    r <- readIORef debug_modes
    writeIORef debug_modes $ Set.delete mode r

{-
   Execute a ( unit -> * ) function when the indicated debug mode is active
   ++param mode The triggering mode
   ++param f    The function to evaluate if the triggering mode is active
-}
{-exec (mode::debug_mode_t) (f::(unit->'a)):: unit =-}
   {-if StringSet.mem mode !DebugInternal.debug_modes -}
      {-then let _ = f () in () else ();;-}

{-
   Print a string when the indicated debug mode is active.  The string should 
   be encapsulated in a generator function of type ( unit -> string )
   ++param mode The triggering mode
   ++param f    The string-generating function
-}
{-print (mode::debug_mode_t) (f::(unit->string)):: unit =-}
   {-exec mode (\() -> print_endline (f ()));;-}

{-
   Determine whether the indicated debug mode is active
   ++param mode The mode to be tested
   ++return     True if the indicated mode is active
-}
active mode = unsafePerformIO $ do
    r <- readIORef debug_modes
    return $ Set.member mode r

verbose = active "PRINT-VERBOSE"

{-
   Determine the operating system on which we are running
   ++return A string describing the operating system (the output of unmame or 
           ??? if uname does not exist
-}
{-os () =-}
   {-let fdes = (Unix.open_process_in "uname") in-}
   {-let ostr = try input_line fdes with End_of_file -> "???" in-}
   {-let _ = (Unix.close_process_in fdes) in-}
      {-ostr-}

{-(** Tools for detailed logging. *)-}
{-module Logger = struct-}

   {-type level =-}
      {-| Inform-}
      {-| Warn-}
      {-| Error-}
      {-| Bug-}
   {-(** [string_of_level level]-}
   
      {-Generate a human-readable string representation of a debug level-}
   {-*)-}
   {-let string_of_level = function-}
      {-| Inform -> "INFO"-}
      {-| Warn   -> "WARNING"-}
      {-| Error  -> "ERROR"-}
      {-| Bug    -> "BUG"-}

   {-(** [log level ~continuation ~detail ~exc m msg]-}
   
      {-Log a message of a specified class, for a specified module -}
      
      {-++param m            The module to associate with this warning, or an empty-}
                          {-string for an unassociated message-}
      {-++param level        The log level to post the message at-}
      {-++param continuation (optional) A function that will be invoked after the-}
                          {-log message has been printed (for inlined functions)-}
      {-++param detail       A detailed description of the log message-}
      {-++param exc          True if the last recorded backtrace should be printed-}
      {-++param msg          The log message-}
   {-*)-}
   {-let log (level::level) ?(continuation=Nothing) (m::string)-}
           {-?(detail=(\() -> "")) ?(exc=False) (msg::string) =-}
      {-prerr_endline ("["++(string_of_level level)++-}
                     {-(if m = "" then "" else "::::"++m)++"]  "++msg);-}
      {-if exc && (active "DETAIL") then Printexc.print_backtrace stderr;-}
      {-if active "DETAIL" then prerr_endline (detail ());-}
      {-match continuation with Just(s) -> (s ()) | Nothing -> ()-}

   {-(** Log a message at the informational level *)-}
   {-let inform = log Inform ~continuation::Nothing-}

   {-(** Log a message at the warning level *)-}
   {-let warn = log Warn ~continuation::Nothing-}

   {-(** Log a message at the error level and exit with status -1 *)-}
   {-let error = log Error ~continuation::(Just(\_ -> exit (-1)))-}

   {-(** Log a message at the bug level and exit with status -1 *)-}
   {-let bug = log Bug ~continuation::(Just(\_ -> exit (-1)))-}

   {-(** Generate a set of logging functions for a specific module -}
   
      {-Typical usage for a module m is::-}
      
      {-[let (inform, warn, error, bug) = Debug.Logger.functions_for_module m]-}
      {-++param m    The name of a module-}
      {-++return     A set of functions [(inform, warn, error, bug)] identical to-}
                  {-those in the logger module, but with the module parameter -}
                  {-bound to [m].-}
   {-*)-}
   {-let functions_for_module m = (inform m, warn m, error m, bug m)-}
{-end-}

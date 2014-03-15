import Distribution.Simple
import System.Process
import System.Exit

main = defaultMainWithHooks $ simpleUserHooks { postBuild = makeManPage }

makeManPage _ _ _ _ =
    runCommand "make sloane.1" >>= waitForProcess >>= exitWith

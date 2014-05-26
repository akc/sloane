# Better command line argument handling

Use optparse-applicative or docopt. In any case "sloane [options]
search-terms" should be replaced with something more specific.

sloane # Expects input from stdin.
sloane (--version | --help | --update)
sloane [-a | --all | -k KEYS | --keys=KEYS | --url] [-n INT] search-terms

Deprecate '-u', '-?', '-V', '--limit' (and '--keys'?)

# Superseeker like functionality

Implement the transformations in <https://oeis.org/transforms.txt> in
Haskell. Combined with local search this would enable local (fast!)
superseeker like functionality.

# Make local search (against the cache) possible

~~~
$ sloane --help
...
OPTIONS
...
   -q --quick
       Quick search -- search the local cache rather than oeis.org
...
EXAMPLE
$ sloane -q 1,1,3,19,183,2371,38703,763099
...
~~~

This would require downloading names.gz (in addition to
stripped.gz). Ideally stripped and names would be hosted so that
incremental updates are possible, e.g. using git or rsync.

# Show progress while downloading stripped.gz and names.gz

If the incremental update route isn't feasible, then it would be nice to
at least get an indication of progress while downloading stripped.gz and
names.gz. One way is to use http-conduit. See Michael Snoyman's message
on [Haskell-beginners](http://www.haskell.org/pipermail/beginners/2012-December/011061.html):

~~~
[Haskell-beginners] Download a large file using Network.HTTP
Michael Snoyman michael at snoyman.com
Mon Dec 10 14:46:41 CET 2012

Here's an example of printing the total number of bytes consumed using
http-conduit:

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import           Data.Conduit
import           Data.Conduit.Binary    as CB
import           Network.HTTP.Conduit

main :: IO ()
main = withManager $ \manager -> do
    req <- parseUrl "http://www.yesodweb.com/"
    res <- http req manager
    responseBody res $$+- printProgress =$ CB.sinkFile "yesodweb.html"

printProgress :: Conduit S.ByteString (ResourceT IO) S.ByteString
printProgress =
    loop 0
  where
    loop len = await >>= maybe (return ()) (\bs -> do
        let len' = len + S.length bs
        liftIO $ putStrLn $ "Bytes consumed: " ++ show len'
        yield bs
        loop len')


HTH,
Michael
~~~

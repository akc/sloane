-- |
-- Copyright   : Anders Claesson 2012-2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
import Data.Monoid
import Control.Monad
import Test.QuickCheck
import Sloane.Transform

instance Arbitrary NamedTransform where
    arbitrary = do
        i <- choose (0, length transforms - 1)
        return (transforms !! i)

check :: Testable prop => Int -> prop -> IO ()
check n = quickCheckWith stdArgs {maxSuccess = n}

checkUnit f seq0 seq1 = ("unit/" ++ name f, check 1 ((f $$ seq0) == seq1))

runTests :: [(String, IO a)] -> IO ()
runTests = mapM_ (\(name, t) -> putStr (name ++ ":\t") >> t)

prop_Growth f = forAll (resize 18 arbitrary) $ \cs ->
    let n = length cs; as = f $$ cs in null as || length as == growth f n

prop_NoRuntimeError f = forAll (resize 18 arbitrary) $ \cs ->
    (f $$ cs) `seq` True

prop_LEFT_RIGHT cs = (tLEFT $$ tRIGHT $$ cs) == cs

prop_M2i_M2 cs = (tM2i $$ tM2 $$ cs) == cs

prop_BINOMIALi_BINOMIAL = forAll (resize 20 arbitrary) $ \cs ->
    (tBINOMIALi $$ tBINOMIAL $$ cs) == cs

prop_BINOMIAL_BINOMIALi = forAll (resize 20 arbitrary) $ \cs ->
    (tBINOMIAL $$ tBINOMIALi $$ cs) == cs

prop_EULER_EULERi = forAll (resize 10 arbitrary) $ \cs ->
    (tEULER $$ tEULERi $$ cs) == cs

prop_EULERi_EULER = forAll (resize 15 arbitrary) $ \cs ->
    (tEULERi $$ tEULER $$ cs) == cs

prop_MOBIUS_MOBIUSi cs = (tMOBIUS $$ tMOBIUSi $$ cs) == cs

prop_MOBIUSi_MOBIUS cs = (tMOBIUSi $$ tMOBIUS $$ cs) == cs

prop_LOG_EXP = forAll (resize 20 arbitrary) $ \cs ->
    (tLOG $$ tEXP $$ cs) == cs

prop_EXP_LOG = forAll (resize 20 arbitrary) $ \cs ->
    (tEXP $$ tLOG $$ cs) == cs

prop_CONVi_CONV = forAll (resize 20 arbitrary) $ \cs ->
    not (null cs) && head cs > 0 ==> (tCONVi $$ tCONV $$ cs) == cs

prop_NEGATE_involutive cs = (tNEGATE $$ tNEGATE $$ cs) == cs

prop_BIN1_involutive cs = (tBIN1 $$ tBIN1 $$ cs) == cs

prop_BISECT0_AERATE1 cs = (tBISECT0 $$ tAERATE1 $$ cs) == cs

prop_TRISECT0_AERATE2 cs = (tTRISECT0 $$ tAERATE2 $$ cs) == cs

tests =
    [ checkUnit tLEFT [4,3,2,1] [3,2,1]
    , checkUnit tRIGHT [4,3,2,1] [1,4,3,2,1]
    , checkUnit tM2 [5,4,3,2,1] [5,8,6,4,2]
    , checkUnit tM2i [5,8,6,4,2] [5,4,3,2,1]
    , checkUnit tAERATE1 [5,4,3,2,1] [5,0,4,0,3,0,2,0,1]
    , checkUnit tAERATE2 [1,2,3,4] [1,0,0,2,0,0,3,0,0,4]
    , checkUnit tBINOMIAL [1,2,4,8,16] [1,3,9,27,81]
    , checkUnit tBINOMIALi [1,3,9,27,81] [1,2,4,8,16]
    , checkUnit tBIN1 [2,4,8,16] [2,-8,26,-80]
    , checkUnit tBISECT0 [0,1,2,3,4,5] [0,2,4]
    , checkUnit tBISECT1 [0,1,2,3,4,5] [1,3,5]
    , checkUnit tDIFF [9,4,1,0,1,4,9] [-5,-3,-1,1,3,5]
    , checkUnit tMOBIUS [1,3,4,7,6,12] [1,2,3,4,5,6]
    , checkUnit tMOBIUSi [1,2,3,4,5,6] [1,3,4,7,6,12]
    , checkUnit tEULER [1,1,0,0,0,0,0] [1,2,2,3,3,4,4]
    , checkUnit tEULERi [1,2,2,3,3,4,4] [1,1,0,0,0,0,0]
    , checkUnit tEXP [1,2,3,4] [1,3,10,41]
    , checkUnit tLOG [1,3,10,41] [1,2,3,4]
    , checkUnit tCONV [1,2,3,4,5] [1,4,10,20,35]
    , checkUnit tCONVi [1,4,10,20,35] [1,2,3,4,5]
    , checkUnit tEXPCONV [1,4,9,16,25] [1,8,50,248,1048]
    , checkUnit tNEGATE [5,4,3,2,1] [5,-4,-3,-2,-1]
    , checkUnit tPRODS [1,2,3,4,5] [1,2,6,24,120]
    , checkUnit tPSUM [1,2,3,4,5] [1,3,6,10,15]
    , checkUnit tPSUMSIGN [1,2,3,4,5] [1,1,2,2,3]
    , checkUnit tSTIRLING [1,2,3,4,5] [1,3,10,37,151]
    , checkUnit tTRISECT0 [0,1,2,3,4,5,6] [0,3,6]
    , checkUnit tTRISECT1 [0,1,2,3,4,5,6] [1,4]
    , checkUnit tTRISECT2 [0,1,2,3,4,5,6] [2,5]
    , checkUnit tPOINT [1,1,4,27,256] [0,1,8,81,1024]
    , checkUnit tWEIGHT [1,1,1,1,1,1,1,1,1] [1,1,2,2,3,4,5,6,8]
    , checkUnit tPARTITION [1,3,5,13] [1,1,2,2]
    , ("eval/NoRuntimeError",     check 600 prop_NoRuntimeError)
    , ("growth",                  check 200 prop_Growth)
    , ("LEFT.RIGHT = id",         check 100 prop_LEFT_RIGHT)
    , ("M2i.M2 = id",             check 100 prop_M2i_M2)
    , ("BINOMIAL.BINOMIALi = id", check 100 prop_BINOMIAL_BINOMIALi)
    , ("BINOMIALi.BINOMIAL = id", check 100 prop_BINOMIALi_BINOMIAL)
    , ("EULER.EULERi = id",       check 100 prop_EULER_EULERi)
    , ("EULERi.EULER = id",       check 100 prop_EULERi_EULER)
    , ("MOBIUS.MOBIUSi = id",     check 100 prop_MOBIUS_MOBIUSi)
    , ("MOBIUSi.MOBIUS = id",     check 100 prop_MOBIUSi_MOBIUS)
    , ("EXP.LOG = id",            check 100 prop_EXP_LOG)
    , ("LOG.EXP = id",            check 100 prop_LOG_EXP)
    , ("CONVi.CONV = id",         check 100 prop_CONVi_CONV)
    , ("NEGATE/involutive",       check 100 prop_NEGATE_involutive)
    , ("BIN1/involutive",         check 100 prop_BIN1_involutive)
    , ("BISECT0.AERATE1 = id",    check 100 prop_BISECT0_AERATE1)
    , ("TRISECT0.AERATE2 = id",   check 100 prop_TRISECT0_AERATE2)
    ]

main = runTests tests

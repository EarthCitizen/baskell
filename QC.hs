import Test.QuickCheck

data ListOfInt = ListOfInt { getList :: [Int] } deriving (Eq, Ord)

instance Show ListOfInt where
    show loi = show $ getList loi

instance Arbitrary ListOfInt where
    arbitrary = do
        len  <- choose (0, 10) :: Gen Int
        pos  <- choose (0, 10) :: Gen Int
        ints <- vectorOf len $ choose (0, 10)
        insp <- choose (0, len - 1) :: Gen Int
        insv <- choose (-1, -100) :: Gen Int
        let (p1, p2)   = splitAt insp ints
            joinedList = p1 ++ [insv] ++ p2
        return $ ListOfInt joinedList

prop_lessThan3000 :: ListOfInt -> Bool
prop_lessThan3000 (ListOfInt ints) = (sum ints) < 3000


-- main = sample (arbitrary :: Gen ListOfInt)
main = verboseCheck prop_lessThan3000

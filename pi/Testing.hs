{-#LANGUAGE TemplateHaskell#-}
import Test.QuickCheck
import Data.DeriveTH
import Data.ByteString.Char8
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.URI

import Parser
import TypDefs

derive makeArbitrary ''Term
derive makeArbitrary ''Type
derive makeArbitrary ''HttpData
derive makeArbitrary ''URI
derive makeArbitrary ''RequestMethod
derive makeArbitrary ''Header
derive makeArbitrary ''HeaderName
derive makeArbitrary ''URIAuth

instance Arbitrary ByteString where
    arbitrary = do
        s <- arbitrary
        return $ pack s

instance Arbitrary a => Arbitrary (Request a) where
    arbitrary = do 
        u <- arbitrary
        m <- arbitrary
        h <- arbitrary
        b <- arbitrary
        return $ Request u m h b

instance Arbitrary a => Arbitrary (Response a) where
    arbitrary = do 
        x <- dig
        y <- dig
        z <- dig
        let c = (x,y,z)
        r <- arbitrary
        h <- arbitrary
        b <- arbitrary
        return $ Response c r h b
        where
            dig = choose (1,9)

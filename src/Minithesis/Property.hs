{-# LANGUAGE FlexibleInstances #-}

module Minithesis.Property
  ( Property,
    property,
    withRunOptions,
    withTests,
    applyPropertyOptions,
    runProperty,
    ToProperty (..),
  )
where

import Minithesis.Runner (RunOptions (..), runTest)
import Minithesis.TestCase (TestCase)

-- | Wrapper around a test function together with option tweaks.
data Property
  = Property (RunOptions -> RunOptions) (TestCase -> IO ())

-- | Lift a raw test function into a 'Property'.
property :: (TestCase -> IO ()) -> Property
property = Property id

-- | Modify the 'RunOptions' used for a property.
withRunOptions :: (ToProperty p) => (RunOptions -> RunOptions) -> p -> Property
withRunOptions f propLike =
  let Property tweak test = toProperty propLike
   in Property (f . tweak) test

-- | Override the maximum number of test cases.
withTests :: (ToProperty p) => Int -> p -> Property
withTests n propLike
  | n > 0 = withRunOptions (\opts -> opts {runMaxExamples = n}) propLike
  | otherwise = toProperty propLike

-- | Apply a property's option tweaks to a base configuration.
applyPropertyOptions :: RunOptions -> Property -> RunOptions
applyPropertyOptions opts (Property tweak _) = tweak opts

-- | Execute a 'Property' given fully prepared options.
runProperty :: RunOptions -> Property -> IO ()
runProperty opts (Property _ test) = runTest opts test

-- | Allow both 'Property' values and plain test functions to be used.
class ToProperty p where
  toProperty :: p -> Property

instance ToProperty Property where
  toProperty = id

instance ToProperty (TestCase -> IO ()) where
  toProperty = property

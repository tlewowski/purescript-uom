module UOM (massSum, Grams, lengthSum, Meters) where

import Data.Monoid.Additive

import Data.Array (length)
import Data.Generic (class Generic)
import Data.Monoid (class Monoid, (<>))
import Prelude (id, (*), (/), class Show)

class (Monoid a) <= UOM a where
  collect :: a -> a -> a

instance genericUOM :: (Monoid a) => UOM a where 
  collect a b = a <> b

class (UOM a, UOM b) <= ConvertibleMeasure a b where
  convert :: a -> b

instance convertableId :: (UOM a) => ConvertibleMeasure a a where
  convert = id

newtype Grams = Grams (Additive Number)
derive newtype instance monoidGrams :: Monoid Grams
derive newtype instance showGrams :: Show Grams
grams n = Grams (Additive n)


newtype Kilograms = Kilograms (Additive Number)
derive newtype instance monoidKilograms :: Monoid Kilograms
kilograms n = Kilograms (Additive n)

newtype Tons = Tons (Additive Number)
derive newtype instance monoidTons :: Monoid Tons
tons n = Tons (Additive n)

instance convertKilogramsToGrams :: ConvertibleMeasure Kilograms Grams where
  convert (Kilograms (Additive n)) = Grams (Additive (1000.0 * n))

instance convertTonsToGrams :: ConvertibleMeasure Tons Grams where
  convert (Tons (Additive n)) = Grams (Additive (1000.0 * 1000.0 * n))

instance convertId :: (UOM a) => ConvertibleMeasure a a where
  convert a = a

add :: forall a b c. UOM a => UOM b=> UOM c => ConvertibleMeasure a c => ConvertibleMeasure b c => a -> b -> c
add a b = (convert a) <> (convert b)

massSum :: Grams
massSum = add (add (tons 2.0) (kilograms 300.0) :: Grams) (grams 10.0)

newtype Meters = Meters (Additive Number)
derive newtype instance monoidMeters :: Monoid Meters
derive newtype instance showMeters :: Show Meters
meters n = Meters (Additive n)

newtype Inches = Inches (Additive Number)
derive newtype instance monoidInches :: Monoid Inches
inches n = Inches (Additive n)

instance convertInchesToMeters :: ConvertibleMeasure Inches Meters where
  convert (Inches (Additive n)) = Meters (Additive (0.025 * n))


lengthSum :: Meters
lengthSum = add (inches 2.0) (meters 1.0)
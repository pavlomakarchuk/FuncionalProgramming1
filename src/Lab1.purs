module Lab1 where
  
import Prelude

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Prim.RowList (Nil)
import Data.Foldable(foldr, foldl)


singleton :: forall a. a -> List a
singleton a = a : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc xs x = foldr (:) (x : Nil) xs

length :: forall a. List a -> Int
length = foldl (\l _ -> l + 1) 0

test::Effect Unit
test = do
  log $ show $ singleton "b"

  log $ show $ null (1 : Nil)

  log $ show $ snoc (2:Nil) 3

  log $ show $ length (1 : 2: 3: Nil)
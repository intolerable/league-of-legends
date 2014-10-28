module League.Internal.TH.FieldGen
  ( generate
  , generates ) where

import Data.Char
import Language.Haskell.TH

generate :: String -> Q Dec
generate name = return $ make name

make :: String -> Dec
make name =
  ClassD
    []
    (mkName $ "Has" ++ name)
    [PlainTV s, PlainTV a]
    [FunDep [s] [a]]
    [SigD (funName name) typeExp]
  where s = mkName "s"
        a = mkName "a"
        f = mkName "f"
        funName (x:xs) = mkName $ toLower x : xs
        funName [] = error "choose a sensible class name"
        x `arrow` y = AppT (AppT ArrowT x) y
        typeExp =
          ForallT
            [PlainTV f]
            [ClassP ''Functor [(VarT f)]]
            ((VarT a `arrow` AppT (VarT f) (VarT a)) `arrow`
                (VarT s `arrow` AppT (VarT f) (VarT s)))

generates :: [String] -> Q [Dec]
generates = mapM generate

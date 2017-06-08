module Located where

import Data.List
import Data.Maybe

data SrcSpan
  = NoSpanInfo
  | Span        { spn_Sln :: Int
                , spn_Scl :: Int
                , spn_Eln :: Int
                , spn_Ecl :: Int
                }
  deriving Eq

instance Ord SrcSpan where
        compare x y = case (x, y) of
                (NoSpanInfo      , NoSpanInfo      ) -> EQ
                (NoSpanInfo      , _               ) -> LT
                (_               , NoSpanInfo      ) -> GT
                (Span xa xb xc xd, Span ya yb yc yd) -> 
                        fromMaybe EQ $ find (/=EQ) $ zipWith compare [xa,xb,xc,xd] [ya,yb,yc,yd]
instance Show SrcSpan where
        show spn = case spn of
                NoSpanInfo       -> ""
                Span sl sc el ec -> shp sl sc ++
                                if (sl,sc) == (el,ec) then "" else shp el ec
            where
                shp :: Int -> Int -> String
                shp l c = "(" ++ show l ++ "," ++ show c ++ "):"

data Located e
  = Loc         { loc_span :: SrcSpan
                , loc_in   :: e
                }

instance Eq (Located e) where
        Loc x _  == Loc y _ = x == y

instance Ord (Located e) where
        compare (Loc x _) (Loc y _) = compare x y

instance Show e => Show (Located e) where
        show (Loc p e) = pS ++ sep ++ show e
            where
                pS :: String
                pS = show p
                sep :: String
                sep = "\n "

srcSpanSpan :: SrcSpan -> SrcSpan -> SrcSpan
srcSpanSpan aSp bSp = case (aSp, bSp) of
        (Span aSl aSc aEl aEc, Span bSl bSc bEl bEc) ->
                Span (min aSl bSl) (min aSc bSc) (min aEl bEl) (min aEc bEc)
        (NoSpanInfo, _) -> bSp
        (_, NoSpanInfo) -> aSp

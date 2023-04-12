module Language.Fixlat.M
  ( M,
    runM,
  )
where

type M = IO

runM :: M a -> IO a
runM = id

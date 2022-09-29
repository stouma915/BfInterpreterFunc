module Lib ( evaluate ) where

import qualified Data.Map.Strict as M

data State = State
  { index :: Int
  , memory :: M.Map Int Int
  , pointer :: Int
  , output :: String
  , hasError : Bool
  }

evaluate :: String -> Maybe String
evaluate sourceCode = do
  let emptyMemory = M.fromList []
  let emptyState = State 0 emptyMemory 0 "" False

  eval sourceCode emptyState
  where
    eval :: String -> State -> State
    eval code state = do
      let ind = index state

      if ind >= length code then
        state
      else do
        let newInd = ind + 1

        let mem = memory state
        let ptr = pointer state
        let out = output state
        let err = hasError state

        let newState = case code !! ind of
            '>' ->
              State newInd mem (ptr + 1) out err
            '<' -> do
              let newPtr = if ptr >= 1 then ptr - 1 else ptr

              State newInd mem newPtr out err
            '+' ->
              if not $ M.member ptr mem then
                State newInd (M.insert ptr 1 mem) ptr out err
              else do
                let current = mem M.! ptr

                if current >= 255 then
                  State newInd (M.update (\_ -> Just 0) ptr mem) ptr out err
                else
                  State newInd (M.update (\x -> Just (x + 1)) ptr mem) ptr out err
            '-' ->
              if not $ M.member ptr mem then
                State newInd (M.insert ptr 255 mem) ptr out err
              else do
                let current = mem M.! ptr

                if current <= 0 then
                  State newInd (M.update (\_ -> Just 255) ptr mem) ptr out err
                else
                  State newInd (M.update (\x -> Just (x - 1)) ptr mem) ptr out err
            '.' ->
              if not $ M.member ptr mem then
                State newInd mem ptr (out ++ "\0") err
              else do
                let current = mem M.! ptr

                State newInd mem ptr (out ++ [toEnum current :: Char]) err
            _ ->
              State newInd mem ptr out err
        
        eval code newState

    searchLoopEnd :: String -> Int -> Maybe Int
    searchLoopEnd code startIndex = search code startIndex 0 0
      where
        search :: String -> Int -> Int -> Int -> Maybe Int
        search src index x y =
          if y /= 0 && x == y then
            Just (index - 1)
          else if index >= length src then
            Nothing
          else do
            let newInd = index + 1

            case src !! index of
              '[' ->
                search src newInd (x + 1) y
              ']' ->
                search src newInd x (y + 1)
              _ ->
                search src newInd x y

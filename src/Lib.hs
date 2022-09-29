module Lib ( evaluate ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data State = State
  { index :: Int
  , memory :: M.Map Int Int
  , pointer :: Int
  , output :: String
  , hasError :: Bool
  }

evaluate :: String -> Maybe String
evaluate sourceCode = do
  let emptyMemory = M.fromList []
  let emptyState = State 0 emptyMemory 0 "" False

  let result = eval sourceCode emptyState
  if hasError result then
    Nothing
  else
    Just $ output result
  where
    evalOnce :: String -> State -> State
    evalOnce code state = do
      let ind = index state
      let mem = memory state
      let ptr = pointer state
      let out = output state
      let err = hasError state

      let newInd = ind + 1

      case code !! ind of
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
        '[' ->
          case searchLoopEnd code ind of
            Just loopEnd -> do
              let loopCode = substring newIndex loopEnd code
              let afterLoop = substring (loopEnd + 1) (length code) code

              let afterLoopState = loop loopCode afterLoop state
              let memAfter = memory afterLoopState
              let ptrAfter = pointer afterLoopState
              let outAfter = output afterLoopState
              let errAfter = hasError afterLoopState

              State (loopEnd + 1) memAfter ptrAfter outAfter errAfter
            Nothing ->
              State newInd mem ptr out True
        _ ->
          State newInd mem ptr out err

    loop :: String -> String -> State -> State
    loop loopCode afterLoop state = do
      let mem = memory state
      let ptr = pointer state
      let out = output state
      let err = hasError state

      let stateWithoutIndex = State 0 mem ptr out err

      if err then
        stateWithoutIndex
      else if not $ M.member ptr mem || (mem M.! ptr) == 0 then
        eval afterLoop stateWithoutIndex
      else do
        let newState = eval loopCode stateWithoutIndex

        loop loopCode newState

    eval :: String -> State -> State
    eval code state = do
      let ind = index state
      let err = hasError state

      if err then
        state
      else if ind >= length code then
        state
      else do
        let newState = evalOnce code state

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

    substring :: Int -> Int -> String -> String
    substring start end str = T.unpack $ T.take (end - start) (T.drop start (T.pack str))

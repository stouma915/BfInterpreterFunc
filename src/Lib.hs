module Lib ( evaluate ) where

import qualified Data.Map.Strict as M

evaluate :: String -> Maybe String
evaluate sourceCode = do
  let emptyMemory = M.fromList []

  eval sourceCode 0 emptyMemory 0 "" False
  where
    eval :: String -> Int -> M.Map Int Int -> Int -> String -> Bool -> Maybe String
    eval code index memory pointer output error = do
      if error then
        Nothing
      else if index >= length code then
        Just output
      else do
        let newIndex = index + 1

        case code !! index of
          '>' ->
            eval code newIndex memory (pointer + 1) output error
          '<' -> do
            let newPointer = if pointer >= 1 then pointer - 1 else pointer
            eval code newIndex memory newPointer output error
          '+' ->
            if not $ M.member pointer memory then
              eval code newIndex (M.insert pointer 1 memory) pointer output error
            else do
              let current = memory M.! pointer

              if current >= 255 then
                eval code newIndex (M.update (\_ -> Just 0) pointer memory) pointer output error
              else
                eval code newIndex (M.update (\x -> Just (x + 1)) pointer memory) pointer output error
          '-' ->
            if not $ M.member pointer memory then
              eval code newIndex (M.insert pointer 255 memory) pointer output error
            else do
              let current = memory M.! pointer

              if current <= 0 then
                eval code newIndex (M.update (\_ -> Just 255) pointer memory) pointer output error
              else
                eval code newIndex (M.update (\x -> Just (x - 1)) pointer memory) pointer output error
          '.' ->
            if not $ M.member pointer memory then
              eval code newIndex memory pointer (output ++ "\0") error
            else do
              let current = memory M.! pointer

              eval code newIndex memory pointer (output ++ [toEnum current :: Char]) error
          _ ->
            eval code newIndex memory pointer output error

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
            let newIndex = index + 1

            case src !! index of
              '[' ->
                search src newIndex (x + 1) y
              ']' ->
                search src newIndex x (y + 1)
              _ ->
                search src newIndex x y

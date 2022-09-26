module Lib ( evaluate ) where

import qualified Data.Map.Strict as M

evaluate :: String -> String
evaluate sourceCode = do
  let emptyMemory = M.fromList []

  eval sourceCode 0 emptyMemory 0 ""
  where
    eval :: String -> Int -> M.Map Int Int -> Int -> String -> String
    eval code index memory pointer output = do
      if index >= length code then
        output
      else do
        let newIndex = index + 1

        case code !! index of
          '>' ->
            eval code newIndex memory (pointer + 1) output
          '<' -> do
            let newPointer = if pointer >= 1 then pointer - 1 else pointer
            eval code newIndex memory newPointer output
          '+' ->
            if not $ M.member pointer memory then
              eval code newIndex (M.insert pointer 1 memory) pointer output
            else do
              let current = memory M.! pointer

              if current >= 255 then
                eval code newIndex (M.update (\_ -> Just 0) pointer memory) pointer output
              else
                eval code newIndex (M.update (\x -> Just (x + 1)) pointer memory) pointer output
          '-' ->
            if not $ M.member pointer memory then
              eval code newIndex (M.insert pointer 255 memory) pointer output
            else do
              let current = memory M.! pointer

              if current <= 0 then
                eval code newIndex (M.update (\_ -> Just 255) pointer memory) pointer output
              else
                eval code newIndex (M.update (\x -> Just (x - 1)) pointer memory) pointer output
          '.' ->
            if not $ M.member pointer memory then
              eval code newIndex memory pointer (output ++ "\0")
            else do
              let current = memory M.! pointer

              eval code newIndex memory pointer (output ++ [toEnum current :: Char])
          _ ->
            eval code newIndex memory pointer output

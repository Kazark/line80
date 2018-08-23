module Main

import System

%default total
%access public export

Foldable (Either a) where
  foldr _ z (Left _) = z
  foldr f z (Right y) = f y z

Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y

mapi : (Nat -> a -> b) -> List a -> List b
mapi = mapi' Z where
  mapi' : Nat -> (Nat -> a -> b) -> List a -> List b
  mapi' _ _ [] = []
  mapi' k f (x :: xs) = f k x :: mapi' (S k) f xs

lineTooLong : Nat -> Nat -> String -> Maybe (Nat, Nat)
lineTooLong limit lineN s =
  let len = length s
  in if len > limit
  then Just (lineN, len)
  else Nothing

linesInViolation : Nat -> List String -> List (Nat, Nat)
linesInViolation limit = catMaybes . mapi (lineTooLong limit)

formatViolation : Nat -> String -> (Nat, Nat) -> String
formatViolation limit filepath (lineN, len) =
  filepath ++ ":" ++ cast lineN ++ ": " ++ cast len ++ " > " ++ cast limit

printViolations : Nat -> String -> List (Nat, Nat) -> IO Nat
printViolations limit filepath =
  map length . traverse (putStrLn . formatViolation limit filepath)

processFile' : Nat -> String -> String -> IO Nat
processFile' limit filepath =
  printViolations limit filepath . linesInViolation limit . lines

-- Idris does not have bifunctor in its prelude
mapLeft : (a -> c) -> Either a b -> Either c b
mapLeft f (Left l) = Left (f l)
mapLeft _ (Right r) = Right r

processFile : Nat -> String -> IO (Either (String, FileError) Nat)
processFile limit filepath = do
  result <- readFile filepath
  traverse (processFile' limit filepath) $ mapLeft (MkPair filepath) result

formatTotalMsg : Nat -> Nat -> String
formatTotalMsg limit Z = "All lines <" ++ cast limit ++ "."
formatTotalMsg limit (S k) = cast (S k) ++ " total lines >" ++ cast limit ++ "."

handleResult : Nat -> Either (String, FileError) (List Nat) -> IO Int
handleResult _ (Left (filepath, err)) = do
  fPutStrLn stderr ("Failed to read " ++ filepath ++ ": " ++ show err)
  pure 1
handleResult limit (Right violations) = do
  putStrLn $ formatTotalMsg limit $ sum violations
  pure 0

main_ : Nat -> List String -> IO Int
main_ limit args = do
  result <- traverse (processFile limit) args
  handleResult limit $ traverse id result

partial
main : IO ()
main = exit !(main_ 80 !getArgs)

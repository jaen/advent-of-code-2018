{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day3
  ( overlappingClaimArea
  , nonOverlappingClaimId
  , parseInput
  )
where

import           Flow
import qualified Text.Megaparsec                 as P
import           Control.Applicative.Combinators as AC
import           Text.Megaparsec.Char            as C
import qualified Data.Either                     as E
import qualified Text.Megaparsec.Char.Lexer      as L
import           Data.Void
import           Data.Maybe

import qualified Data.Map.Strict                 as Map
import           Data.Map.Strict (Map, (!?))
import qualified Data.Set                        as Set
import           Data.Set (Set)

import           Control.Monad
import           Data.STRef.Lazy
import           Control.Monad.ST.Lazy

import Debug.Trace

type Point =
  ( Int, Int )

data Extents =
    Extents { topLeft     :: Point
            , bottomRight :: Point }
  deriving
    ( Show )

data Claim =
    Claim { id      :: Int
          , extents :: Extents }
  deriving
    ( Show )

-- solution

positionsInClaim :: Claim -> [Point]
positionsInClaim claim =
    [ (x, y) | x <- xs, y <- ys ]
  where
    xs = [top .. bottom]
    ys = [left .. right]

    Claim { extents = Extents { topLeft = (top, left)
                              , bottomRight = (bottom, right) } } = claim

overlappingClaimArea :: [Claim] -> Int
overlappingClaimArea claims =
    area
  where
    area :: Int
    area = Map.size overlappingClaims

    overlappingClaims :: Map Point (Set Int)
    overlappingClaims =
      Map.filter (Set.size .> (> 1)) claimMap

    claimMap :: Map Point (Set Int)
    claimMap = runST $ computeClaimMap claims

nonOverlappingClaimId :: [Claim] -> Int
nonOverlappingClaimId claims =
    claimId
  where
    claimId :: Int
    claimId = (Map.filter (Set.intersection overlappingClaimIds
                            .> Set.size
                            .> (== 0))
                          claimMap)
      |> Map.toList
      |> head
      |> (\(_, ids) -> ids)
      |> Set.toList
      |> head

    overlappingClaimIds :: Set Int
    overlappingClaimIds =
      Map.filter (Set.size .> (> 1)) claimMap
        |> Map.foldl Set.union Set.empty


    claimMap :: Map Point (Set Int)
    claimMap = runST $ computeClaimMap claims

computeClaimMap :: [Claim] -> ST s (Map Point (Set Int))
computeClaimMap claims = do
    map' <- newSTRef $ Map.empty

    let positions = concatMap positionsInClaim' claims

    forM_ positions (uncurry insertClaim .> modifySTRef map')

    -- equivalent to

    -- forM_ positions $ \(id, position) -> do
    --   modifySTRef map' $ insertClaim id position

    readSTRef map'
  where
    insertClaim :: Int -> Point -> Map Point (Set Int) -> Map Point (Set Int)
    insertClaim id position =
      Map.insertWith Set.union position (Set.singleton id)

    -- TODO: make less ugly
    positionsInClaim' :: Claim -> [(Int, Point)]
    positionsInClaim' claim@(Claim { id }) =
      map (\position -> ( id, position )) $ positionsInClaim claim

-- parsing

type Parser = P.Parsec Void String

consumeSpaces :: Parser ()
consumeSpaces = L.space space1 P.empty P.empty
  -- where
  --   spaces = AC.skipSome $ C.char ' '

symbolic :: String -> Parser String
symbolic = L.symbol consumeSpaces

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeSpaces

inputParser :: Parser [Claim]
inputParser =
    AC.many claim
    -- claim `AC.sepBy` C.newline
  where
    claim :: Parser Claim
    claim = do
      id <- claimId
      symbolic "@"
      ( left, top ) <- topLeft
      symbolic ":"
      ( width, height ) <- dimensions

      let topLeft     = ( top, left )
      let bottomRight = ( top + height - 1, left + width - 1 )
      let extents     = Extents { topLeft, bottomRight }

      return Claim { id, extents }

    claimId :: Parser Int
    claimId = (symbolic "#") >> number

    dimensions :: Parser Point
    dimensions = do
      width <- number
      symbolic "x"
      height <- number

      return ( width, height )

    topLeft :: Parser Point
    topLeft = do
      left <- number
      symbolic ","
      top <- number

      return ( left, top )

    number :: Parser Int
    number = lexeme $ (AC.many C.digitChar) >>= return . read

parseInput :: String -> [Claim]
parseInput =
  P.parse inputParser "" .> E.fromRight []

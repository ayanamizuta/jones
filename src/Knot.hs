module Knot where

import Polynomial

import Flow

import Debug.Trace

data Front = Front {tagf :: Int} deriving (Show,Eq)
data Back = Back {tagb :: Int} deriving (Show,Eq)
data Intersection = InterSection {edges :: (Front,Front,Back,Back)} | Trivial deriving (Show,Eq)
type Link = [Intersection]


const_trivial :: LaurantPolynomial
const_trivial = [(-2,-1),(2,-1)]

replace_tag_intersection :: Intersection -> (Int,Int) -> Intersection
replace_tag_intersection Trivial _ = Trivial
replace_tag_intersection (InterSection (Front f1,Front f2,Back b1,Back b2)) pair
                          = InterSection (Front f1_new,Front f2_new,Back b1_new,Back b2_new)
                              where [f1_new,f2_new,b1_new,b2_new] = map modify [f1,f2,b1,b2]
                                            where modify tag = if tag == fst pair then snd pair else tag

replace_tag :: Link -> (Int,Int) -> Link
replace_tag link pair = map ((flip replace_tag_intersection) pair) link

two_pairs_are_united :: (Front,Back) -> (Front,Back) -> Bool
two_pairs_are_united (Front f1,Back b1) (Front f2,Back b2) = f1 == b2 && f2 == b1

pair_united :: (Front,Back) -> Bool
pair_united (Front f,Back b) = f == b

connection_change :: Link -> (Front,Back) -> (Front,Back) -> Link
connection_change link p1 p2 | two_pairs_are_united p1 p2         = Trivial:link
                             | (tagf $ fst p1) == (tagb $ snd p2) = replace_tag link $ (tagf $ fst p2,tagb $ snd p1)
                             | (tagf $ fst p2) == (tagb $ snd p1) = replace_tag link $ (tagf $ fst p1,tagb $ snd p2)
                             | otherwise                          = link |> (flip replace_tag) (tagf $ fst p1,tagb $ snd p1)
                                                                         |> (flip replace_tag) (tagf $ fst p2,tagb $ snd p2)
                                                                         |> (++) (take (length . filter id $ map pair_united [p1,p2]) $ repeat Trivial)

kauffman_bracket :: Link -> LaurantPolynomial
kauffman_bracket [] = []
kauffman_bracket [Trivial] = [(0,1)]
kauffman_bracket (Trivial:ls) = const_trivial *. kauffman_bracket ls
kauffman_bracket all_link@((InterSection (f1,f2,b1,b2)):xs) = -- trace (show all_link) $
      let renamed_link_1 = connection_change (tail all_link) (f1,b1) (f2,b2)
          renamed_link_2 = connection_change (tail all_link) (f1,b2) (f2,b1) in
           ([(-1,1)]*.(kauffman_bracket renamed_link_1))+.([(1,1)]*.(kauffman_bracket renamed_link_2))

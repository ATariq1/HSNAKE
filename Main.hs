module Main(main) where

import System.IO.Unsafe 
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height,offset ::Int
width = 500
height = 500
offset = 0

window :: Display
window = InWindow "Modern Snake" (width,height) (offset,offset)

background :: Color
background = green

snake :: Int -> [(Float,Float)] -> [Picture]
snake 0 _ = []
snake _ [] = [] 
snake len (current:rest) = (uncurry translate current $ color red $ rectangleSolid 20 20) : snake (len-1) rest

hwall :: Float -> Picture
hwall offset = translate 0 offset $
	color (light blue) $
	  rectangleSolid 480 20

randPos::Int->(Float,Float) 
randPos x = (a,b)
	where a = fromIntegral (((x*x + 22 - x*3) `mod` 480) - 240)
              b = fromIntegral (((x*x*x + 111 - 7*x) `mod` 480) - 240)

vwall :: Float -> Picture
vwall offset = translate offset 0 $
	color (light blue) $
	  rectangleSolid 20 480

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

moveHead :: Int -> (Float,Float) -> (Float, Float)
moveHead 1 (x,y) = (x,y +20)
moveHead 2 (x,y) = (x +20,y)
moveHead 3 (x,y) = (x,y-20)
moveHead 4 (x,y) = (x-20,y)

moveSnake :: Float -> SnakeGame -> SnakeGame
moveSnake seconds game = game { snakeLoc = newSnakeLoc, snakeHist = newSnakeHist}
	where (x,y) = snakeLoc game
	      newSnakeLoc = moveHead (snakeDir game) (snakeLoc game)
	      newSnakeHist = take (snakeLen game) ([newSnakeLoc]++((snakeHist game)))

distance:: (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) = sqrt(x*x + y*y)
	where x = x2 - x1
	      y = y2 - y1

foodCollision::SnakeGame -> Bool
foodCollision game = if (distance (snakeLoc game) (foodLoc game)) <= 19 then True else False

wallCollision::SnakeGame -> Bool
wallCollision game
	| count (snakeLoc game) (snakeHist game) > 1 = True
	| fst (snakeLoc game) <= (-250) = True
	| fst (snakeLoc game) >= 250 = True
	| snd (snakeLoc game) <= (-250) = True
	| snd (snakeLoc game) >= 250 = True
	| otherwise = False

hit :: SnakeGame -> SnakeGame
hit game
	| wallCollision game = initialState
	| foodCollision game = game {foodLoc = randPos (snakeLen game),snakeLen = succ  (snakeLen game) }
	| otherwise = game

handleInput::Event -> SnakeGame -> SnakeGame
handleInput (EventKey (Char 'w') _ _ _) game = game {snakeDir = 1}
handleInput (EventKey (Char 'd') _ _ _) game = game {snakeDir = 2}
handleInput (EventKey (Char 's') _ _ _) game = game {snakeDir = 3}
handleInput (EventKey (Char 'a') _ _ _) game = game {snakeDir = 4}
handleInput _ game = game

render:: SnakeGame -> Picture
render game =  pictures [snakes,foods,walls]
	where snakes = pictures (snake (snakeLen game) (snakeHist game)) 
              foods = uncurry translate (foodLoc game) $ color white $ circleSolid 10
              walls = pictures [vwall 250,hwall 250, vwall (-250 )  ,hwall (-250)]  


data SnakeGame = Game { foodLoc:: (Float, Float),
			snakeLoc:: (Float,Float),
			snakeHist:: [(Float,Float)],
			snakeLen :: Int,
			snakeDir :: Int} deriving Show


initialState :: SnakeGame
initialState = Game {
		foodLoc = (0,100),
		snakeLoc = (0,20),
		snakeHist = [(0,20),(0,0)],
		snakeLen = 2,
		snakeDir = 1} --1 is up,2 is right, 3 is down, 4 is left
	  
fps::Int
fps = 10

update::Float -> SnakeGame -> SnakeGame
update seconds = hit . moveSnake seconds

main::IO ()
main =play window background fps initialState render handleInput update

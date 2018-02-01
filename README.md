# game-of-life-haskell
Conway's game of life written in Haskell.

## Get setup
* Install haskell: `brew cask install haskell-platform`

## Seeing Life
* Compile the executable (from the project directory): `cabal build`
* Run the executable from the provided symlink: `./game`

## Options
### Board size
* (default is a measly 10 by 10)
* `./game 400 100` will run life on a board width 400 and height 
### Board initialization
* A third command line argument with legal values `live` or `cross` will initialize a board with that shape of live cells.
* Why `line` and `cross`? They produce neat patterns.
* Example: `./game 400 100 line`

## Notes
Life is more fun to watch on a large board like 400 x 100.
This means you'll have to reduce your font size to the point that your terminal is only useful for watching life run.

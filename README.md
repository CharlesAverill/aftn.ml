# ALIEN: Fate of The Nostromo

This is a terminal-based clone of the ["ALIEN: Fate of The Nostromo" board game](https://boardgamegeek.com/boardgame/332321/alien-fate-nostromo), 
implemented in OCaml (see my original [C implementation](https://github.com/CharlesAverill/aftn)).

## Installation

```sh
opam install . --deps-only
dune install
```

## Usage
```sh
Usage: AFTN [OPTION...]
  --characters Number of characters to play with
  --use-ash Include Ash for a more challenging game
  --map Path to an alternative game board. Run `aftn.ml --map-format` to see details
  --print-map Prints the game map and exits
  --locate-game-data Print location of game data
  --help  Display this list of options
```

Check [game_data/maps/format.txt](https://github.com/CharlesAverill/aftn.ml/tree/main/game_data/maps/format.txt) 
to create your own game boards

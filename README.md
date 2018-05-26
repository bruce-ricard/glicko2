# glicko2

## Ocaml Glicko2 implementation

This library implements the Glicko2 algorithm
(http://www.glicko.net/glicko/glicko2.pdf), which provides
a rating system for 2 player games.

This library is composed of a pre-instanciated module you can use
out of the box, and a functor which allows you to configure certain
variables of the algorithm.

## Installing

### With opam (recommended)

```
opam pin add https://github.com/bruce-ricard/o-glicko2
```

### From the source

```
git clone https://github.com/bruce-ricard/o-glicko2.git
cd o-glicko2
make
make install

```

## Starting out

For first time users, or if all you need is a library which helps you
rate games, one game at a time, look into the [single game](https://github.com/bruce-ricard/o-glicko2/blob/master/example/single_game.ml)
module.

If you need to take advantage of the rating periods, you will
need to look into the
[low level](https://github.com/bruce-ricard/o-glicko2/blob/master/example/low_level.ml)
module.

If you need to configure the low level variables of the Glicko2
algorith, you might want to use provided
[functor](https://github.com/bruce-ricard/o-glicko2/blob/master/example/functor.ml).
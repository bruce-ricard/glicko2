# Glicko2 [![Build Status](https://travis-ci.org/bruce-ricard/glicko2.svg?branch=master)](https://travis-ci.org/bruce-ricard/glicko2)

## Ocaml Glicko2 implementation

This library implements the
[Glicko2 algorithm](http://www.glicko.net/glicko/glicko2.pdf),
which provides
a rating system for 2 player games.

This library is composed of a pre-instantiated module you can use
out of the box, and a functor which allows you to configure certain
variables of the algorithm.

The [original implementation](https://github.com/jlouis/o-glicko2)
of the algorithm was written by
[Jesper Louis Andersen](https://github.com/jlouis). This library adds
a clean API layer around it.

## Installing

### With opam (recommended)

```
opam pin add https://github.com/bruce-ricard/glicko2
```

### From the source

```
git clone https://github.com/bruce-ricard/glicko2.git
cd glicko2
# Install dependencies, you can find them in the opam file.
make
make install

```

## Starting out

For first time users, or if all you need is a library which helps you
rate games, one game at a time, look into the [single game](https://github.com/bruce-ricard/glicko2/blob/master/example/single_game.ml)
module.

If you need to take advantage of the rating periods, you will
need to look into the
[low level](https://github.com/bruce-ricard/glicko2/blob/master/example/low_level.ml)
module.

If you need to configure the low level variables of the Glicko2
algorithm, you might want to use provided
[functor](https://github.com/bruce-ricard/glicko2/blob/master/example/functor.ml).

## License

This library is distributed under
[GNU GPLv2](https://github.com/bruce-ricard/glicko2/blob/master/LICENSE.md).
Feel free to contact me if you have any question or issue with the licensing.

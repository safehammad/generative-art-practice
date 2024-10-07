# Generative Art Practice

In September 2024 I started a schedule of [generative art](https://en.wikipedia.org/wiki/Generative_art) practice to create a regular output of generative art every two weeks. Rather than creating anything too polished and therefore time consuming, my aim is to create interesting and varied art in a reasonable amount of time and of reasonable quality. This repository contains the code and screenshots of that art.

## Quick Start

1. This project is written in the Clojure programming language. Follow [these instructions](https://clojure.org/guides/install_clojure) to install Clojure.

2. Open a terminal, check out the repository, and navigate into the resulting directory:

```
$ git clone https://github.com/safehammad/generative-art-practice.git
$ cd generative-art-practice
```

3. Choose a piece from the `src/com/safehammad/generative_art_practice/` directory and generate the art:

```
$ clojure -M src/com/safehammad/generative_art_practice/crimson_sun.clj
```

## About

*Generative Art* is a term often used to mean independently generated art, wholly or partly generated automatically via pre-determined rules, in my case, by a computer program.

This art is written in the Clojure programming language, leaning on the excellent [Quil](https://github.com/quil/quil) library, which in turn is based on the celebrated [Processing](https://processing.org/) visual arts software, to provide the drawing routines. Although Quil is capable of creating animations, I have focused on creating static drawings, the type which can be printed and hung on the wall for all to see :)

In September 2024 I attended the amazing [Heart of Clojure](https://2024.heartofclojure.eu/) conference. [Lu Wilson](https://www.todepond.com/) gave an inspiring [keynote talk](https://2024.heartofclojure.eu/talks/what-it-means-to-be-open/) about the rewards of an "open practice" of sharing openly, early and continually, no matter how scrappy the work. That talk inspired me to start and share this generative art practice.

## The Art

### Crimson Sun

![Crimson Sun](https://raw.githubusercontent.com/safehammad/generative-art-practice/main/images/crimson-sun.png)
*27th September 2024*

### Quad Crystals

![Quad Crystals](https://raw.githubusercontent.com/safehammad/generative-art-practice/main/images/quad-crystals.png)
*8th October 2024*

## License

* Code is licenced under the [MIT](https://choosealicense.com/licenses/mit/) licence.
* Screenshots are licenced under [Creative Commons Attribution-ShareAlike 4.0 International](https://creativecommons.org/licenses/by-sa/4.0/) (CC BY-SA 4.0).

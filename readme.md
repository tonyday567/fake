fake
===

[![Build Status](https://travis-ci.org/tonyday567/online-random.png)](https://travis-ci.org/tonyday567/online-random)

See https://tonyday567.github.io/online-random/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/online-random-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~

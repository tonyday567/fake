fake
===

[![Build Status](https://travis-ci.org/tonyday567/fake.png)](https://travis-ci.org/tonyday567/fake)

See https://tonyday567.github.io/fake/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/fake-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~

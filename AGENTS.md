# Agent instructions

Follow [CONTRIBUTING.md](CONTRIBUTING.md) and [CODE_STYLE.md](CODE_STYLE.md).

- Helper functions and madrat caching: madrat decides whether a cached result is still valid from the source code of the `read*`, `convert*`, `calc*` and `tool*` functions involved. Other functions, such as a `.helper` defined next to such a function, are not part of that check, so changing them leaves outdated results in the cache. Therefore, define a helper inside the function that uses it, or, if several functions share it, as a `tool*` function of its own.

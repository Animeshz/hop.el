# hop.el

An easymotion-like plugin highly inspired from [hop.nvim](https://github.com/phaazon/hop.nvim).

```
Emacs Motion on Speed!
Move anywhere in your buffer with 1 or 2 keypress.
```

## Why?

While there has been past options for jumping over the buffers like [avy](https://github.com/abo-abo/avy) and [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode), they both didn't fit me due to various reasons & limitations.

Hence, this package evolves.

### Features

1. **Jumps closer to cursor be single-character**.
2. **As much as single character jumps:** Runtime hop/jump alphabet generation, making sure as max single-character jumps are used as possible (taking convenience into account), inspired by [TrieBackfilling Algorithm](https://phaazon.net/blog/hop-trie-backtrack-filling).
3. **Flexibility:** Allows modification of which alphabet should be splitted first (see `hop-jump-keys`), and where to hop/jump (start/middle/end of match).
4. **Clear distinction of jumps according to required keypress remaining** to that character, i.e. different faces (can be modified).
5. Easy modification of regex, with full PCRE support, first group on regex is taken as selected match for hop.
6. By default doesn't match within subwords (usually it is distraction with too many jump targets as nobody usually want to jump after hyphens `-`).


## Installation

It requires [syohex/emacs-pcre](https://github.com/syohex/emacs-pcre), and pcre(-dev) package installed on your system at build time.

### Install pcre(-dev) package based on your system

```bash
sudo xbps-install -y pcre-devel
sudo pacman --no-confirm pcre
# ...
```

### Install the emacs package

If you're using elpaca/straight as package manager, write the following package declaration:

```lisp
(use-package pcre
  ;; :straight if you use stright.el
  :elpaca (pcre :host github :repo "syohex/emacs-pcre"
                  :pre-build ("make" "all")
                  :files (:default "pcre.el" "pcre-core.so")))
(use-package hop
  :elpaca (hop :host github :repo "Animeshz/hop.el"))
```

## Usage

The library exposes **5 interactive functions** that can be accessed via M-x:

* hop-word
* hop-char
* hop-line
* hop-line-skip-whitespace
* hop-regex-pattern

Each of the function matches & create hops/jumps for their respective targets in the viewable buffer.

Package also exposes a few **user-customizable variables**, see the top of [hop.el](hop.el).

### Known Issues

These things has to be nitpicked:

* Overlay on \n makes it go and continue in upper/previous line, hence line functions feel buggy.
* Wrong key press / mouse-move interrupts are not handled.

# find-dupes-dired

Find dupes using external command (fdupes/jdupes) and handle them in dired-mode.

## Installation

Clone this repository, or install from MELPA. Add the following to your `.emacs`:

``` elisp
(require 'find-dupes-dired)
```

It needs "fdupes" in gnu/linux or "jdupes" in Windows, or others by setting
`find-dupes-dired-program` and `find-dupes-dired-ls-option`.

Run `find-dupes-dired`.


# Reference

Thanks [fd-dired](https://github.com/yqrashawn/fd-dired/blob/master/fd-dired.el)
and [rg](https://github.com/dajva/rg.el). I got some references from them.

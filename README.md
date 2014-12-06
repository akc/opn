```
  ____  ____  ____
 / __ \/ __ \/ __ \
/ /_/ / /_/ / / / /
\____/ .___/_/ /_/
    /_/            
```

[![Build Status](https://travis-ci.org/akc/opn.svg)](https://travis-ci.org/akc/opn)

The `opn` command lets you open files and URLs with associated
programs. It's intended to "just work" and to be trivial to
configure. Its configuration resides in `~/.opnconfig`; here's an
example:

```
[browser]
browser: chromium

[associations]
mupdf:    .pdf
emacs:    .lhs .hs .py .c
chromium: .png .jpg .html .txt
mpv:      .avi .mpg .mp4
djview:   .djvu
```

With this configuration, PDFs would open in `mupdf`; Haskell, Python and
C source files in `emacs`, etc.  If asked to open a file with no
matching extension, or a file without an extension, `opn` tries to be a
bit smart. If, for example, one runs

```
opn foo.h
```

then the header file `foo.h` would be opened in `emacs`. The reason is
that such files have mime type `text/x-c`, and, as files with the
extension `'.c'` shares this mime type, `opn` "guesses" that `'.h'` files
also should be opened with `emacs`.

Both `[browser]` and `[associations]` sections must be present and
nonempty in `~/.opnconfig`. The `[browser]` section should in fact
always have exactly one key, namely `browser`. So a (close to) minimal
configuration looks something like this:

```
[browser]
browser: chromium

[associations]
chromium: html
```

The `browser` is used for URLs and as a fallback, so with this
configuration all files and URLs would open in `chromium`.

Talking of `chromium`: on Linux `chromium` opens downloads using
`xdg-open`. This works pretty well if you are using a full desktop
environment like GNOME, KDE or Xfce, but less so if you are running a
light window manager such as i3 or xmonad.  We can, however, exploit a
behavior of `xdg-open` to effectively replace it with `opn`. As a
fallback, when no supported desktop session is running, `xdg-open` uses
`$BROWSER`. Thus putting

```
export BROWSER=opn
```

in your `.bashrc`, or something similar for your favorite shell, will
"trick" `chromium` and `xdg-open` into using `opn`. This is my own main
use for `opn`.

For further info see the man page:
<https://github.com/akc/opn/blob/master/opn.md>.

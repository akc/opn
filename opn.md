---
title: OPN(1) Opn User Manual | Version 0.1.1
date: 24 Aug 2014
---

# NAME

opn - open files and URLs with associated programs

# SYNOPSIS

opn (--version | [--dry-run] PATHS...)

# DESCRIPTION

The opn command lets you open files and URLs with associated
programs. It's intended to "just work" and to be trivial to configure.

The configuration resides in `~/.opnconfig`; here's an example:

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

Both `[browser]` and `[associations]` sections must be present and
nonempty. The `[browser]` section should in fact always have exactly one
key, namely `browser`. Comments must start at the beginning of the line
and start with `';'` or `'#'`.

If asked to open a file with no matching extension, or a file without an
extension, `opn` tries to be a bit smart. For example, a header file, say
`foo.h`, would be opened in `emacs`. The reason is that header files
have mime type `text/x-c`, the same type as `'.c'` files, and thus `opn`
"guesses" that `'.h'` files also should be opened with `emacs`.

# OPTIONS

--dry-run
:   Display the command, or commands, that would be executed, then exit.

--version
:   Display version information.

--help
:   Display a short help message.

# EXAMPLES

Open `opn.hs` in the application associated with the extension `'.hs'`:

```
opn opn.hs
```

Open `http://akc.is/` in the browser specified under the `[browser]`
section:

```
opn http://akc.is/
```

For each file in the current directory display the manner in which it
would be opened:

```
opn --dry-run *
```

# SEE ALSO

The source code to opn can be found at <https://github.com/akc/opn>. See
also the `README` on that page.

# AUTHOR

Anders Claesson <http://akc.is>

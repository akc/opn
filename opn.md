---
title: OPN(1) Opn User Manual | Version 0.1.0
date: 24 Aug 2014
---

# NAME

opn - open files and URLs with associated programs

# SYNOPSIS

opn (--version | [--dry-run] PATHS...)

# DESCRIPTION

The opn command lets you open files and URLs with associated
programs. It's designed to just work and to be trivial to configure.

# OPTIONS

--dry-run
:   Display the command, or commands, that would be executed, then exit.

--version
:   Display version information.

--help
:   Display a short help message.

# EXAMPLES

```
opn opn.hs
```

```
opn http://akc.is/
```

# CONFIGURATION FILE

`~/.opnrc`

# SEE ALSO

The source code to opn can be found at <https://github.com/akc/opn>. See
also the `README` on that page.

# AUTHOR

Anders Claesson <http://akc.is>

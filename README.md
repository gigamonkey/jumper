This a stupid scheme for letting me jump around in source code since I
can never seem to get TAGS files to work the way I want. It provides
two minor modes, `jumper-mode` and `jumper-line-mode`. The former is
useful when looking at source code and can be used to jump to the
definition of the symbol at any point via the `M-.` key combo and back
to where you came from with `M-,`. (The locations are arranged in a
stack so if you `M-.` several times, you can unwind with repeated
applications of `M-,`.)

The other mode, `jumper-line-mode`, is useful in shell buffers. It
also binds `M-.`, but this time to a function that tries to find the
name of a file and, optionally, a line number in the current line and
then jumps there. It currently understands error messages and stack
traces from Perl, Python, and Ruby as well as the output of `grep
-nr`. As in `jumper-mode`, `M-,` pops you back to where you came from
and the two modes use the same stack so you can `M-.` to a file from
`jumper-line-mode` and then explore further with the `jumper-mode`
`M-.` and eventually `M-,` your way back to where you started.

To find a definition, `jumper-mode` looks in a file named `JUMPER`
that lives somewhere above the source file you are in. Each line of
this file should contain a name, file name, line-number triple
delimited by tabs. The scripts `rubydefs` and `scaladefs` included
with this library provide a quick and dirty way to do this for Ruby
and Scala code. Here's how I generate my `JUMPER` file:

    rm -f JUMPER
    touch JUMPER
    find . -name '*.rb' | xargs ~/hacks/jumper/rubydefs >> JUMPER
    find . -name '*.scala' | xargs ~/hacks/jumper/scaladefs >> JUMPER

To install the elisp, place the file `jumper.el` in a directory that's
in your `load-path` (or use `add-to-list` to add the directory it's in
to `load-path`) and add these lines to your `.emacs` to enable this
mode, for example, in Ruby and Scala code and in `shell-mode`.

    (require 'jumper)
    (add-hook 'ruby-mode-hook 'jumper-mode)
    (add-hook 'scala-mode-hook 'jumper-mode)
    (add-hook 'shell-mode-hook 'jumper-line-mode)

There's, at the moment, nothing particularly clever about this: if
there are multiple definitions for the same name your get taken to
some random one. I plan to fix that if it turns out to be a real
problem in practice.
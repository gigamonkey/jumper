This a stupid scheme for letting me jump around in source code since I
can never seem to get TAGS files to work the way I want.

The basic idea is this: create a file named `JUMPER` somewhere above
most of you source with each line containing a name, file name,
line-number triple delimited by tabs. The scripts `rubydefs` and
`scaladefs` provide a quick and dirty way to do this for Ruby and
Scala code. Here's how I generate my `JUMPER` file:

    rm -f JUMPER
    touch JUMPER
    find . -name '*.rb' | xargs ~/hacks/jumper/rubydefs >> JUMPER
    find . -name '*.scala' | xargs ~/hacks/jumper/scaladefs >> JUMPER

Then the minor mode, `jumper-mode` provides bindings for `M-.` and
`M-,` that allow you to jump to definitions and back. Put the file
`jumper.el` in a directory that's in your `load-path` (or use
`add-to-list` to add the directory its in to `load-path`) and  add these
lines to your `.emacs` to enable this mode in Ruby and Scala code.

    (require 'jumper)
    (add-hook 'ruby-mode-hook 'jumper-mode)
    (add-hook 'scala-mode-hook 'jumper-mode)

Then in any Ruby or Scala file whenever the cursor is in or
immediately after a symbol (e.g. the name of a function or class), I
can hit `M-.` to jump to the definition. `M-,` pops back to where I
came from. (The locations are arranged in a stack so if you `M-.`
several times, you can unwind with repeated applications of `M-,`.)

There's, at the moment, nothing particularly clever about this: if
there are multiple definitions for the same name your get taken to
some random one. I plan to fix that if it turns out to be a real
problem in practice.
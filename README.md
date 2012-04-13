This a stupid scheme for letting me jump around in source code since I
can never seem to get TAGS files to work the way I want.

The basic idea is this: create a file named `JUMPER` somewhere above
most of you source with each line containing a name, filename,
line-number triple delimited by tabs. The scripts `rubydefs` and
`scaladefs` provide a quick and dirty way to do this for Ruby and
Scala code. Here's how I generate my `JUMPER` file:

    rm -f JUMPER
    touch JUMPER
    find . -name '*.rb' | xargs ~/hacks/jumper/rubydefs >> JUMPER
    find . -name '*.scala' | xargs ~/hacks/jumper/scaladefs >> JUMPER

Then I add these two lines to my `.emacs`:

    (add-hook 'ruby-mode-hook 'jumper-mode)
    (add-hook 'scala-mode-hook 'jumper-mode)

Then in any Ruby or Scala file whenever the cursor is in or
immediately after a symbol (e.g. the name of a function or class), I
can hit `M-.` to jump to the definition. Then `M-,` pops back to where
I came from. (The locations are arranged in a stack so if you `M-.`
several times, you can unwind with repeated `M-,`s.)
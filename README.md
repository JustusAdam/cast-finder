# cast-finder

Let it loose on C source files or entire directories and it will print, for each
file, every time something is being cast and the line number where this occurs.

Build with `stack build` and run with `stack exec -- cast-finder [source
files/dirs]`. You can also pass `--help` to see the command line options.

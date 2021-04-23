# About

`bfs` (Browse File System) is an emacs environment that allow you
to **browse** your file system with a layout similar to the
[ranger](https://github.com/ranger/ranger) layout.

# Warning

`bfs` is a small package with a uniq goal: *to give the user
a way to visualize the structure of his file system with preview of
the visited files*.  **Nothing more**.

If you want to copy, past, rename, modify ownership, package and
compress files..., use `dired`, `wdired` and your favorite `shell`.
At any time, `bfs` tries (or will try) to replace or re-implement
features from those tools.

I don't care about *colors* and *icons* to distinguish file type.  So
I didn't design the code neither to allow them nor to refuse them.

The `ls` utility let you list files almost in any way you can think
about.  In `bfs`, files are listed in alphabetical order with the
directories first appending a `/` to the directory names.  The exact
command used is: `ls -Ap --group-directories-first`.  I didn't design
the code to allow  *ls switches*, so if you need fancy file listings,
use [dired-quick-sort](https://gitlab.com/xuhdev/dired-quick-sort)
inside `dired` or directly `ls` in your terminal.

# License

Project under MIT license

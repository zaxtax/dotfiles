#!/bin/sh
kitty --class="scratch.pad" -o hide_window_decorations=true -o clear_all_shortcuts=yes -e emacs -nw --no-init-file --no-site-file --load ~/.emacs.minimal ~/notes/scratch.md

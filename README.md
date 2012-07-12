emamux.el
==================

Introduction
------------
*tmux* manipulation from Emacs. *emamux.el* is inspired by
[tslime.vim](https://github.com/kikijump/tslime.vim) and
[vimux](https://github.com/benmills/vimux/).

*WARNINGS: THIS IS ALPHA VERSION.*


Requirements
------------
* Emacs 22.1 or higher.
* tmux 1.6


Basic Usage
-----------

Send command to specified *target-session*(session:window.pane).

    M-x emamux:send-command

*target-session* is set as default at first `emamux:send-command` called.
If you change default *target-session*, you add `C-u` prefix.

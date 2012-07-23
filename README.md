emamux.el
==================
Interact with tmux from Emacs.

![emamux_run_command](https://raw.github.com/syohex/emacs-emamux/master/image/image/run_command_screenshot.png)


Introduction
------------
**emamux.el** let emacs interact with **tmux**.
**emamux.el** is inspired by [tslime.vim](https://github.com/kikijump/tslime.vim) and
[vimux](https://github.com/benmills/vimux/).

*WARNINGS: THIS IS ALPHA VERSION.*


Requirements
------------
* Emacs 22.1 or higher.
* tmux 1.5


Basic Usage
-----------

Send command to specified *target-session*(session:window.pane).

    M-x emamux:send-command

*target-session* is set as default at first `emamux:send-command` called.
You can change default *target-session* with `C-u` prefix.

**Following commands can be executed only within tmux**

Run command in a small split pane(`runner pane`) where emacs is in.

    M-x emamux:run-command

Move into the `runner pane` and enter the copy mode.

    M-x emamux:inspect-runner

Close `runner pane`.

    M-x emamux:close-runner-pane

Close all other panes in current window.

    M-x emamux:close-panes

Interrupt command which is running in `runner-pane`.

    M-x emamux:interrupt-runner

Clear tmux history in `runnerr-pane`

    M-x emamux:clear-runner-history


Customize
---------

Orientation of split pane, 'vertical or 'horizonal(Default is 'vertical).

    emamux:default-orientation

Height of `runner-pane`(Default is 20).

    emamux:runner-pane-height

Use nearest pane as `runner pane` instead of spliting pane(Default is nil).

    emamux:use-nearest-pane

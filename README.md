# emamux.el

Interact with tmux from Emacs.

![emamux_run_command](image/run_command_screenshot.png)


## Introduction

**emamux.el** let emacs interact with **tmux**.
**emamux.el** is inspired by [tslime.vim](https://github.com/kikijump/tslime.vim) and
[vimux](https://github.com/benmills/vimux/).

*WARNINGS: THIS IS ALPHA VERSION.*


## Requirements

* Emacs 23 or higher.
* tmux 1.5


## Basic Usage

#### emamux:send-command

Send command to specified *target-session*(session:window.pane).

*target-session* is set as default at first `emamux:send-command` called.
You can change default *target-session* with `C-u` prefix.

#### emamux:copy-kill-ring

Copy content of (car kill-ring) to tmux buffer

You can change buffer index with Numerical Prefix.


**Following commands can be executed only within tmux**

#### emamux:run-command

Run command in a small split pane(`runner pane`) where emacs is in.

#### emamux:run-last-command

Run last command in `runner pane`.

#### emamux:inspect-runner

Move into the `runner pane` and enter the copy mode.

#### emamux:close-runner-pane

Close `runner pane`.

#### emamux:close-panes

Close all other panes in current window.

#### emamux:interrupt-runner

Interrupt command which is running in `runner-pane`.

#### emamux:clear-runner-history

Clear tmux history in `runner-pane`


## Customize

#### emamux:default-orientation

Orientation of split pane, 'vertical or 'horizonal(Default is 'vertical).

#### emamux:runner-pane-height

Height of `runner-pane`(Default is 20).


#### emamux:use-nearest-pane

Use nearest pane as `runner pane` instead of spliting pane(Default is nil).


## Emamux Applications

* [emamux-perl-test](https://github.com/syohex/emamux-perl-test) a set of commands to easily run perl tests
* [emamux-ruby-test](https://github.com/syohex/emamux-ruby-test) a set of commands to easily run ruby tests

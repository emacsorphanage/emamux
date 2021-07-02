;;; emamux.el --- Interact with tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-emamux
;; Version: 0.14
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emamux makes you interact emacs and tmux.
;; emamux is inspired by `vimux' and `tslime.vim'.
;;
;; To use emamux, add the following code into your init.el or .emacs:
;;
;;    (require 'emamux)
;;
;; Please see https://github.com/syohex/emacs-emamux/
;; for more information.

;;; Code:

(eval-when-compile
  (defvar helm-mode))

(require 'cl-lib)
(require 'tramp)

(with-eval-after-load "emamux" (emamux:setup-hooks))

(defgroup emamux nil
  "tmux manipulation from Emacs"
  :prefix "emamux:"
  :group 'processes)

(defcustom emamux:default-orientation 'vertical
  "Orientation of spliting runner pane"
  :type '(choice (const :tag "Split pane vertial" vertical)
                 (const :tag "Split pane horizonal" horizonal)))

(defcustom emamux:runner-pane-height 20
  "Orientation of spliting runner pane"
  :type  'integer)

(defcustom emamux:use-nearest-pane nil
  "Use nearest pane for runner pane"
  :type  'boolean)

(defsubst emamux:helm-mode-enabled-p ()
  (and (featurep 'helm) helm-mode))

(defcustom emamux:completing-read-type (if ido-mode
                                           'ido
                                         (if (emamux:helm-mode-enabled-p)
                                             'helm
                                           'normal))
  "Function type to call for completing read.
For helm completion use either `normal' or `helm' and turn on `helm-mode'."
  :type '(choice (const :tag "Using completing-read" 'normal)
                 (const :tag "Using ido-completing-read" 'ido)
                 (const :tag "Using helm completion" 'helm)))

(defcustom emamux:get-buffers-regexp
  "^\\([0-9]+\\): +\\([0-9]+\\) +\\(bytes\\): +[\"]\\(.*\\)[\"]"
  "Regexp used to match buffers entries in output of tmux command `get-buffers'.
The entry selected should be the subexp 4 of regexp.
NOTE that on last versions of tmux (2.0+) each line start with \"buffer\", so regexp
should be:
    \"^\\(buffer[0-9]+\\): +\\([0-9]+\\) +\\(bytes\\): +[\"]\\(.*\\)[\"]\""
  :type 'regexp)

(defcustom emamux:show-buffers-with-index t
  "Pass INDEX (a number) as argument to tmux command show-buffer when non-nil.
Tmux versions >= 2.0 expect a buffer name whereas versions < 2.0 require an index.
Use nil when using recent tmux versions, the `emamux:get-buffers-regexp' should
match \"buffer[0-9]+\" in its first subexp as well."
  :type 'boolean)

(defvar emamux:last-command nil
  "Last emit command")

(defvar emamux:session nil)
(defvar emamux:window nil)
(defvar emamux:pane nil)

(defsubst emamux:tmux-running-p ()
  (zerop (process-file "tmux" nil nil nil "has-session")))

(defun emamux:tmux-run-command (output &rest args)
  (let ((retval (apply 'process-file "tmux" nil output nil args)))
    (unless (zerop retval)
      (error (format "Failed: %s(status = %d)"
                     (mapconcat 'identity (cons "tmux" args) " ")
                     retval)))))

(defun emamux:set-parameters ()
  (emamux:set-parameter-session)
  (emamux:set-parameter-window)
  (emamux:set-parameter-pane))

(defun emamux:unset-parameters ()
  (setq emamux:session nil emamux:window nil emamux:pane nil))

(defun emamux:set-parameters-p ()
  (and emamux:session emamux:window emamux:pane))

(defun emamux:select-completing-read-function ()
  (cl-case emamux:completing-read-type
    ((normal helm) 'completing-read)
    (ido 'ido-completing-read)))

(defun emamux:mode-function ()
  (cl-case emamux:completing-read-type
    ((normal ido) 'ignore)
    (helm (if (emamux:helm-mode-enabled-p) 'ignore 'helm-mode))))

(defun emamux:completing-read (prompt &rest args)
  (let ((mode-function (emamux:mode-function)))
    (unwind-protect
        (progn
          (funcall mode-function +1)
          (apply (emamux:select-completing-read-function) prompt args))
      (funcall mode-function -1))))

(defun emamux:read-parameter-session ()
  (let ((candidates (emamux:get-sessions)))
    (if (= (length candidates) 1)
        (car candidates)
      (emamux:completing-read "Session: " candidates nil t))))

(defun emamux:set-parameter-session ()
  (setq emamux:session (emamux:read-parameter-session)))

(defun emamux:read-parameter-window ()
  (let* ((candidates (emamux:get-window))
         (selected (if (= (length candidates) 1)
                       (car candidates)
                     (emamux:completing-read "Window: " candidates nil t))))
    (car (split-string selected ":"))))

(defun emamux:set-parameter-window ()
  (setq emamux:window (emamux:read-parameter-window)))

(defun emamux:read-parameter-pane ()
  (let ((candidates (emamux:get-pane)))
    (if (= (length candidates) 1)
        (car candidates)
      (emamux:completing-read "Input pane: " candidates))))

(defun emamux:set-parameter-pane ()
  (setq emamux:pane (emamux:read-parameter-pane)))

(cl-defun emamux:target-session (&optional (session emamux:session)
                                           (window emamux:window)
                                           (pane emamux:pane))
  (format "%s:%s.%s" session window pane))

(defun emamux:get-sessions ()
  (with-temp-buffer
    (emamux:tmux-run-command t "list-sessions")
    (goto-char (point-min))
    (let (sessions)
      (while (re-search-forward "^\\([^:]+\\):" nil t)
        (push (match-string-no-properties 1) sessions))
      sessions)))

(defun emamux:get-buffers ()
  (with-temp-buffer
    (emamux:tmux-run-command t "list-buffers")
    (goto-char (point-min))
    (cl-loop for count from 0 while
          (re-search-forward emamux:get-buffers-regexp nil t)
          collect (cons (replace-regexp-in-string
                         "\\s\\" "" (match-string-no-properties 4))
                        (if emamux:show-buffers-with-index
                            count
                            (match-string-no-properties 1))))))

(defun emamux:show-buffer (index)
  (with-temp-buffer
    (emamux:tmux-run-command t "show-buffer" "-b" (if emamux:show-buffers-with-index
                                                      (number-to-string index)
                                                      index))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun emamux:get-window ()
  (with-temp-buffer
    (emamux:tmux-run-command t "list-windows" "-t" emamux:session)
    (goto-char (point-min))
    (let (windows)
      (while (re-search-forward "^\\([0-9]+: [^ ]+\\)" nil t)
        (push (match-string-no-properties 1) windows))
      (reverse windows))))

(defun emamux:get-pane ()
  (with-temp-buffer
    (let ((pane-id (concat emamux:session ":" emamux:window)))
      (emamux:tmux-run-command t "list-panes" "-t" pane-id))
    (goto-char (point-min))
    (let (panes)
      (while (re-search-forward "^\\([0-9]+\\):" nil t)
        (push (match-string-no-properties 1) panes))
      (reverse panes))))

(defun emamux:read-command (prompt use-last-cmd)
  (let ((cmd (read-shell-command prompt (and use-last-cmd emamux:last-command))))
    (setq emamux:last-command cmd)
    cmd))

(defun emamux:check-tmux-running ()
  (unless (emamux:tmux-running-p)
    (error "'tmux' does not run on this machine!!")))

;;;###autoload
(defun emamux:send-command (&optional command target)
  "Send command to target-session of tmux"
  (interactive)
  (emamux:check-tmux-running)
  (condition-case nil
      (progn
        (when (and (not target) (or current-prefix-arg (not (emamux:set-parameters-p))))
          (emamux:set-parameters))
        (let* ((target (emamux:target-session))
               (prompt (format "Command [Send to (%s)]: " target))
               (input  (or command (emamux:read-command prompt t))))
          (emamux:reset-prompt target)
          (emamux:send-keys input)))
      (quit (emamux:unset-parameters))))

;;;###autoload
(defun emamux:send-region (beg end)
  "Send region to target-session of tmux"
  (interactive "r")
  (emamux:check-tmux-running)
  (condition-case nil
      (progn
        (if (or current-prefix-arg (not (emamux:set-parameters-p)))
            (emamux:set-parameters))
        (let ((target (emamux:target-session))
              (input (buffer-substring-no-properties beg end)))
          (setq emamux:last-command input)
          (emamux:reset-prompt target)
          (emamux:send-keys input)))
    (quit (emamux:unset-parameters))))

;;;###autoload
(defun emamux:copy-kill-ring (arg)
  "Set (car kill-ring) to tmux buffer"
  (interactive "P")
  (emamux:check-tmux-running)
  (when (null kill-ring)
    (error "kill-ring is nil!!"))
  (let ((index (or arg 0))
        (data (substring-no-properties (car kill-ring))))
    (emamux:set-buffer data index)))

;;;###autoload
(defun emamux:yank-from-list-buffers ()
  (interactive)
  (emamux:check-tmux-running)
  (let* ((candidates (emamux:get-buffers))
         (index (assoc-default
                 (emamux:completing-read
                  "Buffers: " (mapcar 'car candidates))
                 candidates)))
    (insert (emamux:show-buffer index))))

;;;###autoload
(defun emamux:kill-session ()
  "Kill tmux session"
  (interactive)
  (emamux:check-tmux-running)
  (let ((session (emamux:read-parameter-session)))
    (emamux:tmux-run-command nil "kill-session" "-t" session)))

(defsubst emamux:escape-semicolon (str)
  (replace-regexp-in-string ";\\'" "\\\\;" str))

(cl-defun emamux:send-keys (input &optional (target (emamux:target-session)))
  (let ((escaped (emamux:escape-semicolon input)))
    (emamux:tmux-run-command nil "send-keys" "-t" target escaped "C-m")))

(defun emamux:set-buffer-argument (index data)
  (if (zerop index)
      (list data)
    (list "-b" (number-to-string index) data)))

(defun emamux:set-buffer (data index)
  (let ((args (emamux:set-buffer-argument index data)))
    (apply 'emamux:tmux-run-command nil "set-buffer" args)))

(defun emamux:in-tmux-p ()
  (and (not (display-graphic-p))
       (getenv "TMUX")))

(defvar emamux:runner-pane-id-map nil)

(defun emamux:gc-runner-pane-map ()
  (let ((alive-window-ids (emamux:window-ids))
        ret)
    (dolist (entry emamux:runner-pane-id-map)
      (if (and (member (car entry) alive-window-ids))
          (setq ret (cons entry ret))))
    (setq emamux:runner-pane-id-map ret)))

;;;###autoload
(defun emamux:run-command (cmd &optional cmddir)
  "Run command"
  (interactive
   (list (emamux:read-command "Run command: " nil)))
  (emamux:check-tmux-running)
  (unless (emamux:in-tmux-p)
    (error "You are not in 'tmux'"))
  (emamux:gc-runner-pane-map)
  (let ((current-pane (emamux:current-active-pane-id)))
    (unless (emamux:runner-alive-p)
      (emamux:setup-runner-pane)
      (emamux:chdir-pane cmddir))
    (emamux:send-keys cmd (emamux:get-runner-pane-id))
    (emamux:select-pane current-pane)))

;;;###autoload
(defun emamux:run-last-command ()
  (interactive)
  (unless emamux:last-command
    (error "You have never run command"))
  (emamux:run-command emamux:last-command))

(defun emamux:reset-prompt (pane)
  (emamux:tmux-run-command nil "send-keys" "-t" pane "q" "C-u"))

(defun emamux:chdir-pane (dir)
  (let ((chdir-cmd (format " cd %s" (or dir default-directory))))
    (emamux:send-keys chdir-cmd (emamux:get-runner-pane-id))))

(defun emamux:get-runner-pane-id ()
  (assoc-default (emamux:current-active-window-id) emamux:runner-pane-id-map))

(defun emamux:add-to-assoc (key value alist-variable)
  (let* ((alist (symbol-value alist-variable))
         (entry (assoc key alist)))
    (if entry (setcdr entry value)
      (set alist-variable
           (cons (cons key value) alist)))))

(defun emamux:setup-runner-pane ()
  (let ((nearest-pane-id (emamux:nearest-inactive-pane-id (emamux:list-panes))))
    (if (and emamux:use-nearest-pane nearest-pane-id)
        (progn
          (emamux:select-pane nearest-pane-id)
          (emamux:reset-prompt nearest-pane-id))
      (emamux:split-runner-pane))
    (emamux:add-to-assoc
     (emamux:current-active-window-id)
     (emamux:current-active-pane-id)
     'emamux:runner-pane-id-map)))

(defun emamux:select-pane (target)
  (emamux:tmux-run-command nil "select-pane" "-t" target))

(defconst emamux:orientation-option-alist
  '((vertical . "-v") (horizonal . "-h")))

(defun emamux:split-runner-pane ()
  (let ((orient-option (assoc-default emamux:default-orientation
                                      emamux:orientation-option-alist)))
    (emamux:tmux-run-command nil
                             "split-window" "-p"
                             (number-to-string emamux:runner-pane-height)
                             orient-option)))

(defun emamux:list-panes ()
  (with-temp-buffer
    (emamux:tmux-run-command t "list-panes")
    (cl-loop initially (goto-char (point-min))
             while (re-search-forward "^\\(.+\\)$" nil t)
             collect (match-string-no-properties 1))))

(defun emamux:active-pane-id (panes)
  (cl-loop for pane in panes
           when (string-match "\\([^ ]+\\) (active)\\'" pane)
           return (match-string-no-properties 1 pane)))

(defun emamux:current-active-pane-id ()
  (emamux:active-pane-id (emamux:list-panes)))

(defun emamux:nearest-inactive-pane-id (panes)
  (cl-loop for pane in panes
           when (and (not (string-match-p "(active)\\'" pane))
                     (string-match " \\([^ ]+\\)\\'" pane))
           return (match-string-no-properties 1 pane)))

;;;###autoload
(defun emamux:close-runner-pane ()
  "Close runner pane"
  (interactive)
  (let ((window-id (emamux:current-active-window-id)))
    (emamux:kill-pane window-id)
    (delete (assoc window-id emamux:runner-pane-id-map) emamux:runner-pane-id-map)))

;;;###autoload
(defun emamux:close-panes ()
  "Close all panes except current pane"
  (interactive)
  (when (> (length (emamux:list-panes)) 1)
    (emamux:kill-all-panes)))

(defun emamux:kill-all-panes ()
  (emamux:tmux-run-command nil "kill-pane" "-a"))

(defun emamux:kill-pane (target)
  (emamux:tmux-run-command nil "kill-pane" "-t" target))

(defsubst emamux:pane-alive-p (target)
  (zerop (process-file "tmux" nil nil nil "list-panes" "-t" target)))

(defun emamux:runner-alive-p ()
  (let ((pane-id
         (assoc-default
          (emamux:current-active-window-id)
          emamux:runner-pane-id-map)))
    (and pane-id (emamux:pane-alive-p pane-id))))

(defun emamux:check-runner-alive ()
  (unless (emamux:runner-alive-p)
    (error "There is no runner pane")))

;;;###autoload
(defun emamux:inspect-runner ()
  "Enter copy-mode in runner pane"
  (interactive)
  (emamux:check-runner-alive)
  (emamux:select-pane (emamux:get-runner-pane-id))
  (emamux:tmux-run-command nil "copy-mode"))

;;;###autoload
(defun emamux:interrupt-runner ()
  "Send SIGINT to runner pane"
  (interactive)
  (emamux:check-runner-alive)
  (emamux:tmux-run-command nil "send-keys" "-t" (emamux:get-runner-pane-id) "^c"))

;;;###autoload
(defun emamux:clear-runner-history ()
  "Clear history of runner pane"
  (interactive)
  (emamux:check-runner-alive)
  (emamux:tmux-run-command nil "clear-history" (emamux:get-runner-pane-id)))

;;;###autoload
(defun emamux:zoom-runner ()
  "Zoom runner pane. This feature requires tmux 1.8 or higher"
  (interactive)
  (emamux:check-runner-alive)
  (emamux:tmux-run-command nil "resize-pane" "-Z" "-t" (emamux:get-runner-pane-id)))

(defmacro emamux:ensure-ssh-and-cd (&rest body)
  "Do whatever the operation, and send keys of ssh and cd according to the `default-directory'."
  (cl-declare (special localname host))
  `(let (cd-to ssh-to ssh-user)
     (if (file-remote-p default-directory)
         (with-parsed-tramp-file-name
             default-directory nil
           (setq cd-to localname)
           (unless (string-match tramp-local-host-regexp host)
             (setq ssh-to host)
             (setq ssh-user user)))
       (setq cd-to default-directory))
     (let ((default-directory (expand-file-name "~")))
       ,@body
       (let ((new-pane-id (emamux:current-active-pane-id))
             (chdir-cmd (format " cd %s" cd-to)))
         (if ssh-to
             (emamux:send-keys
              (format " ssh %s%s"
                      (if ssh-user (concat ssh-user "@") "")
                      ssh-to)
              new-pane-id))
         (emamux:send-keys chdir-cmd new-pane-id)))))

;;;###autoload
(defun emamux:new-window ()
  "Create new window by cd-ing to current directory.
With prefix-arg, use '-a' option to insert the new window next to current index."
  (interactive)
  (emamux:ensure-ssh-and-cd
   (apply 'emamux:tmux-run-command nil "new-window"
          (and current-prefix-arg '("-a")))))

(defun emamux:list-windows ()
  (with-temp-buffer
    (emamux:tmux-run-command t "list-windows")
    (cl-loop initially (goto-char (point-min))
             while (re-search-forward "^\\(.+\\)$" nil t)
             collect (match-string-no-properties 1))))

(defun emamux:window-ids ()
  (with-temp-buffer
    (emamux:tmux-run-command t "list-windows" "-F" "#{window_id}")
    (split-string (buffer-string))))

(defun emamux:active-window-id (windows)
  (cl-loop for window in windows
           when (string-match "\\([^ ]+\\) (active)\\'" window)
           return (match-string-no-properties 1 window)))

(defun emamux:current-active-window-id ()
  (emamux:active-window-id (emamux:list-windows)))

(defvar emamux:cloning-window-state nil)

;;;###autoload
(defun emamux:clone-current-frame ()
  "Clones current frame into a new tmux window.
With prefix-arg, use '-a' option to insert the new window next to current index."
  (interactive)
  (setq emamux:cloning-window-state (window-state-get (frame-root-window)))
  (apply 'emamux:tmux-run-command nil
         "new-window" (and current-prefix-arg '("-a")))
  (let ((new-window-id (emamux:current-active-window-id))
        (chdir-cmd (format " cd %s" default-directory))
        (emacsclient-cmd " emacsclient -t -e '(run-with-timer 0.01 nil (lambda () (window-state-put emamux:cloning-window-state nil (quote safe))))'"))
    (emamux:send-keys chdir-cmd new-window-id)
    (emamux:send-keys emacsclient-cmd new-window-id)))

;;;###autoload
(defun emamux:split-window ()
  (interactive)
  (emamux:ensure-ssh-and-cd
   (emamux:tmux-run-command nil "split-window")))

;;;###autoload
(defun emamux:split-window-horizontally ()
  (interactive)
  (emamux:ensure-ssh-and-cd
   (emamux:tmux-run-command nil "split-window" "-h")))

;;;###autoload
(defun emamux:run-region (beg end)
  "Send region to runner pane."
  (interactive "r")
  (let ((input (buffer-substring-no-properties beg end)))
    (emamux:run-command input)))

(defun emamux:set-title-string (title)
  (emamux:tmux-run-command nil "rename-window" title)
  (emamux:tmux-run-command nil "select-pane" "-T" title))

(defun emamux-hook-buffer-change (frame)
  (if (buffer-name)
    (emamux:set-title-string (buffer-name))))

(defun emamux:setup-hooks ()
  (add-hook 'window-buffer-change-functions 'emamux-hook-buffer-change)
  (add-hook 'window-selection-change-functions 'emamux-hook-buffer-change))

(defvar emamux:keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" #'emamux:send-command)
    (define-key map "\C-y" #'emamux:yank-from-list-buffers)
    (when (emamux:in-tmux-p)
      (define-key map "\M-!" #'emamux:run-command)
      (define-key map "\M-r" #'emamux:run-last-command)
      (define-key map "\M-s" #'emamux:run-region)
      (define-key map "\C-i" #'emamux:inspect-runner)
      (define-key map "\C-k" #'emamux:close-panes)
      (define-key map "\C-c" #'emamux:interrupt-runner)
      (define-key map "\M-k" #'emamux:clear-runner-history)
      (define-key map "c"    #'emamux:new-window)
      (define-key map "C"    #'emamux:clone-current-frame)
      (define-key map "2"    #'emamux:split-window)
      (define-key map "3"    #'emamux:split-window-horizontally))
    map)
  "Default keymap for emamux commands. Use like
\(global-set-key (kbd \"M-g\") emamux:keymap\)

Keymap:

| Key | Command                          |
|-----+----------------------------------|
| C-s | emamux:send-command              |
| C-y | emamux:yank-from-list-buffers    |
| M-! | emamux:run-command               |
| M-r | emamux:run-last-command          |
| M-s | emamux:region                    |
| C-i | emamux:inspect-runner            |
| C-k | emamux:close-panes               |
| C-c | emamux:interrupt-runner          |
| M-k | emamux:clear-runner-history      |
| c   | emamux:new-window                |
| C   | emamux:clone-current-frame       |
| 2   | emamux:split-window              |
| 3   | emamux:split-window-horizontally |
")

(provide 'emamux)

;;; emamux.el ends here

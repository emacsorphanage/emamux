;;; emamux.el --- tmux manipulation from Emacs

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:

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

;;; History:
;; Revision 0.1  2012/07/???? syohex
;; Initial version
;;

;;; Code:

(eval-when-compile (require 'cl))

(defgroup emamux nil
  "tmux manipulation from Emacs"
  :group 'emamux)

(defvar emamux:session nil)
(defvar emamux:window nil)
(defvar emamux:pane nil)

(defvar emamux:last-command nil
  "Last emit command")

(defun emamux:tmux-running-p ()
  (with-temp-buffer
    (= (call-process-shell-command "tmux has-session") 0)))

(defun emamux:set-parameters ()
  (progn
    (emamux:set-parameter-session)
    (emamux:set-parameter-window)
    (emamux:set-parameter-pane)))

(defun emamux:set-parameters-p ()
  (and emamux:session emamux:window emamux:pane))

(defun emamux:set-parameter-session ()
  (let ((candidates (emamux:get-sessions)))
    (setq emamux:session
          (if (= (length candidates) 1)
              (car candidates)
            (completing-read "Input session: " candidates nil t)))))

(defun emamux:set-parameter-window ()
  (let* ((candidates (emamux:get-window))
         (selected (if (= (length candidates) 1)
                       (car candidates)
                     (completing-read "Input window: " candidates nil t))))
    (setq emamux:window (car (split-string selected ":")))))

(defun emamux:set-parameter-pane ()
  (let ((candidates (emamux:get-pane)))
    (setq emamux:pane
          (if (= (length candidates) 1)
              (car candidates)
            (completing-read "Input pane: " candidates)))))

(defun* emamux:target-session (&optional (session emamux:session)
                                         (window emamux:window)
                                         (pane emamux:pane))
  (format "%s:%s.%s" session window pane))

(defun emamux:get-sessions ()
  (with-temp-buffer
    (let ((ret (call-process-shell-command "tmux list-sessions" nil t nil)))
      (unless (= ret 0)
        (error "Failed 'tmux list-sessions'"))
      (goto-char (point-min))
      (let (sessions)
        (while (re-search-forward "^\\([^:]+\\):" nil t)
          (push (match-string-no-properties 1) sessions))
        sessions))))

(defun emamux:get-window ()
  (with-temp-buffer
    (let* ((cmd (format "tmux list-windows -t %s" emamux:session))
           (ret (call-process-shell-command cmd nil t nil)))
      (unless (= ret 0)
        (error (format "Faild %s" cmd)))
      (goto-char (point-min))
      (let (windows)
        (while (re-search-forward "^\\([0-9]+: [^ ]+\\)" nil t)
          (push (match-string-no-properties 1) windows))
        (reverse windows)))))

(defun emamux:get-pane ()
  (with-temp-buffer
    (let* ((cmd (format "tmux list-panes -t %s:%s" emamux:session emamux:window))
           (ret (call-process-shell-command cmd nil t nil)))
      (unless (= ret 0)
        (error (format "Faild %s" cmd)))
      (goto-char (point-min))
      (let (panes)
        (while (re-search-forward "^\\([0-9]+\\):" nil t)
          (push (match-string-no-properties 1) panes))
        (reverse panes)))))

(defun emamux:send-command ()
  "Send command to tmux target-session"
  (interactive)
  (unless (emamux:tmux-running-p)
    (error "'tmux' does not run on this machine!!"))
  (if (or current-prefix-arg (not (emamux:set-parameters-p)))
      (emamux:set-parameters))
  (let* ((prompt (format "Send to (%s): " (emamux:target-session)))
         (input (concat (read-string prompt emamux:last-command) "\n")))
    (emamux:set-buffer input)
    (emamux:paste-buffer)
    (setq emamux:last-command input)))

(defun emamux:set-buffer (input)
  (let* ((escaped (replace-regexp-in-string "'" "'\\\\''" input))
         (cmd (format "tmux set-buffer '%s'" escaped)))
    (unless (= (call-process-shell-command cmd nil nil nil) 0)
      (error "Failed tmux set-buffer"))))

(defun emamux:paste-buffer ()
  (let ((cmd (format "tmux paste-buffer -t %s" (emamux:target-session))))
    (unless (= (call-process-shell-command cmd nil nil nil) 0)
      (error "Failed tmux paste-buffer"))))

;;; emamux.el ends here

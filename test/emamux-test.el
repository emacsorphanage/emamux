;;; emamux-test.el --- Unit test for emamux.el -*- lexical-binding: t -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'emamux)

(ert-deftest find-active-pane-id ()
  "find active pane"
  (let ((data '("0: [194x19] [history 1/2000, 2718 bytes] %6 (active)"
                "1: [97x19] [history 2/2000, 2734 bytes] %7"
                "2: [96x19] [history 1/2000, 30 bytes] %8")))
    (let ((got (emamux:active-pane-id data)))
      (should (string= got "%6")))))

(ert-deftest find-nearlist-inactive-pane-id ()
  "find nearest pane"
  (let ((data '("0: [194x19] [history 1/2000, 2718 bytes] %6 (active)"
                "1: [97x19] [history 2/2000, 2734 bytes] %7"
                "2: [96x19] [history 1/2000, 30 bytes] %8")))
    (let ((got (emamux:nearest-inactive-pane-id data)))
      (should (string= got "%7")))))

(ert-deftest escape-semicolon ()
  "Escape semicolon if last character of input is ';'"
  (let ((got (emamux:escape-semicolon "ls -l;")))
    (should (string= got "ls -l\\;")))

  (let ((got (emamux:escape-semicolon "ls -l; cd /tmp")))
    (should (string= got "ls -l; cd /tmp"))))

;;; emamux-test.el ends here

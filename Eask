;; -*- mode: eask; lexical-binding: t -*-

(package "emamux"
         "0.14"
         "Interact with tmux")

(website-url "https://github.com/emacsorphanage/emacs-emamux")
(keywords "unix")

(package-file "emamux.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "26.1")

(development
 (depends-on "helm"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

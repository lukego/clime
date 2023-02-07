;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-clime
    :author "Luke Gorrie <https://github.com/lukego>"
    :depends-on (#:slynk)
  :description "CLIM for Emacs"
  :components ((:file "clime")))

;; Local Variables:
;; coding: utf-8
;; End:

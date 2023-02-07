;;; sly-hello-world.el --- A template SLY contrib  -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/capitaomorte/sly-hello-world
;; Keywords: languages, lisp, sly
;; Package-Requires: ((sly "1.0.0-beta2"))
;; Author: Luke Gorrie <lukego@gmail.com>
;;
;; Copyright (C) 2023 Luke Gorrie
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `sly-clime` provides "CLIM for Emacs" i.e. integration between Emacs and
;; graphical presentations from (Mc)CLIM.
;;
;; See README.md.
;;
;;; Code:

(require 'sly)

(define-sly-contrib sly-clime
  "Define the `sly-clime' contrib.
Depends on the `slynk-clime' ASDF system."
  (:slynk-dependencies slynk-clime)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-hello-world-mode))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-hello-world-mode)))

(defun clime-output-buffer ()
  )

(sly-def-connection-var clime-input-context nil
  "Current CLIM input context for the connection.
List of (THREAD TAG INPUT-CONTEXT)")

;;; Automatically add ourselves to `sly-contribs' when this file is loaded
;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-clime 'append)
  (add-to-list 'sly-event-hooks 'clime-dispatch-clime-event 'append))

(provide 'sly-clime)
;;; sly-hello-world.el ends here


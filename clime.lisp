(defpackage #:slynk-clime
  (:use :cl #:slynk-api)
  (:export
   #:accept-in-emacs))
(in-package #:slynk-clime)

(defun accept-in-emacs (acceptable-presentation-ids)
  "Ask user to accept a CLIM presentation."
  (check-type acceptable-presentation-ids list)
  (dolist (id acceptable-presentation-ids)
    (check-type id (integer 0 *)))
  (let ((tag (make-tag)))
    (force-output)
    (send-to-emacs `(:accept-for-clime ,(current-thread-id)
                                       ,tag
                                       ,acceptable-presentation-ids))
    (third (wait-for-event `(:emacs-return ,tag result)))))

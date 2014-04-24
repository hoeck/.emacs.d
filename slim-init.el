;;; Load init.el with the slim-init parameter set to t.
;;;
;;; This allows me using emacs as a GIT_EDITOR with my normal customizations
;;; set (font-lock, certain keys, ...) but without the long startup overhead.
;;;
;;; USAGE:
;;;     emacs -Q -l <full-path-to>/slim-init.el $@ 2> /dev/null
;;;

(defvar slim-init t)
(load "/home/soehnel/.emacs.d/init.el" t t)

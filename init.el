;;; folklore: initially taken from my first linux distro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File name: ` ~/.xemacs/init.el '
;;; ---------------------
;;;
;;; Copyright (c) 2002 SuSE Gmbh Nuernberg, Germany.
;;;
;;; Author: Werner Fink, <feedback@suse.de> 2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slim loading, skip expensive inits, see slim-init.el and ~/bin/em
(when (not (boundp 'slim-init))
  (defvar slim-init nil))

(setq custom-file "~/.emacs.d/custom.el")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; eat up all whitespace when pressing C-d
;;(c-toggle-hungry-state 1)

;;; huge killring
(setq-default kill-ring-max 500)

;;; allow copy and paste with other applications
(setq x-select-enable-clipboard t)

;;; get rid of ui chrome
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-splash-screen t)

;;; show paren match mode
(show-paren-mode 2)

;; pressing C-BS eats up all whitespace!
(require 'cc-mode)
(global-set-key [(control backspace)] 'c-hungry-backspace)

;; dabbrev
(global-set-key "\M--" 'dabbrev-expand)
(global-set-key "\M-_" 'dabbrev-completion)

;; I mostly use it to format docstrings
(global-set-key "\M-q" 'fill-region)

;; pabbrev, scans buffers for most used words
;; hitting (default: tab) completes, hitting (default: tab) twice pops up a selection buffer
(require 'pabbrev)
(define-key pabbrev-mode-map "\M--" 'pabbrev-expand-maybe)
(define-key pabbrev-mode-map "TAB" 'undefined)

;; yeah
(setq-default truncate-lines t)

;; Interactively Do Things
(require 'ido)

;;         _     _____    _     __   ___      _
;; |\  |  / \      |     /_\   |__> <___     | |
;; | \ | |   |     |    /   \  |  \     \    |_|
;; |  \|  \_/      |   /     \ |__/ ____/    OOO
;; ---------------------------------------------
(setq-default indent-tabs-mode nil)
;; ps: use M-x untabify to for previous atrocities

;;  _   _  ___    _____ ____      _    ___ _     ___ _   _  ____   ____  ____   _    ____ _____ ____  _
;; | \ | |/ _ \  |_   _|  _ \    / \  |_ _| |   |_ _| \ | |/ ___| / ___||  _ \ / \  / ___| ____/ ___|| |
;; |  \| | | | |   | | | |_) |  / _ \  | || |    | ||  \| | |  _  \___ \| |_) / _ \| |   |  _| \___ \| |
;; | |\  | |_| |   | | |  _ <  / ___ \ | || |___ | || |\  | |_| |  ___) |  __/ ___ \ |___| |___ ___) |_|
;; |_| \_|\___/    |_| |_| \_\/_/   \_\___|_____|___|_| \_|\____| |____/|_| /_/   \_\____|_____|____/(_)
;; ---------------------------------------------
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; backup files go into .emacs.d!
;;; credits to: http://snarfed.org/space/gnu_emacs_backup_files

(setq-default make-backup-files t)
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir (concat "~/.emacs.d/emacs_autosaves/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
     (replace-regexp-in-string "/" "-" (expand-file-name
                                        (concat "#%" (buffer-name) "#"))))))

;;; don't know how that plays with the code from above
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "~/.emacs.d/emacs_backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; don't create file-lock-symlicks (files startingwith ".#")
;; no one is editing files on the same computer anymore
(setq create-lockfiles nil)

;;; request syntax highlighting
(global-font-lock-mode 1)

;;; delete all text in the active region when writing or pasting to it
;;; (just like all the other editors out there)
(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colors
(require 'font-lock)
(set-face-foreground 'font-lock-string-face "green4")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window enlargement & shrunkenizing

(defun turbo-enlarge-window (arg)
  (interactive "p")
  (enlarge-window (* (or arg 1) 3)))

(defun turbo-shrink-window (arg)
  (interactive "p")
  (shrink-window (* (or arg 1) 3)))

(defun turbo-enlarge-window-horizontally (arg)
  (interactive "p")
  (enlarge-window-horizontally (* (or arg 1) 6)))

(defun turbo-shrink-window-horizontally (arg)
  (interactive "p")
  (shrink-window-horizontally (* (or arg 1) 6)))


(global-set-key [(control *)] 'turbo-shrink-window) ;; c-* is actually c shift + on a german keyboard
(global-set-key [(control _)] 'turbo-enlarge-window) ;; c-_ is actually c shift - on a german keyboard
(global-set-key [(meta control -)] 'turbo-shrink-window-horizontally)
(global-set-key [(meta control +)] 'turbo-enlarge-window-horizontally)

;; resize window vertically: shift + meta and [+] or [-]
;;
;;   | [SHIFT]
;;   | [CTRL]       [ALT]
;;      `-------------´
;;          resize current window horizontally: ctrl + meta and [+] or [-]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings for query replace
(global-set-key [(control \#)] 'query-replace)
(global-set-key [(meta control \#)] 'query-replace-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; comment-or-uncomment-region
(global-set-key [(control \;)] 'comment-or-uncomment-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs internal window movement
(global-set-key [(shift right)] 'windmove-right)
(global-set-key [(shift left)] 'windmove-left)
(global-set-key [(shift down)] 'windmove-down)
(global-set-key [(shift up)] 'windmove-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'transpose-frame)
;; see transpose-frame.el in .emacs.d
;; commands: flop-frame, flip-frame, transpose-frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; startup
(when (not slim-init)
    ;; fat startup
    (require 'package)
    (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                             ("marmalade" . "http://marmalade-repo.org/packages/")
                             ("melpa" . "http://melpa.org/packages/")))

    ;; manually control package loading (at the end of this file)
    (setq package-enable-at-startup nil)

    (package-initialize)

    (load "~/.emacs.d/custom.el" t t) ;; load custom options *after* initing the packages
    (load "~/.emacs.d/init-packages.el" t t)

    ;; org files are required for my normal workspace
    (find-file "~/org/notes.org")
    (find-file "~/org/current.org")
    (switch-to-buffer "notes.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs strange/old custom options

(put 'scroll-left 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(put 'erase-buffer 'disabled nil)

(put 'upcase-region 'disabled nil)

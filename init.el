;;; folklore: initially taken from my first linux distro:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File name: ` ~/.xemacs/init.el '
;;; ---------------------
;;;
;;; Copyright (c) 2002 SuSE Gmbh Nuernberg, Germany.
;;;
;;; Author: Werner Fink, <feedback@suse.de> 2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/custom.el" t t)
(add-to-list 'load-path "~/.emacs.d")

;; eat up all whitespace when pressing C-d
;;(c-toggle-hungry-state 1)

;;; huge killring
(setq-default kill-ring-max 500)

;;; allow copy and paste with other applications
(setq x-select-enable-clipboard t)

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

;; re-builder: interactively build and test regexes
(require 're-builder)
;; use the same regexp syntax as in m-x *-regexp commands
;; (defaults to 'read: the syntax used in elisp strings where each '\' has to be quoted)
(setq reb-re-syntax 'string)

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
(fmakunbound 'lock-buffer)
(defun lock-buffer (bla))

;; Shell mode colors (m-x shell)
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; make the prompt read-only:
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)
(defun shell-clear ()
   (interactive)
   (let ((comint-buffer-maximum-size 0))
     (comint-truncate-buffer)))

;; artist binds C-cC-c to artist-mode-off for leaving artist
(autoload 'artist-mode "artist" "Enter artist-mode" t)

;;; request syntax highlighting
(global-font-lock-mode 1)

;;; delete all text in the active region when writing or pasting to it
;;; (just like all the other editors out there)
(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colors, mostly font-lock
(require 'font-lock)
(set-face-background 'default "RGB:FFFF/EAF6/CB8B")
(set-face-foreground 'default "black")
(set-face-background 'show-paren-match "GoldenRod1")
;; font locks
(set-face-foreground 'font-lock-comment-face "#d12f00");;"#ed1600") ;;"#e8410c") ;;"#E82C0C") ;;OrangeRed2

(set-face-foreground 'font-lock-string-face "green4")
(set-face-foreground 'font-lock-doc-face "green4")
(set-face-foreground 'font-lock-function-name-face "MediumBlue")
(set-face-foreground 'font-lock-keyword-face "CornflowerBlue") ; lisp 'reserved makros' (if then else defun ...)
;;(set-face-foreground 'font-lock-reference-face "DarkOrange2") ; lisp ':keywords'
(set-face-foreground 'font-lock-variable-name-face "#7F0C00") ;; SandyBrown2") ;;SandyBrown")
(set-face-foreground 'font-lock-type-face "cyan3")
(set-face-foreground 'font-lock-constant-face "LightSalmon2")
(set-face-foreground 'font-lock-warning-face "DarkKhaki")
(set-face-foreground 'font-lock-preprocessor-face "SpringGreen")

(set-face-background 'fringe "RGB:EFFF/DAF6/AB8B")
(set-face-background 'cursor "#000000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; redefine some paredit keys
(require 'paredit)
;; (define-key paredit-mode-map "\C-j" nil) ;; use C-j for lisp-eval-print
;; (define-key paredit-mode-map ";" nil) ;; i want normal comments or a `comment-next-expr' function
;; (define-key paredit-mode-map ")" 'paredit-close-parenthesis)
;; (define-key paredit-mode-map [(meta right)] 'paredit-forward)
;; (define-key paredit-mode-map [(meta left)] 'paredit-backward)

;; can't live without
(global-set-key [(meta left)] 'paredit-backward)
(global-set-key [(meta right)] 'paredit-forward)

(eval-after-load 'paredit

  '(progn
     ;; do not treat strings as s-expressions when beeing inside them and trying to escape!
     (defun paredit-at-string-start ()
       (if (paredit-in-string-p)
           (eql (+ 1 (first (paredit-string-start+end-points))) (point))))

     (defun paredit-at-string-end ()
       (if (paredit-in-string-p)
           (eql (cdr (paredit-string-start+end-points)) (point))))

     (defun-saving-mark paredit-backward ()
       "like paredit-backward with different in-string movement.
When at the start of a string, do not move to the
end of the string before - just skip the string delim char."
       (paredit-handle-sexp-errors
           (if (paredit-at-string-start)
               (backward-char)
             (backward-sexp))
         (if (paredit-in-string-p) (backward-char) (backward-up-list))))

     (defun-saving-mark paredit-forward ()
       "like paredit-forward with different in-string movement.
When at the end of a string, do not move to the
start of the next string - just skip the string delim char."
       (paredit-handle-sexp-errors
           (if (paredit-at-string-end)
               (forward-char)
             (forward-sexp))
         (if (paredit-in-string-p) (forward-char) (up-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;;(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; java abbrevs
;; (require 'jde)
;; ;; for jde-mode
;; (define-abbrev java-mode-abbrev-table "fori" "for(int i=0;i<UL;i++)")
;; (define-abbrev java-mode-abbrev-table "forit" "for(Iterator i = c.iterator(); i.hasNext();)")
;; (define-abbrev java-mode-abbrev-table "fore" "for(type x : coll")
;; (define-abbrev java-mode-abbrev-table "pr" "System.out.println();")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; webjump stuff
(require 'webjump)
(global-set-key [f2] 'webjump)
(setq webjump-sites
      (append '(("ddg" . [simple-query "www.duckduckgo.com" "http://www.duckduckgo.com/?q=" ""])
                ("duckduckgo" . [simple-query "www.duckduckgo.com" "http://www.duckduckgo.com/?q=" ""])
                ("reddit-search" .
                 [simple-query "www.reddit.com" "http://www.reddit.com/search?q=" ""])
                ("google-image-search" .
                 [simple-query "images.google.com" "images.google.com/images?hl=en&q=" ""])
                ("flickr-search" .
                 [simple-query "www.flickr.com" "flickr.com/search/?q=" ""])
                ("leo-search" .
                 [simple-query "dict.leo.org" "dict.leo.org/ende?search=" ""])
                ("wikipedia-search" .
                 [simple-query "www.wikipedia.org" "en.wikipedia.org/wiki/Special:Search?search=" "&go=Go"])
                ("wikipedia-de-search" .
                 [simple-query "www.wikipedia.de" "de.wikipedia.org/wiki/Special:Search?search=" "&go=Go"])
                ("google-maps-search" .
                 [simple-query "maps.google.com" "http://maps.google.com/maps?&q=" ""])
                ("fhdw-stundenplan" .
                 "https://ims-dd.bib.de/plan")
                ("clojure-group" .
                 "http://groups.google.com/group/clojure/topics")
                ("xkcd" .
                 "www.xkcd.org")
                ("fefe" .
                 "blog.fefe.de")
                ;;; java and clojure docs, local and www
                ("javadoc" .
                 "file:///usr/share/doc/sun-java6-doc/html/api/allclasses-noframe.html")
                ("javadoc-overview" .
                 "file:///usr/share/doc/sun-java6-doc/html/index.html")
                ("jbox2d-doc-all-classes" .
                 "file:///home/timmy-turner/src/jbox2d/trunk/doc/allclasses-noframe.html")
                ("processing-doc-PApplet" .
                 "file:///home/timmy-turner/src/processing/build/javadoc/core/allclasses-noframe.html")
                ("processing" .
                 "file:///home/timmy-turner/src/processing/build/linux/work/reference/index.html")
                ("jgraph-javadoc" .
                 "file:///home/timmy-turner/src/jgraph5/doc/api/allclasses-noframe.html")
                ("pivot-javadoc" .
                 "file:///home/timmy-turner/src/apache-pivot-1.3-incubating/doc/allclasses-noframe.html")
                ("ant-doc" .
                 "file:///home/timmy-turner/doc/ant-doc/manual/index.html")
                ;;; pyrat
                ("pyrat-trunk-admin" . "http://localhost/pyrat-trunk/cgi-bin/login.py?username=admin&password=admin")
                ("pyrat-stable-admin" . "http://localhost/pyrat-trunk/cgi-bin/login.py?username=admin&password=admin")
                )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
;;(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; files to search through for agenda view
(setq org-agenda-files (list "~/org/notes.org"))
;; ditaa, see http://ditaa.sourceforge.net/#usage (render text-drawn diagrams)
(add-to-list 'load-path "~/src/org-mode/contrib/lisp")
;;(require 'org-babel-init)
;;(require 'org-babel-dot)
;;(require 'org-babel-ditaa)
;;(org-babel-load-library-of-babel)
;;(setq org-ditaa-jar-path "~/src/ditaa/trunk/releases/ditaa0_9.jar")
(require 'org) ;; for org-mode-map
(define-key org-mode-map [(meta left)] 'paredit-backward)
(define-key org-mode-map [(meta right)] 'paredit-forward)

;;; org remember (capture), see: http://orgmode.org/manual/Capture.html#Capture
(setq org-default-notes-file "~/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("l" "add a link" item (file+olp "~/org/notes.org" "links" "misc")
         "- %?\n")
        ;; write down and clock admin tasks on various servers
        ("a" "admin journal")
        ("am" "admin journal: Martinsried" entry (file+datetree "~/org/admin-journal.org")
         "* %U %? :martinsried:\n"
         :clock-in t
         )))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mail
;; (setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p")
;; (setq gnus-select-method '(nnimap "dd.bib.de" ;; port 143
;;                                   (nnimap-address "mail.dd.bib.de")
;;                                   (nnimap-stream ssl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/src/clojure-mode")
;; (require 'clojure-mode)

;;; clojure & slime
;; (add-to-list 'load-path "~/clj/clojure-mode")
;; (add-to-list 'load-path "~/clj/swank-clojure")
;; (add-to-list 'load-path "~/src/clojure-mode")
;; (require 'clojure-mode)
;; (define-key slime-mode-map [(control j)] 'slime-eval-print-last-expression)
;; (add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python mode customs
(require 'python-mode)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(define-key py-mode-map (read-kbd-macro "TAB") 'py-indent-line)

(defun py-attrib-to-key ()
  "Convert a python attribute expression to a key lookup expression.

Example (-!- is the point):

  object.attribute_name-!-    => object['attribute_name']-!-
  object.attr-!-ibute_name    => object['attribute_name']-!-
"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (if (and bounds
             (string= (buffer-substring (- start 1) start) "."))
        (let* ((word (delete-and-extract-region (- start 1) end))
               (key (format "['%s']" (substring word 1))))
          (insert key))
      (message "No attribute at point."))))
(define-key py-mode-map [(control .)] 'py-attrib-to-key)

(require 'paredit) ;; for paren navigation
(define-key py-mode-map [(meta left)] 'paredit-backward)
(define-key py-mode-map [(meta right)] 'paredit-forward)
(define-key py-mode-map [(meta n)] 'py-next-statement)
(define-key py-mode-map [(meta down)] 'py-next-statement)
(define-key py-mode-map [(meta p)] 'py-previous-statement)
(define-key py-mode-map [(meta up)] 'py-previous-statement)

;;(define-key python-mode-map [(control x) (control e)] 'py-execute-region)
;;(define-key python-mode-map [(control c) (control r)] 'py-execute-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ipython via py-shell
(require 'ipython)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YaSnippet v0.6.1c
(add-to-list 'load-path"~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
;; see http://yasnippet.googlecode.com/svn/trunk/doc/snippet-organization.html
;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(setq yas/root-directory "~/.emacs.d/mysnippets")
(yas/load-directory yas/root-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javascript

;; currently I like js2-mode more (saner indentation and flymake runs jslint just fine)
(require 'paredit) ;; for paren navigation in js buffers
(defun js-custom-setup ()
  (define-key js2-mode-map [(meta left)] 'paredit-backward)
  (define-key js2-mode-map [(meta right)] 'paredit-forward))
(add-hook 'js2-mode-hook 'js-custom-setup)
(add-hook 'js2-mode-hook 'flymake-jslint-load)

;; keep j3 mode around though, just don't autoload it
;; see http://www.emacswiki.org/emacs/ELPA for how to configuring ELPA packages
(eval-after-load "js3-mode-autoloads"
  '(progn
     (setq auto-mode-alist (delete (rassoc 'js3-mode auto-mode-alist) auto-mode-alist))))

(eval-after-load "js2-mode-autoloads"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
     (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'anything)
;; (require 'anything-config)
;; (require 'anything-match-plugin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (my)sql-completion
;; disabled for now, does not work so well
;; (require 'sql-completion)
;; (setq sql-interactive-mode-hook
;;       (lambda ()
;;         (define-key sql-interactive-mode-map "\M--" 'comint-dynamic-complete)
;;         (sql-mysql-completion-init)))

(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(defun my-sql-mode-hook ()
  (define-key sql-mode-map (kbd "RET") 'newline-and-indent)

  ;; Make # start a new line comment in SQL. This is MySQL-specific
  ;; syntax.

  (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
  (set-syntax-table sql-mode-syntax-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs internal window movement
(global-set-key [(shift right)] 'windmove-right)
(global-set-key [(shift left)] 'windmove-left)
(global-set-key [(shift down)] 'windmove-down)
(global-set-key [(shift up)] 'windmove-up)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; breadcrump mode - set temporary bookamrks
(require 'breadcrumb)

(global-set-key [(shift space)]         'bc-set)            ;; Shift-SPACE for set bookmark
(global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
(global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
(global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
(global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
(global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
(global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'transpose-frame)
;; see transpose-frame.el in .emacs.d
;; commands: flop-frame, flip-frame, transpose-frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; startup
(find-file "~/org/notes.org")
(switch-to-buffer "notes.org")
(menu-bar-mode 0)
(tool-bar-mode 0)


(put 'scroll-left 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(put 'erase-buffer 'disabled nil)

(put 'upcase-region 'disabled nil)

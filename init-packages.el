;;;
;;; package-specific customizazions which are loaded after pacakge-initialize
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; visual bookmark mode
;;; see https://github.com/joodland/bm

;; visual studio uses these keys too
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<f3>") 'bm-previous)
(global-set-key (kbd "<S-f2>") 'bm-first-in-next-buffer)
(global-set-key (kbd "<S-f3>") 'bm-last-in-previous-buffer)

;; click on fringe to toggle bookmarks, and use mouse wheel to move between them
(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(setq bm-in-lifo-order t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; artist-mode (ascii art)

;; artist binds C-cC-c to artist-mode-off for leaving artist
(autoload 'artist-mode "artist" "Enter artist-mode" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; re-builder

;; interactively build and test regexes
(require 're-builder)
;; use the same regexp syntax as in m-x *-regexp commands
;; (defaults to 'read: the syntax used in elisp strings where each '\' has to be quoted)
(setq reb-re-syntax 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; comint mode

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paredit
(require 'paredit)
(require 'paredit-everywhere)
;; (require 'paredit)
;; (define-key paredit-mode-map "\C-j" nil) ;; use C-j for lisp-eval-print
;; (define-key paredit-mode-map ";" nil) ;; i want normal comments or a `comment-next-expr' function
;; (define-key paredit-mode-map ")" 'paredit-close-parenthesis)
;; (define-key paredit-mode-map [(meta right)] 'paredit-forward)
;; (define-key paredit-mode-map [(meta left)] 'paredit-backward)

;; can't live without
(global-set-key [(meta left)] 'paredit-backward)
(global-set-key [(meta right)] 'paredit-forward)

;; (eval-after-load 'paredit
;;
;;   '(progn
;;      ;; do not treat strings as s-expressions when beeing inside them and trying to escape!
;;      (defun paredit-at-string-start ()
;;        (if (paredit-in-string-p)
;;            (eql (+ 1 (first (paredit-string-start+end-points))) (point))))
;;
;;      (defun paredit-at-string-end ()
;;        (if (paredit-in-string-p)
;;            (eql (cdr (paredit-string-start+end-points)) (point))))
;;
;;      (defun-saving-mark paredit-backward ()
;;        "like paredit-backward with different in-string movement.
;; When at the start of a string, do not move to the
;; end of the string before - just skip the string delim char."
;;        (paredit-handle-sexp-errors
;;            (if (paredit-at-string-start)
;;                (backward-char)
;;              (backward-sexp))
;;          (if (paredit-in-string-p) (backward-char) (backward-up-list))))
;;
;;      (defun-saving-mark paredit-forward ()
;;        "like paredit-forward with different in-string movement.
;; When at the end of a string, do not move to the
;; start of the next string - just skip the string delim char."
;;        (paredit-handle-sexp-errors
;;            (if (paredit-at-string-end)
;;                (forward-char)
;;              (forward-sexp))
;;          (if (paredit-in-string-p) (forward-char) (up-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
(require 'org) ;; for org-mode-map

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

;; files to search through for agenda view
(setq org-agenda-files (list "~/org/notes.org"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure
(require 'cider)

(defun cider-eval-last-expression-pprint ()
  (interactive)
  (let* ((buffer (current-buffer))
          (handler (nrepl-make-response-handler buffer
                                                (lambda (buffer str)
                                                  (with-current-buffer buffer
                                                    (insert (format ";;> %s\n" str))))
                                                (lambda (buffer str)
                                                  (with-current-buffer buffer
                                                    (dolist (x (butlast (split-string str "\n")))
                                                      (insert (format ";;> %s\n" x)))))
                                                (lambda (buffer str)
                                                  (nrepl-emit-into-popup-buffer buffer str))
                                                '())))
    (insert "\n")
    (cider-interactive-eval (format "((if *clojure-version* clojure.pprint/pprint identity) %s)" (cider-last-sexp))
                            handler)))

(define-key cider-mode-map [(control \j)] 'cider-eval-last-expression-pprint)

(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)

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

;; (require 'paredit) ;; for paren navigation
(define-key py-mode-map [(meta left)] 'paredit-backward)
(define-key py-mode-map [(meta right)] 'paredit-forward)
(define-key py-mode-map [(meta n)] 'py-next-statement)
(define-key py-mode-map [(meta down)] 'py-next-statement)
(define-key py-mode-map [(meta p)] 'py-previous-statement)
(define-key py-mode-map [(meta up)] 'py-previous-statement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ipython via py-shell
;;(require 'ipython)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YaSnippet
(require 'yasnippet)
;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(setq yas/root-directory "~/.emacs.d/mysnippets")
(yas/load-directory yas/root-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javascript

;; I used to use flymake, but now its jscs + jshint. flycheck works
;; flawlessly with two linters, so I don't need flymake any more.
;; (add-hook 'js2-mode-hook 'flymake-jshint-load)

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
;; breadcrump mode - set temporary bookamrks
(require 'breadcrumb)

(global-set-key [(shift space)]         'bc-set)            ;; Shift-SPACE for set bookmark
(global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
(global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
(global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
(global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
(global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
(global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ispell - fix german dictionary
(require 'ispell)
(add-to-list 'ispell-dictionary-alist '("de_DE-neu" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-C") "~tex" utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; customize the time after which the company auto completion popup opens
;; its slow for js2-mode
(setq company-idle-delay 1.5)

;; Reduce the number of characters before company kicks in
(setq company-minimum-prefix-length 2)

;; keychords
(global-set-key [(control .)] 'company-complete)
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)

;; nicer completion listing for staticically typed languages
(setq company-tooltip-align-annotations t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm Global Mode!
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile - Project Interaction Library
;; https://github.com/bbatsov/projectile
(projectile-global-mode)
(setq projectile-switch-project-action 'helm-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace Jump
;; http://www.emacswiki.org/emacs/AceJump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Move
;; http://www.emacswiki.org/emacs/buffer-move.el
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck JSCS
;; https://github.com/jscs-dev/node-jscs
(require 'flycheck)

(flycheck-def-config-file-var flycheck-jscs javascript-jscs ".jscsrc" :safe #'stringp)

(flycheck-define-checker javascript-jscs
  "A JavaScript code style checker.
See URL `https://github.com/mdevils/node-jscs'."
  :command ("jscs" "--reporter" "checkstyle"
            (config-file "--config" flycheck-jscs)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (js-mode js2-mode js3-mode))

;;; custom eshint definition without source-inplace to not disturb the grunt
;;; watch process with generating temporary files

(flycheck-define-checker javascript-eslint-custom
  "A Javascript syntax and style checker using eslint.

See URL `https://github.com/eslint/eslint'."
  :command ("eslint" "--format=checkstyle"
            (config-file "--config" flycheck-eslintrc)
            source)
  :error-parser flycheck-parse-checkstyle
  :error-filter (lambda (errors)
                  (mapc (lambda (err)
                          ;; Parse error ID from the error message
                          (setf (flycheck-error-message err)
                                (replace-regexp-in-string
                                 (rx " ("
                                     (group (one-or-more (not (any ")"))))
                                     ")" string-end)
                                 (lambda (s)
                                   (setf (flycheck-error-id err)
                                         (match-string 1 s))
                                   "")
                                 (flycheck-error-message err))))
                        (flycheck-sanitize-errors errors))
                  errors)
  :modes (js-mode js2-mode js3-mode)
  :next-checkers ((warning . javascript-jscs)))

(add-to-list 'flycheck-disabled-checkers 'javascript-eslint)
(add-to-list 'flycheck-checkers 'javascript-eslint-custom)

;; use the projects eslint if available (in node_modules)
(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (file-executable-p eslint)
      (setq-local flycheck-javascript-eslint-executable eslint)
      (setq-local flycheck-javascript-eslint-custom-executable eslint))))

(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tern (http://ternjs.net)

(add-to-list 'load-path "~/src/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; The Emacs mode uses the bin/tern server, and project configuration is done
;; with a .tern-project file.
;;
;; Buffers in tern-mode add a completion-at-point function that activates Tern’s
;; completion. So, unless you rebound the key, M-tab (or C-M-i) will trigger
;; completion.
;;
;; When the point is in an argument list, Tern will show argument names and types
;; at the bottom of the screen.
;;
;; The following additional keys are bound:
;;
;; M-.     .. Jump to the definition of the thing under the cursor.
;; M-,     .. Brings you back to last place you were when you pressed M-..
;; C-c C-r .. Rename the variable under the cursor.
;; C-c C-c .. Find the type of the thing under the cursor.
;; C-c C-d .. Find docs of the thing under the cursor. Press again to open the associated URL (if any).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shackle (https://github.com/wasamasa/shackle)
;; enforce rules for popup windows
;; does not work with helms built-in documentation via C-c ? or C-h m
;; (require 'shackle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust-mode
(require 'rust-mode)

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racer (https://github.com/phildawes/racer)
;; Rust auto completion
(require 'racer)
(require 'company-racer)

;; Set path to racer binary
;; (build and install by cloning the racer git reboot and using cargo build)
(setq racer-cmd "/home/eriksoehnel/src/racer/target/release/racer")

;; Set path to rust src directory
(setq racer-rust-src-path "/home/eriksoehnel/src/rustc-1.3.0/src")

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight Symbol Mode
(require 'highlight-symbol)

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

(add-hook 'js-mode-hook 'highlight-symbol-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree global commands
(require 'neotree)

(defun neotree-force-find ()
  (interactive)
  (when (get-buffer "*NeoTree*")
    (message "killing neotree buffer")
    (kill-buffer "*NeoTree*"))
  (neotree-show)
  (neotree-find))

(define-key my-keys-minor-mode-map (kbd "C-c C-n") 'neotree-force-find)

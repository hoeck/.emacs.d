;;;
;;; package-specific customizations which are loaded after pacakge-initialize
;;;

;;; Code:

;;; but first syncronize the packages across all emacs installs
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (let ((refresh-needed t))
    (mapcar
     (lambda (package)
       (unless (package-installed-p package)
         (unless refresh-needed
           (package-refresh-contents)
           (setq refresh-needed nil))
         (package-install package)))
     packages)))

;; make sure to have downloaded an archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; package list
(ensure-package-installed
 'ac-cider
 'ace-jump-mode
 'async
 'auto-complete
 'auto-highlight-symbol
 'autopair
 'bm
 'browse-kill-ring
 'buffer-move
 'cider
 'clojure-mode
 ;; 'clojurescript-mode
 'coffee-mode
 'color-theme
 ;; 'color-theme-railscasts
 'company
 'company-racer
 'csharp-mode
 'dash
 'default-text-scale
 'deferred
 'egg
 'epl
 'f
 'flycheck
 'flycheck-rust
 'flymake-easy
 'flymake-python-pyflakes
 'git-commit
 'go-mode
 'haskell-mode
 'helm
 'helm-core
 'helm-dash
 'helm-projectile
 'highlight-symbol
 ;; 'idle-highlight
 'js2-mode
 'js3-mode
 'let-alist
 'magit
 'magit-popup
 'markdown-mode
 'markdown-mode+
 ;; 'minimap
 'multiple-cursors
 'neotree
 'nyan-mode
 'php-mode
 'pkg-info
 'popup
 'projectile
 'queue
 'racer
 'rust-mode
 's
 ;; 'scala-mode2
 'scss-mode
 'seq
 'shackle
 'smartparens
 'spinner
 'sql-indent
 'sr-speedbar
 'tabbar
 'tagedit
 'tide
 'typescript-mode
 'vimish-fold
 'web-mode
 'with-editor
 'yaml-mode
 'yasnippet)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
(require 'org) ;; for org-mode-map

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

;; files to search through for agenda view
(setq org-agenda-files (list "~/org"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python mode customs
(require 'python-mode)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(define-key py-mode-map (read-kbd-macro "TAB") 'py-indent-line)

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
(global-set-key (kbd "M-x") #'helm-M-x)

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
;; Flycheck
(require 'flycheck)

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
  :modes (js-mode js2-mode js3-mode))

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

;; custom tslint checker (forgot why I actually did this: I was initially
;; supporting the --project option to configure tslint via tsconfig.json but
;; that stopped working after a typescript update so I'm now back to plain
;; tslint.json config and it still works like I expect it)
(flycheck-def-config-file-var flycheck-typescript-tslint-custom-tslint-json typescript-tslint "tslint.json"
  :safe #'stringp)

(flycheck-define-checker typescript-tslint-custom
  "A Typescript syntax and style checker using eslint.

See URL `https://palantir.github.io/tslint/'."
  :command ("tslint"
            "--format" "checkstyle"
            ;; Note: --type-check does not work when linting single only files, as
            ;; flycheck does it on save need to use a commandline task for
            ;; that or use https://github.com/angelozerr/tslint-language-service
            ;; (integrates tslint it into tsserver and makes flycheck-tslint obsolete)
            (config-file "--config" flycheck-typescript-tslint-custom-tslint-json)
            source)
  :error-parser (lambda (output checker buffer)
                  ;; remove everything (tslint warnings) before the opening xml tag
                  ;; you get these warnings when using any rules that require --type-check
                  (let ((output-without-warnings (replace-regexp-in-string "\\(.\\|\n\\)*<?xml version=\"" "<?xml version=\"" output)))
                    (flycheck-parse-checkstyle output-without-warnings checker buffer)))
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
  :modes (tide-mode typescript-mode web-mode))

(add-to-list 'flycheck-checkers 'typescript-tslint-custom)

;; use the projects tslint if available (in node_modules)
(defun use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/.bin/tslint"
                                        root))))
    (when (file-executable-p tslint)
      (setq-local flycheck-typescript-tslint-custom-executable tslint))))

(add-hook 'flycheck-mode-hook #'use-tslint-from-node-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tern (http://ternjs.net)

;; (add-to-list 'load-path "~/src/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)

;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))
;;
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))

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

;; turn of tooltips in neotree, they are useless, annoying and cause rendering glitches
(defun setup-neotree-mode ()
  (tooltip-mode 0))

(add-hook 'neotree-mode-hook #'setup-neotree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C-c m d") 'mc/mark-all-dwim)
(global-set-key (kbd "C-c m s") 'mc/mark-all-symbols-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scaling text like a browser *for all buffers*
(require 'default-text-scale)

(global-set-key (kbd "C-+") 'default-text-scale-increase)
(global-set-key (kbd "C--") 'default-text-scale-decrease)
;; (global-set-key (kbd "C-0") (lambda () (interactive) (default-text-scale-set 0))) TODO: provide a patch to https://github.com/purcell/default-text-scale/blob/master/default-text-scale.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smartparens
(require 'smartparens-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tide (typescript ide) mode
(require 'typescript-mode)
(require 'tide)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save) ;; disabled for now

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; put tslint as the last one into the chain of fly-checkers, but only if
;; compilation did not produce any warnings
(flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint-custom))
(flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint-custom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-mode
(require 'web-mode)

;; js + tsx support
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(defun custom-web-mode-typescript-hook ()
  (when (and (equal web-mode-content-type "jsx")
             (string-match-p "\\.tsx$" buffer-file-name))
    (setup-tide-mode)))

(add-hook 'web-mode-hook  'custom-web-mode-typescript-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sensible unique buffer names
(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p nil)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")   ; don't muck with special buffers

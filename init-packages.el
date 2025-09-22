;;;
;;; package-specific customizations which are loaded after pacakge-initialize
;;;

;;; Code:

;; fix emacs-bug which prevents using elpa:
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
 'ace-jump-mode
 'all-the-icons
 'async
 'auto-complete
 'auto-highlight-symbol
 'autopair
 'bm
 'browse-kill-ring
 'buffer-move
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
 'dockerfile-mode
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
 ;; 'helm-swoop
 'highlight-symbol
 ;; 'idle-highlight
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
 'prettier ;; https://github.com/jscheid/prettier.el
 'projectile
 ;; 'queue -- obsolete?
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
 'unicode-fonts
 'vimish-fold
 'vterm ; required for claude-code.el
 'wgrep
 'wgrep-helm
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YaSnippet
(require 'yasnippet)
;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(setq yas/root-directory "~/.emacs.d/mysnippets")
(yas/load-directory yas/root-directory)

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

;; open grep lines in another window by default instead of the same window as the grep buffer
(define-key helm-grep-mode-map (kbd "RET") 'helm-grep-mode-jump-other-window) ;; defaults to 'helm-grep-mode-jump
(define-key helm-grep-mode-map (kbd "C-<return>") 'helm-grep-mode-jump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile - Project Interaction Library
;; https://github.com/bbatsov/projectile
(projectile-global-mode)
(setq projectile-switch-project-action 'helm-projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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

(defun neotree-helm-do-grep-ag (arg)
  "`helm-do-grep-ag but in the directory the cursor is under in neotree"
  (interactive "P")
  (require 'helm-files)
  (let* ((current-file-or-dir (with-current-buffer (neo-global--get-buffer)
                               (neo-buffer--get-filename-current-line)))
         (current-dir (if (file-directory-p current-file-or-dir)
                          current-file-or-dir
                        (file-name-directory current-file-or-dir))))
    (helm-grep-ag current-dir arg)))

(defun setup-neotree-mode ()
  ;; disable my conflicting keymaps inside neotree
  (my-keys-minor-mode 0)
  ;; turn of tooltips in neotree, they are useless, annoying and cause
  ;; rendering glitches
  (tooltip-mode 0)
  ;; helm-grep-in-dir neotree edition
  (define-key neotree-mode-map (kbd "f") 'neotree-helm-do-grep-ag))

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
;;; treesitter
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))

;; lang definitions picked up by 'M-x treesit-install-language-grammar'
;; see https://gist.github.com/slewsys/4ee95a67577e4df64d5f716c30420555
;; and https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
   '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
     (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
     (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
     (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
     (cmake      "https://github.com/uyha/tree-sitter-cmake")
     (css        "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "file:///opt/src/github/tree-sitter-dockerfile" "main" "src")
     (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
     (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
     (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
     (go         "https://github.com/tree-sitter/tree-sitter-go")
     (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
     (html       "https://github.com/tree-sitter/tree-sitter-html")
     (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json       "https://github.com/tree-sitter/tree-sitter-json")
     (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
     (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
     (make       "https://github.com/alemuller/tree-sitter-make")
     (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
     (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
     (perl       "file:///opt/src/github/tree-sitter-perl" "master" "src")
     (python     "https://github.com/tree-sitter/tree-sitter-python")
     (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
     (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
     (toml       "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettier
;; make json work in emacs own js-json-mode (which seems to be the default for json files)
;; without this, prettier will try to format the buffer as Javascript and fail
;; see https://github.com/jscheid/prettier.el/issues/115
(require 'prettier)
(add-to-list 'prettier-major-mode-parsers
             `(js-json-mode . ,(cdr (assoc 'json-mode prettier-major-mode-parsers))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tide (typescript ide) mode
(require 'tide)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save) ;; disabled for now

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (prettier-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; hook on treesitter typescript modes
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)

;; put tslint as the last one into the chain of fly-checkers, but only if
;; compilation did not produce any warnings
(flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint))
(flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sensible unique buffer names
(require 'uniquify)

;; use the full project path for project-files so they are readily find-able
;; via helm
;; credits to https://emacs.stackexchange.com/questions/38759/projectile-buffer-names-with-project-relative-filenames
(defun custom-projectile-relative-buffer-name ()
  (let ((root-dir (projectile-project-root)))
    (when root-dir
      (rename-buffer
       (file-relative-name buffer-file-name root-dir)))))

(add-hook 'find-file-hook #'custom-projectile-relative-buffer-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editable grep buffers
(require 'wgrep)
(require 'wgrep-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icon fonts
;;;
;;; (for the neotree icon theme)
;;; do not forget to run "M-x all-the-icons-install-fonts"
(require 'all-the-icons)

;; add missing typescript tsx icon
(add-to-list 'all-the-icons-icon-alist
             '("\\.tsx$"
               all-the-icons-alltheicon "react"
               :height 1.0
               :face all-the-icons-blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode for editing Dockerfiles
(require 'dockerfile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; better unicode font configuration (so i can see unicode symbols)
(require 'unicode-fonts)
(unicode-fonts-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline customs
(setq-default mode-line-format
      (list
       "%5l " ;; line numbers
       mode-line-buffer-identification ;; filename
       " "
       '(:eval (list (nyan-create))) ;; rainbow scrollbar
       " "
       vc-mode ;; git
       " "
       mode-line-modes
       mode-line-end-spaces
       mode-line-position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; claude-code.el
;; - using https://github.com/musistudio/claude-code-router as a proxy
;;   start via `ccr start`, configure via `ccr ui`
;; - ccr credentials come via ANTHROPIC_BASE_URL and ANTHROPIC_API_KEY which
;;   are defined in .bashrc so the `claude` command can pick them up
(use-package claude-code :ensure t
  ;; not available on melpa, only via direct git download
  ;; should use a depth of 1 as the repo is pretty large but I have no idea
  ;; how to tell this to `vc`
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  ;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;; (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  ;; :bind
  ;; (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  )

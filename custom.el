(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(c-offsets-alist (quote ((brace-list-intro . 0))))
 '(case-fold-search t)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(comint-history-isearch (quote dwim))
 '(comint-input-ring-file-name nil)
 '(comment-empty-lines t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(display-battery-mode t)
 '(espresso-expr-indent-offset 4)
 '(espresso-indent-level 4)
 '(ethan-wspace-errors (quote (many-nls-eof no-nl-eof eol)))
 '(fill-column 78)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flymake-jslint-args
   (quote
    ("--white" "--unparam" "--todo" "--nomen" "--regexp" "--plusplus" "--bitwise" "--browser" "--nomen" "--predef" "$" "--predef" "jQuery" "--maxerror" "16")))
 '(flymake-jslint-command "jslint")
 '(flymake-jslint-detect-trailing-comma t)
 '(global-flycheck-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-mode-hook
   (quote
    (capitalized-words-mode turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indent)))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ipython-command "ipython")
 '(js-auto-indent-flag nil)
 '(js-expr-indent-offset 4)
 '(js3-indent-level 4)
 '(list-matching-lines-default-context-lines 1)
 '(nrepl-popup-on-error nil)
 '(nrepl-popup-stacktraces nil)
 '(nrepl-popup-stacktraces-in-repl nil)
 '(nrepl-use-pretty-printing t)
 '(org-agenda-files (quote ("~/org/admin-journal.org" "~/org/notes.org")))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path t)
 '(projectile-enable-caching t)
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "main.js")))
 '(projectile-project-root-files-bottom-up
   (quote
    (".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs")))
 '(py-align-multiline-strings-p t)
 '(py-python-command "ipython")
 '(py-python-command-args
   (quote
    ("-i" "-colors" "LightBG" "-nobanner" "-noconfirm_exit")))
 '(py-shell-switch-buffers-on-execute nil)
 '(python-python-command "ipython")
 '(scroll-bar-mode (quote right))
 '(scss-compile-at-save nil)
 '(sgml-basic-offset 4)
 '(show-paren-delay 0)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(size-indication-mode t)
 '(smartparens-global-mode nil)
 '(sp-base-key-bindings (quote paredit))
 '(sql-pop-to-buffer-after-send-region nil)
 '(sql-product (quote mysql))
 '(swank-clojure-classpath
   (list "~/clj/clojure/clojure.jar" "~/clj/clojure-contrib/clojure-contrib.jar" "~/clj/swank-clojure/src/" "~/clj/neo4j-clojure/src" "~/src/neo4j-kernel-1.0-rc/*" "~/clj/clj-pivot/src" "~/clj/clj-pivot/classes" "~/clj/clj-pivot/examples/src" "~/clj/clj-pivot/examples/snippets" "~/clj/clj-pivot/icons" "~/src/apache-pivot-1.5/lib/*" "~/clj/tmp" "~/clj/spreadsheet-repl/src" "/usr/lib/jvm/java-6-sun/lib/tools.jar" "~/clj/clj-debug/src" "~/qm/src/" "~/clj/compojure/compojure.jar" "~/clj/compojure/deps/*" "~/clj/Clojure-yahoo-finance/lib/*" "~/clj/scriptjure/src" "~/clj/clojure-jna/src" "~/clj/cdc/src" "~/src/jnotify-src/jnotify/build/output/jnotify-0.93-PATCH.jar" "/usr/share/java/mysql.jar" "~/clj/metatron-experimental/src" "~/clj/metatron-experimental/build" "~/clj/criterium/src"))
 '(swank-clojure-extra-vm-args
   (list "-Xmx512M" "-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8888" "-Djava.ext.dirs=/usr/share/java"))
 '(table-cell-intersection-char 43)
 '(table-cell-vertical-char 124)
 '(tail-raise nil)
 '(tail-volatile nil)
 '(term-scroll-to-bottom-on-output t)
 '(tramp-auto-save-directory "~/.emacs.d/tramp_backups"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "RGB:FFFF/EAF6/CB8B" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono"))))
 '(bm-face ((t (:background "burlywood1" :box (:line-width 1 :color "dark orange") :slant normal))))
 '(diff-added ((t (:inherit diff-changed :foreground "darkgreen"))))
 '(diff-changed ((nil (:foreground "red"))))
 '(diff-indicator-added ((t (:inherit diff-added :foreground "darkgreen"))))
 '(diff-indicator-changed ((t (:inherit diff-changed :foreground "darkred"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "darkred"))))
 '(font-lock-comment-face ((t (:foreground "Red3"))))
 '(font-lock-constant-face ((t (:foreground "DarkOrange3"))))
 '(font-lock-function-name-face ((t (:foreground "MediumBlue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "CornflowerBlue" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "saddlebrown"))))
 '(fringe ((t (:background "RGB:EFFF/DAF6/AB8B"))))
 '(idle-highlight ((t (:inherit nil :background "PeachPuff2"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "grey90" :foreground "black" :box (:line-width -1 :color "grey75")))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) (:background "grey75" :foreground "grey20" :box (:line-width -1 :style released-button) :weight light))))
 '(outline-2 ((t (:foreground "#228d00"))))
 '(outline-3 ((t (:foreground "tomato3"))))
 '(outline-4 ((t (:foreground "gold3"))))
 '(outline-5 ((t (:foreground "dodgerblue"))))
 '(outline-6 ((t (:foreground "darkorchid3"))))
 '(region ((((class color) (min-colors 88) (background light)) (:background "grey75"))))
 '(table-cell ((t (:background "#f2d995" :foreground "black" :inverse-video nil))))
 '(tex-verbatim ((t (:foreground "deepskyblue3")))))

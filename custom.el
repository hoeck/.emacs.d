(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(bm-electric-show nil)
 '(c-default-style
   '((c-mode . "cc-mode") (c++-mode . "cc-mode") (java-mode . "java")
     (awk-mode . "awk") (other . "gnu")))
 '(c-offsets-alist
   '((brace-list-intro first c-lineup-2nd-brace-entry-in-arglist
                       c-lineup-class-decl-init-+ +)))
 '(case-fold-search t)
 '(claude-code-terminal-backend 'vterm)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(comint-history-isearch 'dwim)
 '(comint-input-ring-file-name nil)
 '(comment-empty-lines t)
 '(company-require-match nil)
 '(css-indent-offset 2)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(default-text-scale-amount 20)
 '(display-battery-mode t)
 '(electric-indent-mode t)
 '(espresso-expr-indent-offset 4)
 '(espresso-indent-level 4)
 '(ethan-wspace-errors '(many-nls-eof no-nl-eof eol))
 '(explicit-shell-file-name nil)
 '(fill-column 78)
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-checkers
   '(ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine
              chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd
              emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis
              fortran-gfortran go-gofmt go-golint go-vet go-build go-test
              go-errcheck groovy haml handlebars haskell-stack-ghc haskell-ghc
              haskell-hlint html-tidy jade javascript-jshint javascript-eslint
              javascript-gjslint javascript-jscs javascript-standard
              json-jsonlint json-python-json less luacheck lua perl
              perl-perlcritic php php-phpmd php-phpcs puppet-parser
              puppet-lint python-flake8 python-pylint python-pycompile r-lintr
              racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-rubylint
              ruby ruby-jruby rust sass scala scala-scalastyle scss-lint scss
              sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim
              sql-sqlint tex-chktex tex-lacheck texinfo verilog-verilator
              xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(flycheck-coffee-coffeelint-executable "/home/erik/heavygoods/node_modules/.bin/coffeelint")
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
 '(flycheck-eslintrc ".eslintrc")
 '(flycheck-jscs ".jscsrc")
 '(flycheck-temp-prefix ".flycheck")
 '(flymake-jslint-args
   '("--white" "--unparam" "--todo" "--nomen" "--regexp" "--plusplus" "--bitwise"
     "--browser" "--nomen" "--predef" "$" "--predef" "jQuery" "--maxerror"
     "16"))
 '(flymake-jslint-command "jslint")
 '(flymake-jslint-detect-trailing-comma t)
 '(global-flycheck-mode nil)
 '(global-font-lock-mode t nil (font-lock))
 '(global-prettier-mode t)
 '(haskell-mode-hook
   '(capitalized-words-mode turn-on-haskell-decl-scan turn-on-haskell-doc
                            turn-on-haskell-indent))
 '(helm-buffer-details-flag nil)
 '(helm-candidate-number-limit 500)
 '(helm-mm-matching-method 'multi2)
 '(helm-move-to-line-cycle-in-source t)
 '(highlight-symbol-idle-delay 0.3)
 '(ido-default-buffer-method 'selected-window)
 '(ido-everywhere nil)
 '(ido-mode 'both nil (ido))
 '(ipython-command "ipython")
 '(js-indent-level 2)
 '(list-matching-lines-default-context-lines 1)
 '(neo-confirm-change-root 'off-p)
 '(neo-confirm-create-directory 'off-p)
 '(neo-confirm-create-file 'off-p)
 '(neo-confirm-kill-buffers-for-files-in-directory 'off-p)
 '(neo-cwd-line-style 'button)
 '(neo-dont-be-alone t)
 '(neo-keymap-style 'concise)
 '(neo-show-hidden-files t)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(neo-window-width 35)
 '(nrepl-popup-on-error nil)
 '(nrepl-popup-stacktraces nil)
 '(nrepl-popup-stacktraces-in-repl nil)
 '(nrepl-use-pretty-printing t)
 '(nyan-mode t)
 '(org-agenda-files '("~/org/admin-journal.org" "~/org/notes.org"))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path t)
 '(package-selected-packages
   '(ac-cider ace-jump-mode all-the-icons auto-highlight-symbol autopair bm
              browse-kill-ring buffer-move claude-code coffee-mode color-theme
              company-racer csharp-mode default-text-scale dockerfile-mode egg
              elm-mode flycheck-rust flymake-python-pyflakes go-mode
              haskell-mode helm-dash helm-projectile helm-swoop
              highlight-symbol json-snatcher magit magit-popup markdown-mode+
              multiple-cursors neotree nyan-mode php-mode pkg-info prettier
              racer scss-mode shackle smartparens sql-indent sr-speedbar
              string-inflection tabbar tagedit tide unicode-fonts vimish-fold
              vterm vue-mode wgrep-helm yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(prettier-enabled-parsers
   '(angular babel babel-flow babel-ts css elm espree flow graphql java json
             json5 json-stringify less lua html markdown mdx meriyah php
             postgresql pug python ruby scss sh solidity svelte swift toml
             typescript vue xml yaml))
 '(prettier-infer-parser-flag t)
 '(prettier-js-command "prettier")
 '(prettier-mode-sync-config-flag nil)
 '(projectile-enable-caching t)
 '(projectile-project-root-files
   '("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt"
     "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini"
     "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json"
     "Cargo.toml" "mix.exs" "main.js"))
 '(projectile-project-root-files-bottom-up '(".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs"))
 '(projectile-use-git-grep t)
 '(py-align-multiline-strings-p t)
 '(py-python-command "ipython")
 '(py-python-command-args '("-i" "-colors" "LightBG" "-nobanner" "-noconfirm_exit"))
 '(py-shell-switch-buffers-on-execute nil)
 '(python-python-command "ipython")
 '(ruby-insert-encoding-magic-comment nil)
 '(ruby-use-encoding-map t)
 '(scroll-bar-mode 'right)
 '(scss-compile-at-save nil)
 '(sgml-basic-offset 4)
 '(show-paren-delay 0)
 '(show-paren-mode t nil (paren))
 '(show-paren-style 'expression)
 '(size-indication-mode nil)
 '(sp-base-key-bindings 'paredit)
 '(sql-pop-to-buffer-after-send-region nil)
 '(sql-product 'mysql)
 '(table-cell-intersection-char 43)
 '(table-cell-vertical-char 124)
 '(tail-raise nil)
 '(tail-volatile nil)
 '(term-scroll-to-bottom-on-output t)
 '(tide-completion-detailed t)
 '(tide-completion-enable-autoimport-suggestions nil)
 '(tide-disable-suggestions t)
 '(tide-native-json-parsing t)
 '(tide-server-max-response-length 524288)
 '(tide-tsserver-executable nil)
 '(tide-user-preferences
   '(:includeCompletionsForModuleExports nil :includeCompletionsWithInsertText t
                                         :allowTextChangesInNewFiles t))
 '(tramp-auto-save-directory "~/.emacs.d/tramp_backups")
 '(uniquify-after-kill-buffer-p nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-min-dir-content 1))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "RGB:FFFF/EAF6/CB8B" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 95 :width normal :family "Source Code Pro"))))
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
 '(outline-2 ((t (:foreground "#2b2bd5"))))
 '(outline-3 ((t (:foreground "#7777ea"))))
 '(outline-4 ((t (:foreground "#88a"))))
 '(outline-5 ((t (:foreground "dodgerblue"))))
 '(outline-6 ((t (:foreground "darkorchid3"))))
 '(region ((((class color) (min-colors 88) (background light)) (:background "grey75"))))
 '(table-cell ((t (:background "#f2d995" :foreground "black" :inverse-video nil))))
 '(tex-verbatim ((t (:foreground "deepskyblue3")))))

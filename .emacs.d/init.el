;; -*- lexical-binding: t -*-

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(package-install 'use-package)

(require 'vc-use-package)

(blink-cursor-mode 0)                           ; Disable the cursor blinking
(scroll-bar-mode 0)                             ; Disable the scroll bar
(tool-bar-mode 0)                               ; Disable the tool bar
(tooltip-mode 0) ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 bookmark-save-flag 1                             ; Always save bookmarks
 calc-kill-line-numbering nil                     ; Do not show line numbers in calc
 compilation-ask-about-save nil                   ; Don't save anything, don't ask
 compilation-save-buffers-predicate '(lambda () nil)
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 completion-cycle-threshold 5                     ; Tab-cycle completions if there are only 5 of them.
 completions-detailed t                           ; Add extra detail to completions
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 column-number-mode t                             ; Useful to look out for line length limits
 delete-by-moving-to-trash t                      ; Delete files to trash
 dired-vc-rename-file t                           ; Rename files in vc via dired
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 left-margin-width 1 right-margin-width 1         ; Add left and right margins
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 next-error-message-highlight t                   ; Highlight the current error in next-error buffer.
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 reb-re-syntax 'string                            ; No double blacklashes in re-builder
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 redisplay-dont-pause t                           ; As recommended by Mastering Emacs
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line.
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace t                       ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 switch-to-buffer-obey-display-actions t          ; Use display action rules for manual window switching
 tab-first-completion 'word                       ; Complete unless we're in the middle of the word.
 tab-always-indent 'complete                      ; If we're already indented, tab should complete
 tab-width 4                                      ; Set width for tabs
 trash-directory "~/.Trash"                       ; Set trash directory
 tooltip-use-echo-area t                          ; Good for non-mouse-users
 use-dialog-box nil                               ; Never use a UI dialog box, only minibuffer
 use-short-answers t                              ; Use y/n instead yes / no.
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(global-subword-mode 1)                           ; Iterate through CamelCase words
(global-so-long-mode 1)                           ; Better performance for files with long lines
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding

(if (member window-system '(ns mac))
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(global-set-key (kbd "s-8") 'toggle-frame-fullscreen)

(add-hook 'focus-out-hook #'garbage-collect)

(ffap-bindings)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(set-input-method "rfc1345")

(setq backup-directory-alist
      '(("." . "~/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      create-lockfiles nil)

(add-hook 'ielm-mode-hook 'eldoc-mode)
(defun g-ielm-init-history ()
  (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
    (make-directory (file-name-directory path) t)
    (setq-local comint-input-ring-file-name path))
  (setq-local comint-input-ring-size 10000)
  (setq-local comint-input-ignoredups t)
  (comint-read-input-ring))
(add-hook 'ielm-mode-hook 'g-ielm-init-history)

(defun g-ielm-write-history (&rest _args)
  (with-file-modes #o600
    (comint-write-input-ring)))

(advice-add 'ielm-send-input :after 'g-ielm-write-history)

(require 'dired)
(define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)

(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun ash/get-current-url ()
  (string-trim (do-applescript "tell application \"Arc\" to return URL of active tab of front window") (rx (1+ (or whitespace ?\"))) (rx (1+ (or whitespace ?\")))))
(defun ash/get-current-title ()
  (string-trim
   (do-applescript "tell application \"Arc\" to return title of front window")
   (rx (1+ (or whitespace ?\"))) (rx (1+ (or whitespace ?\")))))

(setq-default use-package-always-ensure t)
(require 'use-package)

(use-package general
  :config
  ;; Let's make the top-level key categories here
  (general-create-definer ash/key-def :prefix "C-c"))

(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  ;; TEMPORARY, seems like this isn't getting autoloaded correctly.
  (require 'org-duration)
  (require 'org-element)
  ;; A pomodoro group is for a day, so after 8 hours of no activity, that's a group.
  (setq org-pomodoro-expiry-time (* 60 8))
  :general
  ("C-c a" 'ash-goto-agenda)
  ("<f12>" 'org-capture)
  (:keymaps 'org-agenda-mode-map
            "P" 'org-pomodoro))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-resize t
        vertico-cycle t))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-auto-delay 0.8)         ;; Pause a bit before completion, else it's annoying.

  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  :init
  (global-corfu-mode))

;; More completions
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345))

;; From Vertico example installation instructions.
(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism orderless-prefixes))
  (orderless-component-separator " +\\|[-/]")
  :init
  ;; Completion category overrides for file helps with tramp, this is mentioned in the vertico docs.
  (setq completion-styles '(orderless)
        completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; We make the SPC key insert a literal space and the same for the
  ;; question mark.  Spaces are used to delimit orderless groups, while
  ;; the question mark is a valid regexp character.
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; In non-programming-buffers, we don't want `pcomplete-completions-at-point'
  ;; or 't' which seems to complete everything.
  (defun ash/fix-completion-for-nonprog-buffers ()
    (setq completion-at-point-functions
          (-remove-item t (append (-remove-item #'pcomplete-completions-at-point completion-at-point-functions)
                                  '(cape-file cape-abbrev cape-rfc1345)))))
  (add-hook 'org-mode-hook #'ash/fix-completion-for-nonprog-buffers)
  (add-hook 'notmuch-message-mode-hook #'ash/fix-completion-for-nonprog-buffers)

  (setq enable-recursive-minibuffers t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        resize-mini-windows t))

(use-package embark
  :ensure t
  :bind
  (("s-a" . embark-act)
   ("s-A" . embark-act-noexit))
  :general
  (:keymaps 'embark-symbol-map
            "h" 'describe-symbol
            "t" 'trace-function
            "T" 'untrace-function
            "x" 'xref-find-references)
  :config
  (add-to-list 'marginalia-prompt-categories '("tab by name" . tab))
  (defvar-keymap embark-tab-actions
    :doc "Keymap for actions for tab-bar tabs (when mentioned by name)."
    "s" #'tab-bar-select-tab-by-name
    "r" #'tab-bar-rename-tab-by-name
    "k" #'tab-bar-close-tab-by-name)
  (add-to-list 'embark-keymap-alist '(tab . embark-tab-actions))

  ;; By default, embark doesn't know how to handle org-links.  Let's provide a way.
  (defun ash/org-link ()
    "Get the link from an org-link."
    (require 's)
    (when (eq major-mode 'org-mode)
      (let ((context (org-element-context)))
        (cond ((and (eq (car context) 'link)
                    (equal (plist-get (cadr context) :type) "file"))
               (cons 'file (plist-get (cadr context) :path)))
              ((and (eq (car context) 'link)
                    (member (plist-get (cadr context) :type) '("http" "https")))
               (cons 'url (concat (plist-get (cadr context) :type) ":" (s-trim-right (plist-get (cadr context) :path)))))
              (t nil)))))
  (add-to-list 'embark-target-finders 'ash/org-link))

(use-package consult
  :config
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  :general
  ("C-x b" 'consult-buffer))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck))
  ;; If flycheck idle change delay is too short, then it overwrites the helpful
  ;; messages about how to call elisp functions, etc.
  :config (setq flycheck-idle-change-delay 15))

(use-package winum
  :config (winum-mode 1)
  :general
  ("M-1" 'winum-select-window-1)
  ("M-2" 'winum-select-window-2)
  ("M-3" 'winum-select-window-3)
  ("M-4" 'winum-select-window-4))

(defun ash/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
     (not (window-dedicated-p (selected-window)))))

(use-package avy
  :general ("s-j" 'avy-goto-char-timer)
  :init
  (require 'avy)
  (defun ash/avy-goto-url()
    "Use avy to go to an URL in the buffer."
    (interactive)
    ;; avy-action is a global that sometimes is stuck in a weird state, so we
    ;; have to specifically set it here via :action.
    (avy-jump "https?://" :action 'avy-action-goto)))

;; Before hydra because we use pretty-hydra-define in the hydra confg.
(use-package major-mode-hydra
  :bind
  ("M-o" . major-mode-hydra)
  :config
  ;; Mode maps
  (major-mode-hydra-define org-mode nil ("Movement"
                                         (("u" org-up-element "up" :exit nil)
                                          ("n" org-next-visible-heading "next visible heading" :exit nil)
                                          ("l" org-next-link "next link" :exit nil)
                                          ("L" org-previous-link "previous link" :exit nil)
                                          ("b" org-next-block "next block" :exit nil)
                                          ("B" org-prev-block "previous block" :exit nil)
                                          ("g" org-mark-ring-goto "pop mark" :exit nil))
                                         "Subtrees" (("k" org-cut-subtree "kill")
                                                     (">" org-demote-subtree "demote" :exit nil)
                                                     ("<" org-promote-subtree "promote" :exit nil)
                                                     ("N" org-narrow-to-subtree "narrow")
                                                     ("r" org-refile "refile")
                                                     ("." org-tree-to-indirect-buffer "indirect buffer")
                                                     ("'" org-id-get-create "create id"))
                                         "Inserting" (("c" citar-insert-citation "insert citation")
                                                      ("e" org-expiry-insert-expiry "insert expiry property")
                                                      ("i" org-insert-heading-respect-content "insert heading")
                                                      ("y" ash/org-paste-link "yank link" :exit t))
                                         "Opening" (("o" org-open-at-point "open at point"))
                                         "Clock" (("p" org-pomodoro "Start pomodoro")
                                                  ("P" ash/org-pomodoro-til-meeting "Start pomodoro til half hour"))
                                         "Roam" (("-" org-roam-buffer-toggle "Backlinks" :toggle t)
                                                 (";" org-roam-node-insert "add link")
                                                 (":" ash/org-roam-node-insert-immediate "add link immediately")
                                                 ("#" org-roam-tag-add "add tag")
                                                 ("a" org-roam-alias-add "add alias")
                                                 ("R" org-roam-ref-add "add ref"))))
  (major-mode-hydra-define emacs-lisp-mode nil
    ("Eval"
     (("b" eval-buffer "eval buffer")
      (";" eval-expression "eval expression")
      ("d" eval-defun "eval defun")
      ("D" edebug-defun "edebug defun")
      ("e" eval-last-sexp "eval last sexp")
      ("E" edebug-eval-last-sexp "edebug last sexp")
      ("l" ielm "ielm"))
     "Test"
     (("t" ert "prompt")
      ("T" (ert t) "all")
      ("F" (ert :failed) "failed"))
     "Doc"
     (("f" describe-function "function")
      ("v" describe-variable "variable")
      ("i" info-lookup-symbol "info lookup"))))
  (major-mode-hydra-define eshell-mode nil
    ("Movement"
     (("h" consult-history "history" :exit t)))))

(use-package hydra
  :config
  ;; define everything here
  (require 'pretty-hydra)
  (pretty-hydra-define hydra-jumps ()
    ("Jump visually"
     (("j" avy-goto-word-1 "to word" :exit t)
      ("l" avy-goto-line "to line" :exit t)
      ("c" avy-goto-char "to char" :exit t)
      ("r" avy-resume "resume" :exit t))
     "Jump via minibuffer"
     (("i" consult-imenu "imenu" :exit t)
      ("o" consult-outline "outline" :exit t))
     "Jump & go"
     (("u" ash/avy-goto-url "open url" :exit t))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  (pretty-hydra-define hydra-structural ()
    ("Change"
     (("]" puni-slurp-forward "slurp")
      ("." puni-splice "splice")
      ("/" puni-convolute "convolute"))
     "Movement"
     (("b" puni-beginning-of-sexp "beginning of sexp")
      ("e" puni-end-of-sexp "end of sexp")
      ("d" puni-syntactic-forward-punc "down sexp")
      ("e" puni-syntactic-backward-punc "up sexp"))
     "Formatting"
     (("u" puni-squeeze "unwrap"))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  (pretty-hydra-define hydra-multiple-cursors ()
    ("Mark via region"
     (("l" mc/edit-lines "edit lines" :exit t)
      ("s" mc/mark-all-in-region-regexp "mark all in region re" :exit t))
     "Mark"
     (("a" mc/mark-all-like-this "mark all" :exit t)
      ("d" mc/mark-all-dwim "mark dwim" :exit t))
     "Mark incrementally"
     (("n" mc/mark-next-like-this "mark next like this")
      ("N" mc/skip-to-next-like-this "skip to next like this")
      ("M-n" mc/unmark-next-like-this "unmark next like this")
      ("p" mc/mark-previous-like-this "mark previous like this")
      ("P" mc/skip-to-previous-like-this "skip to previous like this")
      ("M-p" mc/unmark-previous-like-this "unmark previous like this")
      ("L" mc/mark-next-lines "mark next lines"))
     "Insert"
     (("0" mc/insert-numbers "insert numbers" :exit t)
      ("A" mc/insert-letters "insert letters" :exit t))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  (pretty-hydra-define hydra-expand ()
    ("Expand/Contract"
     (("e" er/expand-region "expand")
      ("c" er/contract-region "contract"))
     "Expand to..."
     (("d" er/mark-defun "defun")
      ("\"" er/mark-inside-quotes "quotes")
      ("'" er/mark-inside-quotes "quotes")
      ("p" er/mark-inside-pairs "pairs")
      ("." er/mark-method-call "call"))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  (pretty-hydra-define hydra-ekg ()
    ("Navigation"
     (("t" ekg-show-notes-for-today "today" :exit t)
      ("g" ekg-show-notes-with-tag "tag" :exit t)
      ("r" ekg-show-notes-latest-captured "latest" :exit t)
      ("b" ekg-embedding-show-similar-to-current-buffer "similar to buffer" :exit t)
      ("s" ekg-embedding-search "search" :exit t))
     "Capture"
     (("c" ekg-capture)
      ("u" ash/capture-literature-note))))
  (pretty-hydra-define hydra-yas ()
    ("Snippets"
     (("n" yas-new-snippet "new" :exit t)
      ("r" yas-reload-all "reload" :exit t)
      ("v" yas-visit-snippet-file "visit" :exit t))
     "Movement"
     (("f" yas-next-field "forward field" :exit nil)
      ("b" yas-prev-field "previous field" :exit nil))))
  (pretty-hydra-define hydra-flycheck ()
    ("Movement"
     (("n" flymake-goto-next-error "next error")
      ("p" flymake-goto-prev-error "previous error")
      ("d" flymake-goto-diagnostic "diagnostic")
      ("<" flycheck-previous-error "previous flycheck error")
      (">" flycheck-next-error "next flycheck error")
      ("l" flycheck-list-errors "list")
      ("." consult-flymake))
     "Display"
     (("." flymake-show-diagnostic "show diagnostic")
      ("B" flymake-show-diagnostics-buffer "diagnostics buffers"))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  ;; notmuch is too specialized to be set up here, it varies from machine to
  ;; machine. At some point I should break it down into the general &
  ;; specialized parts.
  (defun ash/inbox ()
    (interactive)
    (notmuch-search "tag:inbox" t))
  (pretty-hydra-define hydra-mail ()
    ("Search"
     (("s" notmuch-search "search" :exit t)
      ("h" consult-notmuch "incremental search" :exit t))
     "Application"
     (("n" notmuch-hello "notmuch" :exit t)
      ("i" ash/inbox "inbox" :exit t)
      ("c" notmuch-mua-new-mail "compose" :exit t))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  (pretty-hydra-define hydra-org-main ()
    ("Misc"
     (("a" org-agenda "agenda" :exit t)
      ("c" org-capture "capture" :exit t))
     "Links"
     (("s" org-store-link "store" :exit t))))
  (pretty-hydra-define hydra-find ()
    ("In-Buffer"
     (("i" consult-imenu "imenu" :exit t)
      ("m" consult-mark "mark rings" :exit t)
      ("o" consult-multi-occur "occur" :exit t)
      ("e" consult-flycheck "errors" :exit t)
      ("l" consult-goto-line "line" :exit t))
     "Other"
     (("r" consult-ripgrep "grep" :exit t)
      ("b" consult-bookmark "bookmark" :exit t)
      ("R" consult-register "register" :exit t)
      ("C" consult-complex-command "complex command" :exit t))))
  (pretty-hydra-define hydra-all
    (:quit-key "q" :title "All")
    ("Applications"
     (("m" hydra-mail/body "mail" :exit t)
      ("o" hydra-org-main/body "org" :exit t)
      ("k" hydra-ekg/body "ekg" :exit t)
      ("g" magit-status "magit" :exit t)
      ("!" ash/el-secretario-daily-review "secretary" :exit t))
     "Editing"
     (("s" hydra-structural/body  "structural" :exit t)
      ("c" hydra-multiple-cursors/body "multiple cursors" :exit t)
      ("e" hydra-expand/body "expand region" :exit t)
      ("y" hydra-yas/body "snippets" :exit t))
     "Movement"
     (("j" hydra-jumps/body "jumps" :exit t)
      ("E" hydra-flycheck/body "errors" :exit t)
      ("G" deadgrep "grep" :exit t))
     "Misc"
     (("f" hydra-find/body "find" :exit t))))

  (global-set-key (kbd "M-[") 'hydra-all/body)
  (global-set-key (kbd "C-c c") 'hydra-all/body)
  (global-set-key (kbd "s-c") 'hydra-all/body))

(use-package casual
  :ensure t
  :bind (:map calc-mode-map ("C-o" . casual-main-menu)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq-default yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yas-reload-all)
  (yas-global-mode 1))

(use-package multiple-cursors
  :pin melpa
  :bind (("s-r" . mc/mark-all-like-this-dwim)))

(use-package phi-search
  :bind (("M-C-s" . phi-search)
         ("M-C-r" . phi-search-backward)))

(use-package expand-region)

(global-set-key (kbd "M-z") #'zap-up-to-char)

(use-package vundo)

(add-hook 'before-save-hook
  (lambda ()
    (when (derived-mode-p 'prog-mode)
      (delete-trailing-whitespace))))

(use-package magit
  :general ("C-x g" 'magit-status))

(use-package lsp-mode
  :disabled t
  :config
  (lsp-register-custom-settings
   '(("lsp-pylsp-plugins-pylint-enabled" t t)))
  (setq lsp-warn-no-matched-clients nil)
  :hook ((python-base-mode . lsp-mode)
         (csharp-mode . lsp-mode)))
(use-package lsp-ui)

(use-package eglot
  :hook ((csharp-mode . eglot))
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp .
                          (:configurationSources
                           ["flake8"]
                           :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :pyflakes (:enabled nil) :flake8 (:enabled t)))))))

(use-package flycheck-eglot)
(use-package consult-eglot)
(use-package consult-eglot-embark
  :config
  (consult-eglot-embark-mode))

(use-package lsp-bridge
  :disabled
  :vc (:fetcher github :repo "manateelazycat/lsp-bridge")
  :general
  ("<f2>" 'lsp-bridge-diagnostic-list)
  :bind (:map lsp-bridge-mode-map
              ("M-." . lsp-bridge-find-def)
              ("M-?" . lsp-bridge-find-references))
  :hook (python-base-mode . lsp-bridge-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package puni
  :defer t
  :diminish ""
  :init (puni-global-mode) (electric-pair-mode 1)
  (add-hook 'org-mode-hook #'puni-disable-puni-mode)
  (add-hook 'org-mode-hook (lambda () (electric-pair-local-mode -1))))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq-default flycheck-highlighting-mode 'lines
                ;; Wait before complaining so we don't step on useful help messages.
                flycheck-idle-change-delay 3)
  ;; Define fringe indicator / warning levels
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/Azganoth/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (r "https://github.com/r-lib/tree-sitter-r")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
(use-package tree-sitter-langs)

(use-package markdown-mode)
(use-package flymake-markdownlint)

(use-package yaml-mode)
(use-package flycheck-yamllint)

;; Assuming python-ts-mode is installed
;; Add a hook to automatically use python-ts-mode for Python files

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

(use-package lsp-pyright
  :ensure t)
(add-hook 'python-base-mode-hook (lambda ()
                                   (flycheck-select-checker 'python-pyright)
                                   (setq flycheck-disabled-checkers '(python-mypy))))

(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-eshell))

(use-package combobulate
  :disabled t
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "s-o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook
  ((python-ts-mode . combobulate-mode)
   (typst-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setq fill-column 88)
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "--profile=black" "--sl" file))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(isort black))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))
  (setf (alist-get 'bazel-mode apheleia-mode-alist)
        '(buildifier))

  (advice-add 'apheleia-format-buffer :around
              (lambda (orig-fun &rest args)
                (let ((default-directory (or (locate-dominating-file buffer-file-name ".git")
                                             default-directory)))
                  (apply orig-fun args)))))

(use-package flycheck-package)

(use-package copilot
  :vc (:fetcher github :repo "zerolfx/copilot.el")
  :hook (prog-mode . copilot-mode)
  :bind (("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-<return>" . 'copilot-accept-completion-by-line))
  :ensure t)

(use-package which-key
  :diminish
  :config (which-key-mode 1))

(use-package helpful :disabled
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h h" . helpful-at-point)
         ("C-h c" . helpful-command)))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-visible-fringes t
        modus-themes-mixed-fonts t
        modus-themes-intense-standard-completions t
        modus-themes-org-agenda '((header-block . (variable-pitch scale-title))
                                  (scheduled . uniform))
        modus-themes-variable-pitch-headings t
        modus-themes-variable-pitch-ui t
        modus-themes-rainbow-headings t
        modus-themes-section-headings t
        modus-themes-scale-headings t
        modus-themes-region '(bg-only no-extend)
        modus-themes-scale-1 1.05
        modus-themes-scale-2 1.1
        modus-themes-scale-3 1.15
        modus-themes-scale-4 1.2
        modus-themes-scale-5 1.3))

(use-package nano-theme
  :ensure t
  :config
  (nano-light))

(use-package org-bullets
  :init (add-hook 'org-mode-hook #'org-bullets-mode))

(setq-default org-startup-indented t
              org-bullets-bullet-list '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨")
              org-ellipsis " … " ;; folding symbol
              org-pretty-entities t
              org-hide-emphasis-markers t
              ;; show actually italicized text instead of /italicized text/
              org-agenda-block-separator ""
              org-fontify-whole-heading-line t
              org-fontify-done-headline t
              org-fontify-quote-and-verse-blocks t)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-buffer-encoding nil
                doom-modeline-minor-modes nil
                doom-modeline-icon nil))

(use-package all-the-icons)

(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'gnus-article-mode #'variable-pitch-mode)

(winner-mode 1)
(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)

(use-package darkroom
  :hook ((notmuch-message-mode notmuch-show org-capture-mode) . darkroom-mode))

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

(use-package eshell-git-prompt
  :after eshell
  :custom
  (eshell-git-prompt-use-theme 'multiline2)
  :custom-face
  (eshell-git-prompt-multiline2-dir-face ((t (:weight ultra-bold :foreground "grey")))))

(use-package eat
  :config
  (general-add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (when (eq system-type 'darwin)
    (define-key eat-semi-char-mode-map (kbd "C-h")  #'eat-self-input)
    (define-key eat-semi-char-mode-map (kbd "<backspace>") (kbd "C-h"))))

(setq tab-bar-select-tab-modifiers '(super))

(use-package notmuch
  :custom (notmuch-search-oldest-first nil)
  :config (require 'notmuch))

(use-package consult-notmuch)

(use-package ol-notmuch)

(use-package deadgrep)

(defun ash-goto-agenda (&optional _)
  (interactive)
  (let ((buf (get-buffer "*Org Agenda(l)*")))
    (tab-bar-switch-to-tab "org")
    (if buf
        (progn (switch-to-buffer buf)
               (delete-other-windows))
      (org-agenda))))

(require 'org-tempo)

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))
(setq org-clock-string-limit 80
      org-log-done t
      org-agenda-span 'day
      org-agenda-include-diary t
      org-agenda-start-with-clockreport-mode t
      org-agenda-start-with-archives-mode t
      org-deadline-warning-days 4
      org-capture-bookmark nil  ;; otherwise it sets the bookmark face.
      org-clock-idle-time 30
      org-confirm-babel-evaluate nil
      org-catch-invisible-edits 'error
      org-agenda-sticky t
      org-agenda-start-with-log-mode t
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s)"
                                    "WAITING(w@/!)" "DELEGATED(>@)" "|" "DONE(d)"
                                    "OBSOLETE(o)")
                          (type "PERMANENT")
                          (sequence "REVIEW(r)" "SEND(e)" "EXTREVIEW(g)" "RESPOND(p)" "SUBMIT(u)" "CLEANUP(c)"
                                    "|" "SUBMITTED(b)"))
      org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" tags-todo "+someday"
         ((org-show-hierarchy-above nil) (org-agenda-todo-ignore-with-date t)
          (org-agenda-tags-todo-honor-ignore-options t)))
        ("l" "Agenda and live tasks" ((agenda)
                                      (todo "PERMANENT")
                                      (todo "WAITING|EXTREVIEW|DELEGATED")
                                      (tags-todo "deepwork/!-WAITING-EXTREVIEW-DELEGATED")
                                      (tags-todo "-quick-collab-deepwork/!-WAITING-EXTREVIEW-DELEGATED"))))
      org-enforce-todo-dependencies t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-skip-deadline-if-done 't
      org-agenda-skip-scheduled-if-done 't
      org-src-window-setup 'other-window
      org-src-tab-acts-natively t
      org-fontify-done-headline t
      org-edit-src-content-indentation 0
      org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-use-sub-superscripts "{}"
      org-startup-with-inline-images t
      org-agenda-prefix-format '((agenda . " %i %-18:c%?-12t% s[%3e]")
                                 (timeline . "  % s")
                                 (todo . " %i %-18:c")
                                 (tags . " %i %-18:c[%3e]")
                                 (search . " %i %-18:c[%3e]"))
      org-modules '(org-bbdb org-docview org-info org-jsinfo org-wl org-habit org-gnus org-habit org-inlinetask)
      org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "NOTES")
      org-cycle-separator-lines 0
      org-blank-before-new-entry '((heading) (plain-list-item . auto))
      org-clock-into-drawer nil
      org-clock-report-include-clocking-task t
      org-clock-history-length 20
      org-extend-today-until 6
      org-read-date-prefer-future nil
      org-use-property-inheritance t
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-use-speed-commands t
      org-link-frame-setup '((gnus . gnus)
                             (file . find-file-other-window))
      org-speed-commands-user '(("w" . ash-org-start-work))
      org-completion-use-ido t
      org-use-fast-todo-selection t
      org-habit-show-habits t)

(require 'org-agenda)
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
                                                         (python . t)))

(use-package org-contrib
  :config
  (require 'org-checklist))

(use-package org-pomodoro
  :custom
  (org-pomodoro-manual-break t)
  (org-pomodoro-play-sounds nil)
  :config
  (defun ash/org-pomodoro-til-meeting ()
    "Run a pomodoro until the next 30 minute boundary."
    (interactive)
    (let ((org-pomodoro-length (mod (- 30 (cadr (decode-time (current-time)))) 30)))
      (org-pomodoro))))

(use-package ekg
  ;; Use variable pitch fonts for notes
  :hook (((ekg-notes-mode ekg-capture-mode ekg-edit-mode) . variable-pitch-mode)
         ((ekg-capture-mode ekg-edit-mode) . visual-line-mode))
  :general
  ("<f1>" 'ekg-capture)
  ("C-<f1>" 'ash/capture-literature-note)
  :config
  (require 'ekg-embedding)
  (ekg-embedding-generate-on-save)
  (defun ash/capture-literature-note ()
    (interactive)
    (let ((url (ash/get-current-url)))
      (ekg-capture-url url (ash/get-current-title))))

  (defun ash/log-to-ekg (text &optional org-mode)
    "Log TEXT as a note to EKG's date, appending if possible."
    (let ((notes (ekg-get-notes-with-tags (list (ekg-tag-for-date) "log"))))
      (if notes
          (progn
            (setf (ekg-note-text (car notes)) (concat (ekg-note-text (car notes)) "\n" text))
            (ekg-save-note (car notes)))
        (ekg-save-note (ekg-note-create :text text :mode (if org-mode 'org-mode 'text-mode)
                                        :tags `(,(ekg-tag-for-date) "log"))))))

  (dolist (h '(ekg-capture-mode-hook ekg-edit-mode-hook ekg-notes-mode-hook))
    (add-hook h (lambda ()
                  (when (boundp 'flycheck-mode) (flycheck-mode -1)))))
  (add-to-list 'display-buffer-alist '("*EKG Capture.*\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (slot . 0)
                                       (window-width . 80)
                                       (window-parameters (no-delete-other-windows . t)))))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config (setq org-appear-autolinks nil
                org-appear-autosubmarkers t))

(use-package ob-mermaid)

(add-to-list 'load-path "~/src/llm")
(require 'llm)

(defvar emacs-llm-default-provider nil "The default LLM provider to use in Emacs.")

(use-package ellama
  :init
  (setopt ellama-language "Portguese")
  (setopt ellama-provider emacs-llm-default-provider))

(defun ash/tangle-config ()
  "Tangle the config file to a standard config file."
  (interactive nil org-mode)
  (org-babel-tangle 0 "~/.emacs.d/init.el"))

(general-define-key :keymaps 'org-mode-map
                    :predicate '(s-contains? "emacs.org" (buffer-name))
            "C-c t" 'ash/tangle-config)

(defun ash/find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/emacs.org"))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(setq epa-pinentry-mode 'loopback)

(defun ash/strdec-to-hex (n)
  "Given a decimal as a string, convert to hex.
This has to be done as a string to handle 64-bit or larger ints."
  (concat "0x" (replace-regexp-in-string "16#" "" (calc-eval `(,n calc-number-radix 16)))))

(let ((per-machine-filename "~/.emacs.d/permachine.el"))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

(defalias 'ash/mirror-buffer
  (kmacro "C-x 1 C-x 3 C-x o"))
(general-define-key "s-b" 'ash/mirror-buffer)

(use-package meow
  :config
  ;; It seems really odd that meow doesn't just define this themselves.
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
          meow-use-clipboard t)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '("=" . meow-pop-to-mark)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '(">" . meow-open-below)
     '("<" . meow-open-above)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-pop-grab)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (require 'meow-cheatsheet-layout)
  (meow-setup)
  (meow-global-mode 1)
  (dolist (mode '(eshell-mode calc-mode help-mode info-mode eat-mode vterm-mode))
    (add-to-list 'meow-mode-state-list `(,mode . insert))))

(defconst meow-per-mode-state-list nil
  "Alist of major modes and their corresponding meow state.")

(defun meow-enter-mode-state-list ()
  (interactive)
  (when-let ((state (assoc-default major-mode meow-per-mode-state-list)))
    (funcall (intern (format "meow-%s-mode" state)))))

(meow-define-keys 'normal '("/" . meow-enter-mode-state-list))

(setq meow-org-motion-keymap (make-keymap))
(meow-define-state org-motion
  "Org-mode structural motion"
  :lighter "[/]"
  :keymap meow-org-motion-keymap)

(meow-define-keys 'org-motion
  '("<escape>" . meow-normal-mode)
  '("i" . meow-insert-mode)
  '("g" . meow-normal-mode)
  '("u" .  meow-undo)
  ;; Moving between headlines
  '("k" .  org-previous-visible-heading)
  '("j" .  org-next-visible-heading)
  ;; Moving between headings at the same level
  '("p" .  org-backward-heading-same-level)
  '("n" .  org-forward-heading-same-level)
  ;; Clock
  '("I" .  org-clock-in)
  '("O" .  org-clock-out)
  ;; Moving up and down in the outline
  '("," .  outline-up-heading)
  '("." .  org-down-element)
  ;; Subtree de/promotion, and reordering
  '("L" .  org-demote-subtree)
  '("H" .  org-promote-subtree)
  '("J" .  org-move-subtree-down)
  '("K" .  org-move-subtree-up)
  ;; Completion-style search of headings
  '("v" .  imenu)
  ;; Setting subtree metadata
  '("l" .  org-set-property)
  '("t" .  org-todo)
  '("d" .  org-deadline)
  '("s" .  org-schedule)
  '("e" .  org-set-effort)
  ;; Block navigation
  '("b" .  org-previous-block)
  '("f" .  org-next-block)
  ;; Narrowing/widening
  '("N" .  org-narrow-to-subtree)
  '("W" .  widen)
  ;; Editing
  '("a" . org-archive-subtree)
  '("T" .  org-insert-todo-heading-respect-content))

(add-to-list 'meow-per-mode-state-list '(org-mode . org-motion))

(use-package meow-tree-sitter
  :config
  (meow-tree-sitter-register-defaults))

(use-package tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

;; -*- lexical-binding: t -*-

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(setq use-package-always-ensure t)
(package-initialize)

(unless (fboundp 'set-local)
  (defun set-local (variable value)
    "Make VARIABLE buffer local and set it to VALUE."
    (set (make-local-variable variable) value)))

(when (not package-archive-contents)
  (package-refresh-contents))

(package-install 'use-package)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(blink-cursor-mode 0)                           ; Disable the cursor blinking
(scroll-bar-mode 0)                             ; Disable the scroll bar
(tool-bar-mode 0)                               ; Disable the tool bar
(tooltip-mode 0) ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 bidi-display-reordering 'left-to-right           ; If you don't use RTL languages, this is a speedup
 bidi-paragraph-direction 'left-to-right          ; Ditto
 bookmark-save-flag 1                             ; Always save bookmarks
 calc-kill-line-numbering nil                     ; Do not show line numbers in calc
 compilation-ask-about-save nil                   ; Don't save anything, don't ask
 compilation-save-buffers-predicate '(lambda () nil)
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 completion-cycle-threshold 5                     ; Tab-cycle completions if there are only 5 of them.
 completions-detailed t                           ; Add extra detail to completions
 completion-eager-display t                       ; Show completions immediately
 completion-eager-update t                        ; Keep completions updated as you type
 completion-in-region-function #'consult-completion-in-region ; Use consult for completion in region
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 column-number-mode t                             ; Useful to look out for line length limits
 delete-by-moving-to-trash t                      ; Delete files to trash
 dired-vc-rename-file t                           ; Rename files in vc via dired
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 eldoc-help-at-point t                            ; Show documentation in the echo area
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 highlight-nonselected-windows nil                ; No need to see highlights in other windows
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 kill-do-not-save-duplicates t                    ; Eliminate duplicates in the kill ring
 left-margin-width 1 right-margin-width 1         ; Add left and right margins
 minibuffer-visible-completion 'up-down           ; Navigate minibuffer with arrow keys
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 next-error-message-highlight t                   ; Highlight the current error in next-error buffer.
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 read-process-output-max (* 4 1024 1024)          ; Bigger read-process buffer
 reb-re-syntax 'string                            ; No double blacklashes in re-builder
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 redisplay-dont-pause t                           ; As recommended by Mastering Emacs
 redisplay-skip-fontification-on-input t          ; For speed
 save-interprogram-paste-before-kill t            ; Useful to keep most useful things in kill ring
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line.
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
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
;; Don't try to contact machines on ffap (for latency reasons)
(setq-default ffap-machine-p-known 'reject)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-command-modifier 'hyper)

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

(use-package vlf
  :config
  (require 'vlf-setup)
  :demand t
  :ensure t)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun ash/get-current-url ()
  (string-trim (do-applescript "tell application \"Arc\" to return URL of active tab of front window") (rx (1+ (or whitespace ?\"))) (rx (1+ (or whitespace ?\")))))
(defun ash/get-current-title ()
  (string-trim
   (do-applescript "tell application \"Arc\" to return title of front window")
   (rx (1+ (or whitespace ?\"))) (rx (1+ (or whitespace ?\")))))

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

(use-package completion-preview
  :ensure nil
  :if (fboundp 'global-completion-preview-mode)
  :config
  (global-completion-preview-mode 1)
  :bind
  (:map completion-preview-active-mode-map
        ("C-n" . completion-preview-next-candidate)
        ("C-p" . completion-preview-prev-candidate)))

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
  :custom (savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  :config
  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring)))))
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
      ("G" deadgrep "grep" :exit t))
     "Misc"
     (("f" hydra-find/body "find" :exit t))))

  (global-set-key (kbd "M-[") 'hydra-all/body)
  (global-set-key (kbd "C-c c") 'hydra-all/body)
  (global-set-key (kbd "s-c") 'hydra-all/body))

(use-package casual
  :ensure t
  :config
  (setq transient-align-variable-pitch t)
  :bind (("s-o" . casual-editkit-main-tmenu)
         :map dired-mode-map ("s-o" . casual-dired-tmenu)))

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

(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace t)))
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

(use-package magit
  :general ("C-x g" 'ash/magit-status)
  :config
  (defun ash/magit-status ()
    "Open Magit status for the current project."
    (interactive)
    (let ((project-root (or
                         (let ((tab-src (format "~/src/%s"
                                                (substring-no-properties
                                                 (tabspaces--current-tab-name)))))
                           (when (file-directory-p tab-src)
                             tab-src))
                         (project-root (or (project-current t)
                                           (error "No project found in the current directory")))
                         default-directory)))
      (magit-status project-root))))

(use-package forge
  :ensure t
  :after magit)

(use-package lsp-mode
  :config
  (setq lsp-warn-no-matched-clients nil)
  (defun ash/python-lsp-mode-maybe ()
    "Start Python LSP only when pyright is available in a project."
    (when (and (executable-find "pyright-langserver")
               (project-current nil))
      (lsp-deferred)))
  :hook ((python-base-mode . ash/python-lsp-mode-maybe)
         (go-mode . lsp-mode)
         (go-ts-mode . lsp-mode)))
(use-package lsp-ui)

(use-package eglot
  :hook ((csharp-mode . eglot))
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp .
                          (:configurationSources
                           ["flake8"]
                           :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :pyflakes (:enabled nil) :flake8 (:enabled t)))))))

(use-package consult-eglot)
(use-package consult-eglot-embark
  :config
  (consult-eglot-embark-mode))

(use-package lsp-bridge
  :disabled
  :vc (:fetcher github :repo "manateelazycat/lsp-bridge")
  :general
  ("<f10>" 'lsp-bridge-diagnostic-list)
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

(use-package tree-sitter
  :config
  (when (fboundp 'global-tree-sitter-mode)
    (global-tree-sitter-mode))
  (when (fboundp 'treesit-auto-install-grammar)
    (treesit-auto-install-grammar t))
  (when (fboundp 'treesit-enabled-modes)
    (treesit-enabled-modes t)))
(use-package tree-sitter-langs)

(use-package markdown-mode)

(use-package yaml-mode)

(defun ash/python-mode-for-file ()
  "Use `python-ts-mode' when the grammar is ready; otherwise use `python-mode'."
  (if (and (fboundp 'treesit-ready-p)
           (treesit-ready-p 'python t))
      (python-ts-mode)
    (python-mode)))

(add-to-list 'auto-mode-alist '("\\.py\\'" . ash/python-mode-for-file))

(use-package lsp-pyright
  :ensure t)

(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-eshell))

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setq fill-column 88)
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "--profile=black" "--sl" file))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (setf (alist-get 'bazel-mode apheleia-mode-alist)
        '(buildifier))

  (advice-add 'apheleia-format-buffer :around
              (lambda (orig-fun &rest args)
                (let ((default-directory (or (locate-dominating-file buffer-file-name ".git")
                                             default-directory)))
                  (apply orig-fun args)))))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :preface
  (defun ash/copilot-mode-maybe ()
    "Enable Copilot when its language server is available."
    (when (and (not noninteractive)
               (ignore-errors (copilot-server-executable)))
      (copilot-mode 1)))
  :hook (prog-mode . ash/copilot-mode-maybe)
  :bind (("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-<return>" . 'copilot-accept-completion-by-line)))

(use-package which-key
  :diminish
  :config (which-key-mode 1))

(use-package helpful :disabled
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h h" . helpful-at-point)
         ("C-h c" . helpful-command)))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  :config
  (defun ash/ef-themes-custom-faces ()
    "Customization on top of ef-themes."
    (ef-themes-with-colors
      (custom-set-faces
       `(hydra-face-blue ((,c :foreground ,accent-0))))))
  (add-hook 'ef-themes-post-load-hook
            #'ash/ef-themes-custom-faces))

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

(use-package telephone-line
  :ensure t
  :config
  ;; Gradient separators can throw redisplay errors after theme/frame changes
  ;; because they render PBM images from live face and frame metrics.
  (setq telephone-line-primary-left-separator 'telephone-line-abs-left
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-abs-right
        telephone-line-secondary-right-separator 'telephone-line-nil)

  (setq telephone-line-lhs
        '((evil . (telephone-line-meow-tag-segment))
          (accent . (telephone-line-vc-segment))
          (nil . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil . (telephone-line-position-segment
                  telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))))
  (telephone-line-mode 1))

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

(use-package olivetti :ensure t
  :hook ((text-mode prog-mode org-mode) . olivetti-mode)
  :config
  (defun ash/olivetti-target-columns ()
    "Return target column width for the current major mode, or nil."
    (cond ((derived-mode-p 'prog-mode) 120)
          ((derived-mode-p 'org-mode) 80)
          (t nil)))

  (defun ash/olivetti-adjust-scale ()
    "Scale text so the target number of columns fills the window width."
    (when-let* ((target (and olivetti-mode (ash/olivetti-target-columns))))
      (let* ((ratio (/ (float (window-pixel-width))
                       (* target (frame-char-width))))
             (level (/ (log ratio) (log text-scale-mode-step))))
        (text-scale-set (min (max 0 (floor level)) 2))
        (setq-local olivetti-body-width target))))

  (add-hook 'olivetti-mode-hook #'ash/olivetti-adjust-scale))

(use-package auto-dim-other-buffers
  :ensure t
  :config
  (auto-dim-other-buffers-mode 1))

(use-package vertico-posframe
  :ensure t
  :config
  (defun ash/vertico-posframe-mode-maybe ()
    "Enable `vertico-posframe-mode' only on graphical frames."
    (when (display-graphic-p (selected-frame))
      (vertico-posframe-mode 1)))

  (ash/vertico-posframe-mode-maybe)
  (add-hook 'server-after-make-frame-hook #'ash/vertico-posframe-mode-maybe))

(use-package transient-posframe
  :ensure t
  :config
  (defun ash/transient-posframe-mode-maybe ()
    "Enable `transient-posframe-mode' only on graphical frames."
    (when (display-graphic-p (selected-frame))
      (transient-posframe-mode 1)))

  (ash/transient-posframe-mode-maybe)
  (add-hook 'server-after-make-frame-hook #'ash/transient-posframe-mode-maybe))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'multiline2)
  :custom-face
  (eshell-git-prompt-multiline2-dir-face ((t (:weight ultra-bold :foreground "grey")))))

(use-package ghostel
  ;; Mostly borrowed from Ghostel README
  :bind (("s-g" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill)
         ;; ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers)))

(use-package ghostel-eshell
  :ensure nil
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package ghostel-comint
  :ensure nil
  :hook (after-init . ghostel-comint-global-mode))

(use-package ghostel-compile
  :ensure nil
  :hook (after-init . ghostel-compile-global-mode))

(general-define-key "s-p" 'project-find-file)

(setq tab-bar-select-tab-modifiers '(super))

(use-package notmuch
  :custom (notmuch-search-oldest-first nil)
  :config (require 'notmuch))

(use-package consult-notmuch)

(use-package ol-notmuch)

(use-package org-mime
  :config
  (setq org-mime-library 'mml)
  ;; Make the attribution line match Gmail's "On [date], [name] wrote:" format.
  ;; Gmail uses: "On Sun, Jun 21, 2026 at 11:46\u202fAM Name &lt;email&gt; wrote:"
  ;; We approximate this with a close format that still works for plaintext.
  (setq message-citation-line-format "On %a, %b %d %Y at %I:%M\u202f%p, %N wrote:\n")
  ;; Add gmail_attr class to the attribution line so Gmail recognizes it
  (defun ash/org-mime-html-gmail-style ()
    "Post-process the current org-mime HTML buffer to match Gmail's visual appearance.
Wraps the attribution line in a div with class gmail_attr, and
wraps the entire quoted section in a div with class gmail_quote_container."
    (save-excursion
      ;; Wrap the attribution line in a gmail_attr div.  Gnus uses "Name writes:"
      ;; while Gmail uses "On DATE Name wrote:".
      (goto-char (point-min))
      (while (re-search-forward
              "<p>\\(\\(?:On [^<]+ wrote:\\)\\|\\(?:[^<]+ &lt;[^>]+&gt;\\) writes:\\)</p>"
              nil t)
        (let ((attribution (match-string 1)))
          (replace-match
           (format "<div dir=\"ltr\" class=\"gmail_attr\">%s</div>"
                   (replace-regexp-in-string " writes:\\'" " wrote:" attribution t t))
           t t)))
      ;; Add gmail_quote_container class to the outer blockquote for proper Gmail collapsing
      (goto-char (point-min))
      (while (search-forward "<blockquote class=\"gmail_quote\"" nil t)
        (replace-match "<div class=\"gmail_quote gmail_quote_container\"><blockquote class=\"gmail_quote\""
                       t t))
      ;; Close the gmail_quote_container div before the end of the message
      (goto-char (point-max))
      (when (re-search-backward "</blockquote>\\s-*\\'" nil t)
        (replace-match "</blockquote></div>" t t))))
  (add-hook 'org-mime-html-hook 'ash/org-mime-html-gmail-style)
  :hook
  ((message-mode . (lambda ()
                     (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))
   (message-send-hook . org-mime-htmlize)))

(use-package deadgrep)

(require 'midnight)

(setq-default midnight-node t)
(push ".*Agent.*" clean-buffer-list-kill-never-regexps)
(push ".*eshell*" clean-buffer-list-kill-never-regexps)

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
      org-agenda-start-with-archives-mode nil
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
      org-modules nil
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
      org-use-fast-todo-selection t)

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

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config (setq org-appear-autolinks nil
                org-appear-autosubmarkers t))

(use-package ob-mermaid)

(let ((llm-directory (expand-file-name "~/src/llm")))
  (when (file-directory-p llm-directory)
    (add-to-list 'load-path llm-directory)
    (require 'llm nil t)))

;; This fix is needed to properly use auth-source-search.
(setq epa-pinentry-mode 'loopback)

(defvar ash/llm-claude nil)
(defvar ash/llm-openai nil)
(defvar ash/llm-openai-small nil)
(defvar ash/llm-gemini nil)
(defvar ash/llm-deepseek-reasoner nil)
(defvar emacs-llm-default-provider nil
  "The default LLM provider to use in Emacs.")

(defun ash/auth-secret (host)
  "Return the auth-source secret for HOST, or nil when unavailable."
  (when-let* ((entry (car (auth-source-search :host host :require '(:secret))))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(when (featurep 'llm)
  (require 'llm-openai nil t)
  (require 'llm-gemini nil t)
  (require 'llm-claude nil t)
  (require 'llm-deepseek nil t)
  (when-let* ((key (ash/auth-secret "llm.claude")))
    (setq ash/llm-claude (make-llm-claude :key key)))
  (when-let* ((key (ash/auth-secret "llm.openai")))
    (setq ash/llm-openai (make-llm-openai :chat-model "gpt-4o" :key key)
          ash/llm-openai-small (make-llm-openai :chat-model "gpt-4o-mini" :key key)))
  (when-let* ((key (ash/auth-secret "llm.gemini")))
    (setq ash/llm-gemini (make-llm-gemini :key key)))
  (when-let* ((key (ash/auth-secret "llm.deepseek")))
    (setq ash/llm-deepseek-reasoner (make-llm-deepseek :key key :chat-model "deepseek-reasoner")))
  (setq emacs-llm-default-provider ash/llm-claude))

(use-package magit-gptcommit
  :ensure t
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :config
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

(add-to-list 'load-path "~/src/ekg")

(use-package vecdb :ensure t)

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

  (require 'ekg-llm)
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

  (add-to-list 'display-buffer-alist '("*EKG Capture.*\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (slot . 0)
                                       (window-width . 80)
                                       (window-parameters (no-delete-other-windows . t)))))

(defvar ash/org-code-contexts (make-hash-table :test 'equal)
  "Map from ws-name to context plist for active agent sessions.")

(defvar ash/org-code-agent 'ghostel
  "Which agent frontend to use.  Either `ghostel' or `agent-shell'.")

(require 'org)
(require 'org-id)
(use-package tabspaces :ensure t)

(defun ash/org-code--gather-context ()
  "Gather all context from current buffer for `ash/org-code'.
Must be called while in the org/ekg buffer.  Returns a plist."
  (cond
   ((derived-mode-p 'org-mode)
    (let* ((org-id (org-id-get-create t))
           (heading (org-get-heading t t t t))
           (task-name (org-entry-get nil "TASK_NAME"))
           (tags (org-get-tags))
           (project-tag (car (seq-filter
                              (lambda (tag) (string-prefix-p "project/" tag))
                              tags)))
           (base-repo-name (cond
                            (project-tag
                             (substring project-tag (length "project/")))
                            ((org-entry-get nil "codebase" t))
                            (t nil)))
           (marker (point-marker)))
      (save-buffer)
      (list :mode 'org
            :marker marker
            :buffer (current-buffer)
            :id org-id
            :heading heading
            :task-name task-name
            :base-repo-name base-repo-name
            :agent-command (format "/do-org %s" org-id))))
   ((derived-mode-p 'ekg-org-view-mode)
    (require 'ekg-org)
    (let* ((note-id (ekg-org-view--note-at-point))
           (note (when note-id (ekg-get-note-with-id note-id))))
      (unless note (user-error "No task at point"))
      (let* ((title (or (ekg-org--note-title note) "Untitled"))
             (task-name (ekg-org-get-property note "TASK_NAME"))
             (tags (ekg-note-tags note))
             (project-tag (car (seq-filter
                                (lambda (tag) (string-prefix-p "project/" tag))
                                tags)))
             (base-repo-name (when project-tag
                               (substring project-tag (length "project/")))))
        (list :mode 'ekg
              :note-id note-id
              :heading title
              :task-name task-name
              :base-repo-name base-repo-name
              :agent-command (format "/do-ekg-org %s" note-id)))))
   (t (user-error "Not in an org-mode or ekg-org-view-mode buffer"))))

(defun ash/org-code--resolve-base-repo (context)
  "Resolve the base repo path from CONTEXT, prompting if needed."
  (let ((name (plist-get context :base-repo-name)))
    (unless name
      (let* ((src-dirs (seq-remove
                        (lambda (d)
                          (or (string-prefix-p "ws-" d)
                              (string-prefix-p "." d)))
                        (directory-files "~/src")))
             (choice (completing-read "Base repo (from ~/src/): " src-dirs nil t)))
        (setq name choice)))
    (expand-file-name (format "~/src/%s" name))))

(defun ash/org-code--find-main-branch (base-repo)
  "Find the main branch name in BASE-REPO."
  (let ((default-directory base-repo))
    (cond
     ((zerop (call-process "git" nil nil nil "rev-parse" "--verify" "develop"))
      "develop")
     ((zerop (call-process "git" nil nil nil "rev-parse" "--verify" "main"))
      "main")
     ((zerop (call-process "git" nil nil nil "rev-parse" "--verify" "master"))
      "master")
     (t (error "No main branch found in %s" base-repo)))))

(defun ash/org-code--set-task-name (context task-name)
  "Store TASK_NAME property in the org/ekg item described by CONTEXT."
  (pcase (plist-get context :mode)
    ('org
     (let ((buf (marker-buffer (plist-get context :marker))))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (save-excursion
             (goto-char (plist-get context :marker))
             (org-set-property "TASK_NAME" task-name)
             (save-buffer))))))
    ('ekg
     (let ((note (ekg-get-note-with-id (plist-get context :note-id))))
       (ekg-org-set-property note "TASK_NAME" task-name)
       (ekg-save-note note)))))

(defun ash/org-code--set-state (context new-state)
  "Set the org state to NEW-STATE using CONTEXT."
  (pcase (plist-get context :mode)
    ('org
     (let ((buf (marker-buffer (plist-get context :marker))))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (save-excursion
             (goto-char (plist-get context :marker))
             (org-todo new-state)
             (save-buffer))))))
    ('ekg
     (let* ((note (ekg-get-note-with-id (plist-get context :note-id)))
            (tags (ekg-note-tags note))
            (new-tags (cons (format "org/state/%s" (downcase new-state))
                            (seq-remove (lambda (tag)
                                          (string-prefix-p "org/state/" tag))
                                        tags))))
       (setf (ekg-note-tags note) new-tags)
       (ekg-save-note note)))))

(defun ash/org-code--get-state (context)
  "Get the current org state from CONTEXT.  Returns uppercase string."
  (pcase (plist-get context :mode)
    ('org
     (let ((buf (marker-buffer (plist-get context :marker))))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (save-excursion
             (goto-char (plist-get context :marker))
             (org-get-todo-state))))))
    ('ekg
     (let ((note (ekg-get-note-with-id (plist-get context :note-id))))
       (when note (ekg-org--state note))))))

(defun ash/org-code--on-name-generated (context callback response)
  "Handle LLM RESPONSE for name generation, updating CONTEXT and calling CALLBACK."
  (let* ((parsed (json-parse-string response :object-type 'plist))
         (name (plist-get parsed :name)))
    (plist-put context :task-name name)
    (funcall callback context)))

(defun ash/org-code--generate-name-async (context callback)
  "Generate a worktree name from CONTEXT heading asynchronously.
Calls CALLBACK with the updated context once the name is ready."
  (let ((heading (plist-get context :heading)))
    (llm-chat-async
     emacs-llm-default-provider
     (llm-make-chat-prompt
      (format "Create a short git branch name (2-4 words max, lowercase, hyphenated, no special characters) for this task: %s" heading)
      :response-format '(:type object
                               :properties (:name (:type "string"))
                               :required ["name"]))
     (apply-partially #'ash/org-code--on-name-generated context callback)
     (lambda (err)
       (message "Failed to generate task name: %s" err)))))

(defun ash/agent-buffers ()
  "Get the current agent shell buffers for the current tab."
  (let ((tab-name (tabspaces--current-tab-name)))
    (ash/agent-buffers-for-tab tab-name)))

(defun ash/agent-buffers-for-tab (tab-name)
  "Get agent buffers for TAB-NAME.
Matches agent-shell or ghostel buffers whose name contains the tab name."
  (seq-filter (lambda (buf)
                (and (string-match-p (regexp-quote tab-name) (buffer-name buf))
                     (or (string-match-p "Agent @" (buffer-name buf))
                         (string-match-p "ghostel" (buffer-name buf)))))
              (buffer-list)))

(defun ash/org-code--send-to-ghostel (buf text)
  "Send TEXT as keystrokes to ghostel buffer BUF."
  (let ((proc (get-buffer-process buf)))
    (when proc
      (process-send-string proc text))))

(defun ash/org-code--send-command-to-ghostel (task-name cmd)
  "Send CMD to the ghostel buffer for TASK-NAME."
  (let ((bufs (ash/agent-buffers-for-tab task-name)))
    (when-let* ((buf (car bufs)))
      (ash/org-code--send-to-ghostel buf (concat cmd "\n")))))

(defun ash/org-code--launch-ghostel (worktree-path context)
  "Start a ghostel terminal in WORKTREE-PATH, run claude, then send command."
  (let* ((default-directory worktree-path)
         (task-name (plist-get context :task-name))
         (cmd (plist-get context :agent-command)))
    (ghostel t)
    (rename-buffer (format "*ghostel: %s*" task-name) t)
    ;; Send "claude" to start the agent
    (ash/org-code--send-to-ghostel (current-buffer) "claude --dangerously-skip-permissions\n")
    ;; Copy command to kill ring as fallback
    (kill-new cmd)
    (message "Copied '%s' to kill ring — will also auto-send in 5s." cmd)
    ;; After 5 seconds, send the /do-* command
    (run-at-time 5 nil #'ash/org-code--send-command-to-ghostel task-name cmd)))

(defun ash/org-code--launch-agent-shell (worktree-path)
  "Start a new agent-shell in WORKTREE-PATH without any prompts."
  (let ((default-directory worktree-path)
        (saved-strategy agent-shell-session-strategy))
    (unwind-protect
        (progn
          (setq agent-shell-session-strategy 'new)
          (agent-shell-anthropic-start-claude-code))
      (setq agent-shell-session-strategy saved-strategy))))

(defun ash/org-code--launch-and-command (context)
  "Launch the agent and send the command, using the configured backend."
  (let ((worktree-path (plist-get context :worktree-path)))
    (pcase ash/org-code-agent
      ('ghostel
       (ash/org-code--launch-ghostel worktree-path context))
      ('agent-shell
       (ash/org-code--launch-agent-shell worktree-path)
       (ash/org-code--prepare-command context)))))

(defun ash/org-code--prepare-command (context)
  "Copy the agent command from CONTEXT to the kill ring."
  (let ((cmd (plist-get context :agent-command)))
    (kill-new cmd)
    (message "Copied '%s' to kill ring — yank into the agent shell to start." cmd)))

(defun ash/org-code--start-agent (context)
  "Create tab and start the agent for CONTEXT.
Assumes worktree already exists at the path in CONTEXT."
  (let ((task-name (plist-get context :task-name)))
    (tab-bar-new-tab)
    (tab-bar-rename-tab task-name)
    (ash/org-code--launch-and-command context)))

(defun ash/org-code--activate (context)
  "Activate or switch to the agent for CONTEXT."
  (let* ((task-name (plist-get context :task-name))
         (worktree-dir (format "ws-%s" task-name))
         (base-repo (ash/org-code--resolve-base-repo context))
         (worktree-path (expand-file-name (format "~/src/%s" worktree-dir)))
         (existing-tab (seq-find (lambda (tab)
                                   (string= (alist-get 'name tab) task-name))
                                 (funcall tab-bar-tabs-function)))
         (worktree-exists (file-directory-p worktree-path)))
    ;; Store computed values in context
    (plist-put context :base-repo base-repo)
    (plist-put context :worktree-path worktree-path)
    ;; Save context for later use by finish
    (puthash task-name context ash/org-code-contexts)
    ;; Store TASK_NAME property
    (ash/org-code--set-task-name context task-name)
    ;; Set state to STARTED
    (ash/org-code--set-state context "STARTED")
    (cond
     ;; Tab exists -> switch to it, start agent if needed
     (existing-tab
      (tab-bar-select-tab-by-name task-name)
      (unless (ash/agent-buffers-for-tab task-name)
        (ash/org-code--launch-and-command context)))
     ;; Worktree exists but no tab -> create tab, resume session
     (worktree-exists
      (ash/org-code--start-agent context))
     ;; Neither -> pull upstream, create worktree, start agent
     (t
      (let* ((default-directory base-repo)
             (main-branch (ash/org-code--find-main-branch base-repo)))
        ;; Pull upstream before creating worktree
        (message "Pulling %s from upstream in %s..." main-branch base-repo)
        (call-process "git" nil nil nil "pull" "--ff-only" "origin" main-branch)
        ;; Create worktree
        (unless (zerop (call-process "git" nil nil nil
                                     "worktree" "add" "-b" task-name
                                     worktree-path main-branch))
          (user-error "Failed to create worktree at %s" worktree-path)))
      (ash/org-code--start-agent context)))))

(defun ash/org-code ()
  "Start agentic coding for the current org item.

Works in both `org-mode' and `ekg-org-view-mode' buffers.
Creates a git worktree at ~/src/ws-<TASK_NAME>, opens a new tab,
starts a pi-coding-agent session, and sends the /do-org or
/do-ekg-org command.  If a tab already exists, switches to it.
Sets the task state to STARTED."
  (interactive)
  (let ((context (ash/org-code--gather-context)))
    (if (plist-get context :task-name)
        (ash/org-code--activate context)
      (ash/org-code--generate-name-async context #'ash/org-code--activate))))

(defun ash/ekg-org-copy-do-command ()
  "Copy the /do-ekg-org command for the task at point to the kill ring."
  (interactive)
  (unless (derived-mode-p 'ekg-org-view-mode)
    (user-error "Not in an ekg-org-view-mode buffer"))
  (require 'ekg-org)
  (let ((note-id (ekg-org-view--note-at-point)))
    (unless note-id (user-error "No task at point"))
    (let ((cmd (format "/do-ekg-org %s" note-id)))
      (kill-new cmd)
      (message "Copied '%s'" cmd))))

(defun ash/agent-shell-finish-org ()
  "Clean up agent, worktree, and tab for current session.
Sets state to DONE only if currently STARTED."
  (interactive)
  (let* ((tab-name (tabspaces--current-tab-name))
         (context (gethash tab-name ash/org-code-contexts))
         (task-name (or (plist-get context :task-name) tab-name))
         (worktree-path (or (plist-get context :worktree-path)
                            (expand-file-name (format "~/src/ws-%s" task-name))))
         (agent-buffers (ash/agent-buffers)))
    (unless (and task-name (file-directory-p worktree-path))
      (user-error "No worktree found at %s" worktree-path))
    ;; Set state to DONE only if currently STARTED
    (when context
      (let ((current-state (ash/org-code--get-state context)))
        (when (equal current-state "STARTED")
          (ash/org-code--set-state context "DONE"))))
    ;; Kill agent buffers
    (mapc #'kill-buffer agent-buffers)
    ;; Find base repo and remove worktree + branch
    (let* ((default-directory worktree-path)
           (base-repo (or (plist-get context :base-repo)
                          (string-trim
                           (shell-command-to-string
                            "git worktree list --porcelain | head -1 | sed 's/^worktree //'"))))
           )
      (when (and base-repo (not (string-empty-p base-repo)))
        (let ((default-directory base-repo))
          (call-process "git" nil nil nil
                        "worktree" "remove" "--force" worktree-path)
          (call-process "git" nil nil nil
                        "branch" "-D" task-name))))
    ;; Clean up context
    (remhash tab-name ash/org-code-contexts)
    ;; Close tab
    (tab-bar-close-tab-by-name tab-name)))

(use-package pi-coding-agent
  :ensure t
  :init (defalias 'pi 'pi-coding-agent)
  :config
  (defun ash/pi-coding-agent-two-windows ()
    "Display pi-coding-agent input and chat buffers in two windows.
Input buffer on top, chat buffer on bottom."
    (interactive)
    (let ((agent-buffers (ash/agent-buffers)))
      (cl-flet ((process-buffer (buf)
                  (switch-to-buffer buf)
                  (goto-char (point-max))
                  (when (string-match-p "input" (buffer-name))
                    (set-window-text-height nil 4))))
        (delete-other-windows)
        (process-buffer (car agent-buffers))
        (dolist (buf (cdr agent-buffers))
          (split-window-below)
          (process-buffer buf)))))
  :bind ("s-i" . ash/pi-coding-agent-two-windows))

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

(defun ash/strdec-to-hex (n)
  "Given a decimal as a string, convert to hex.
This has to be done as a string to handle 64-bit or larger ints."
  (concat "0x" (replace-regexp-in-string "16#" "" (calc-eval `(,n calc-number-radix 16)))))

(let ((per-machine-filename "~/.emacs.d/permachine.el"))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

(defalias 'ash/mirror-buffer
  (kmacro "C-x 1 C-x 3 C-x o"))
(general-define-key "s-B" 'ash/mirror-buffer)

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
     '("'" . repeat-fu-execute)
     '("<escape>" . ignore)))
  (require 'meow-cheatsheet-layout)
  (meow-setup)
  (meow-global-mode 1)
  (dolist (mode '(eshell-mode calc-mode help-mode info-mode ghostel-mode))
    (add-to-list 'meow-mode-state-list `(,mode . insert))))

(use-package repeat-fu
  :commands (repeat-fu-mode repeat-fu-execute)

  :config
  (setq repeat-fu-preset 'meow)

  :hook
  ((meow-mode)
   .
   (lambda ()
     (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
       (repeat-fu-mode)
       (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
       (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute)))))

(defconst meow-per-mode-state-list nil
  "Alist of major modes and their corresponding meow state.")

(defun meow-enter-mode-state-list ()
  (interactive)
  (when-let* ((state (assoc-default major-mode meow-per-mode-state-list)))
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
  '("Z" .  org-add-note)
  ;; Block navigation
  '("b" .  org-previous-block)
  '("f" .  org-next-block)
  ;; Narrowing/widening
  '("N" .  org-narrow-to-subtree)
  '("W" .  widen)
  ;; Editing
  '("a" . org-archive-subtree)
  '("C-k" . org-cut-subtree)
  '("T" .  org-insert-todo-heading-respect-content))

(add-to-list 'meow-per-mode-state-list '(org-mode . org-motion))

(use-package meow-tree-sitter
  :config
  (meow-tree-sitter-register-defaults))

(use-package tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :general
  ("s-b" 'project-switch-to-buffer)
  ("s-t" 'tab-bar-select-tab-by-name)
  ("s-T" 'tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tab-bar-show 0))

(require 'server)
(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
(make-directory server-socket-dir t)
(unless (server-running-p server-name)
  (server-start))

(setenv "EDITOR" "~/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
(setenv "EMACS_SOCKET_NAME"
        (or (getenv "EMACS_SOCKET_NAME")
            (expand-file-name server-name server-socket-dir)))

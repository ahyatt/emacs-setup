(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(defvar bootstrap-version)
;; This doesn't work for me even on the latest native comp branch.
(setq straight-disable-native-compilation t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(blink-cursor-mode 0)                           ; Disable the cursor blinking
(scroll-bar-mode 0)                             ; Disable the scroll bar
(tool-bar-mode 0)                               ; Disable the tool bar
(tooltip-mode 0) ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 compilation-ask-about-save nil                   ; Don't save anything, don't ask
 compilation-save-buffers-predicate '(lambda () nil)
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 column-number-mode t                             ; Useful to look out for line length limits
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 left-margin-width 1 right-margin-width 1         ; Add left and right margins
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 reb-re-syntax 'string                            ; No double blacklashes in re-builder
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 redisplay-dont-pause t                           ; As recommended by Mastering Emacs
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line.
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 tooltip-use-echo-area t                          ; Good for non-mouse-users
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding

(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(add-hook 'focus-out-hook #'garbage-collect)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(set-input-method "TeX")

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq-default use-package-always-ensure t)
(require 'use-package)

(use-package general
  :config
  ;; Let's make the top-level key categories here
  (general-create-definer ash/key-def :prefix "C-c"))

(use-package org-plus-contrib
  :straight (org-plus-contrib
             :repo "https://code.orgmode.org/bzg/org-mode.git"
             :local-repo "org"
             :includes (org))
  :load-path "straight/repos/org/contrib/lisp"
  :config
  (require 'org-checklist)
  :hook (org-mode . visual-line-mode)
  :general
  ("C-c a" 'ash-goto-agenda)
  (:keymaps 'org-agenda-mode-map
            "P" 'org-pomodoro))

(use-package selectrum
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package embark
  :ensure t
  :bind
  (("s-a" . embark-act)
   ("s-A" . embark-act-noexit))
  :general
  (:keymaps 'embark-symbol-map
            "h" 'helpful-command
            "t" 'trace-function
            "T" 'untrace-function
            "x" 'xref-find-references)
  :config
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))

  (add-hook 'embark-candidate-collectors #'current-candidates+category)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

  (setq embark-action-indicator
      (lambda (map)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

  (add-to-list 'marginalia-prompt-categories '("tab by name" . tab))
  (embark-define-keymap embark-tab-actions
    "Keymap for actions for tab-bar tabs (when mentioned by name)."
    ("s" tab-bar-select-tab-by-name)
    ("r" tab-bar-rename-tab-by-name)
    ("k" tab-bar-close-tab-by-name))
  (add-to-list 'embark-keymap-alist '(tab . embark-tab-actions)))

(use-package consult)

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
              ("!" . consult-flycheck)))

(use-package winum
  :config (winum-mode 1)
  :general
  ("M-1" 'winum-select-window-1)
  ("M-2" 'winum-select-window-2)
  ("M-3" 'winum-select-window-3)
  ("M-4" 'winum-select-window-4))

(use-package avy
  :config
  (advice-add 'spacemacs/avy-goto-url :after (lambda () (browse-url-at-point)))
  (defun ash/avy-goto-url()
    "Use avy to go to an URL in the buffer."
    (interactive)
    ;; avy-action is a global that sometimes is stuck in a weird state, so we
    ;; have to specifically set it here via :action.
    (avy-jump "https?://" :action 'avy-action-goto))
  (defun ash/avy-open-url ()
    "Use avy to select an URL in the buffer and open it."
    (interactive)
    (save-excursion
      (ash/avy-goto-url)
      (browse-url-at-point))))

(use-package multiple-cursors
  :pin melpa
  :general)

(use-package phi-search
  :bind (("M-C-s" . phi-search)
         ("M-C-r" . phi-search-backward)))

(use-package expand-region)

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
                                          ("B" org-prev-block "previous block" :exit nil))
                                         "Subtrees" (("k" org-cut-subtree "kill")
                                                     (">" org-demote-subtree "demote" :exit nil)
                                                     ("<" org-promote-subtree "promote" :exit nil)
                                                     ("N" org-narrow-to-subtree "narrow")
                                                     ("r" org-refile "refile")
                                                     ("." org-tree-to-indirect-buffer "indirect buffer"))
                                         "Opening" (("o" org-open-at-point "open at point"))
                                         "Clock" (("p" org-pomodoro "Start pomodoro")
                                                  ("P" ash/org-pomodoro-til-meeting "Start pomodoro til half hour"))
                                         "Headings" (("i" org-insert-heading-respect-content "insert heading"))))
  (major-mode-hydra-define emacs-lisp-mode nil
    ("Eval"
     (("b" eval-buffer "eval buffer")
      (";" eval-expression "eval expression")
      ("d" eval-defun "eval defun")
      ("D" edebug-defun "edebug defun")
      ("e" eval-last-sexp "eval last sexp")
      ("E" edebug-eval-last-sexp "edebug last sexp")
      ("i" ielm "ielm"))
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
  (pretty-hydra-define hydra-jumps ()
    ("Jump visually"
     (("j" avy-goto-word-1 "to word")
      ("l" avy-goto-line "to line")
      ("c" avy-goto-char "to char")
      ("r" avy-resume "resume"))
     "Jump via minibuffer"
     (("i" consult-imenu "imenu")
      ("o" consult-outline "outline"))
     "Jump & go"
     (("u" ash/avy-open-url "open url"))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  (pretty-hydra-define hydra-structural ()
    ("Change"
     (("i" sp-change-inner "change inner")
      ("k" sp-kill-sexp "kill sexp")
      ("]" sp-slurp-hybrid-sexp "slurp")
      ("/" sp-swap-enclusing-sexp "swap enclusing"))
     "Movement"
     (("b" sp-beginning-of-sexp "beginning of sexp")
      ("e" sp-end-of-sexp "end of sexp")
      ("d" sp-down-sexp "down sexp")
      ("e" sp-up-sexp "up sexp"))
     "Formatting"
     (("r" sp-rewrap-sexp "rewrap"))
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
      ("n" mc/mark-next-lines "mark next lines"))
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
  (pretty-hydra-define hydra-roam ()
    ("Navigation"
     (("o" org-roam-find-file "open" :exit t)
      ("O" org-roam-open-at-point "open at point" :exit t)
      ("b" org-roam-switch-to-buffer "switch buffer" :exit t)
      ("s" deft "search" :exit t)
      ("t" org-roam-dailies-today "today" :exit t)
      ("y" org-roam-dailies-yesterday "yesterday" :exit t)
      ("d" org-roam-dailies-date "date" :exit t))
    "Sidebar"
    (("r" org-roam "toggle"))
    "Content"
    (("i" org-roam-insert "insert" :exit t))))
  (pretty-hydra-define hydra-straight ()
    ("Package specific"
     (("c" straight-check-package "check" :exit t)
      ("n" straight-normalize-package "normalize" :exit t)
      ("r" straight-rebuild-package "rebuild" :exit t)
      ("p" straight-pull-package "pull" :exit t))
     "All packages"
     (("C" straight-check-all "check" :exit t)
      ("N" straight-normalize-all "normalize" :exit t)
      ("R" straight-rebuild-all "rebuild" :exit t)
      ("P" straight-pull-all "pull" :exit t))
     "State"
     (("v" straight-freeze-versions "freeze" :exit t)
      ("t" straight-thaw-versions "thaw" :exit t)
      ("d" straight-prune-build "prune" :exit t))))
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
      ("h" ash/consult-notmuch "incremental search" :exit t))
     "Application"
     (("n" notmuch-hello "notmuch" :exit t)
      ("i" ash/inbox "inbox" :exit t)
      ("c" notmuch-mua-new-mail "compose" :exit t))
     "Misc"
     (("=" hydra-all/body "back" :exit t))))
  (pretty-hydra-define hydra-org-main ()
    ("Misc"
     (("a" org-agenda "agenda")
      ("c" org-capture "capture"))
     "Links"
     (("s" org-store-link "store")
      ("p" ash/org-paste-link "paste"))))
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
      ("r" hydra-roam/body "roam" :exit t)
      ("S" hydra-straight/body "straight" :exit t)
      ("g" magit-status "magit" :exit t))
     "Editing"
     (("s" hydra-structural/body  "structural" :exit t)
      ("c" hydra-multiple-cursors/body "multiple cursors" :exit t)
      ("e" hydra-expand/body "expand region" :exit t)
      ("y" hydra-yas/body "snippets" :exit t))
     "Movement"
     (("j" hydra-jumps/body "jumps" :exit t)
      ("E" hydra-flycheck/body "errors" :exit t))
     "Misc"
     (("f" hydra-find/body "find" :exit t))))

  (global-set-key (kbd "M-[") 'hydra-all/body)
  (global-set-key (kbd "C-c c") 'hydra-all/body)
  (global-set-key (kbd "s-c") 'hydra-all/body))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq-default yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yas-reload-all)
  (yas-global-mode 1))

(use-package magit
  :general ("C-x g" 'magit-status))

;; Needed by magit-gh-pulls
(use-package magit-popup)

(use-package magit-gh-pulls
  :hook (magit-mode . turn-on-magit-gh-pulls))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package smartparens
  :diminish ""
  :init (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  :hook (org-mode . smartparens-mode)
  :config (require 'smartparens-config))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package flycheck
  :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (setq-default flycheck-highlighting-mode 'lines)
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

(use-package company
  :general ("C-c ." 'company-complete)
  :config
  (setq company-global-modes '(emacs-lisp-mode c-mode c++-mode go-mode java-mode org-mode))
  (setq company-backends (seq-remove (lambda (b) (eq b 'company-dabbrev)) company-backends))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 0))

(use-package company-posframe
  :config (company-posframe-mode 1))

(use-package which-key
  :diminish
  :config (which-key-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h h" . helpful-at-point)
         ("C-h c" . helpful-command)))

(dolist (hook '(text-mode-hook org-mode-hook message-mode-hook notmuch-show-mode-hook))
  (when (boundp hook)
    (add-hook hook (lambda () (variable-pitch-mode 1)))))
;; (use-package poet-theme)
(use-package solarized-theme)

(straight-use-package
 `(ayu-themes :host github :repo "vutran1710/Ayu-Theme-Emacs" :type git))
(add-to-list 'custom-theme-load-path "~/.emacs.d/straight/repos/Ayu-Theme-Emacs/")

(use-package modus-operandi-theme
  :ensure t
  :init
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-visible-fringes t
        modus-operandi-theme-3d-modeline t
        modus-operandi-theme-subtle-diffs t
        modus-operandi-theme-intense-standard-completions t
        modus-operandi-theme-org-blocks 'greyscale
        modus-operandi-theme-proportional-fonts t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-section-headings t
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2
        modus-operandi-theme-scale-5 1.3)
  :config
  (load-theme 'modus-operandi t))

(use-package org-bullets
  :init (add-hook 'org-mode-hook #'org-bullets-mode))

(setq-default org-startup-indented t
              org-bullets-bullet-list '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨") 
              org-ellipsis "  " ;; folding symbol
              org-pretty-entities t
              org-hide-emphasis-markers t
              ;; show actually italicized text instead of /italicized text/
              org-agenda-block-separator ""
              org-fontify-whole-heading-line t
              org-fontify-done-headline t
              org-fontify-quote-and-verse-blocks t)

(use-package messages-are-flowing
  :config
  (add-hook 'message-mode-hook 'messages-are-flowing-use-and-mark-hard-newlines)
  (add-hook 'message-mode-hook 'visual-line-mode))

(with-eval-after-load 'message
  (setq message-cite-style message-cite-style-gmail)
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "On %a, %b %e, %Y at %I:%M %p %f wrote:\n"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-responsive 'top
        highlight-indent-guides-method 'character))

(winner-mode 1)
(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)

(use-package olivetti
  :ensure
  :defer
  :diminish
  :config
  (setq olivetti-body-width 0.65)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :bind ("C-c o" . olivetti-mode))

(defun ash-goto-agenda (&optional _)
  (interactive)
  (let ((buf (get-buffer "*Org Agenda(l)*")))
    (if buf
        (progn (switch-to-buffer buf)
               (delete-other-windows))
      (org-agenda))))

(require 'org-tempo)
(require 'org-checklist)

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))
(add-hook 'org-mode-hook
      (lambda ()
        (auto-fill-mode)
        (variable-pitch-mode 1)))
(setq org-clock-string-limit 80
      org-log-done t
      org-agenda-span 'day
      org-agenda-include-diary t
      org-deadline-warning-days 1
      org-clock-idle-time 30
      org-catch-invisible-edits 'error
      org-agenda-sticky t
      org-agenda-start-with-log-mode nil
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s)"
                                    "WAITING(w@/!)" "|" "DONE(d)"
                                    "OBSOLETE(o)")
                          (type "PERMANENT")
                          (sequence "REVIEW(r)" "SEND(e)" "EXTREVIEW(g)" "RESPOND(p)" "SUBMIT(u)" "CLEANUP(c)"
                                    "|" "SUBMITTED(b)"))
      org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" tags-todo "+someday"
         ((org-show-hierarchy-above nil) (org-agenda-todo-ignore-with-date t)
          (org-agenda-tags-todo-honor-ignore-options t)))
        ("0" "Critical tasks" ((agenda "") (tags-todo "+p0")))
        ("l" "Agenda and live tasks" ((agenda)
                                      (todo "PERMANENT")
                                      (todo "WAITING|EXTREVIEW")
                                      (tags-todo "-someday/!-WAITING-EXTREVIEW")))
        ("S" "Last week's snippets" tags "TODO=\"DONE\"+CLOSED>=\"<-1w>\""
         ((org-agenda-overriding-header "Last week's completed TODO: ")
          (org-agenda-skip-archived-trees nil))))
      org-enforce-todo-dependencies t
      org-agenda-todo-ignore-scheduled t
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-skip-deadline-if-done 't
      org-agenda-skip-scheduled-if-done 't
      org-src-window-setup 'other-window
      org-src-tab-acts-natively t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-edit-src-content-indentation 0
      org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-use-sub-superscripts "{}"
      org-startup-with-inline-images t
      org-agenda-prefix-format '((agenda . " %i %-18:c%?-12t% s")
                                 (timeline . "  % s")
                                 (todo . " %i %-18:c")
                                 (tags . " %i %-18:c")
                                 (search . " %i %-18:c"))
      org-modules '(org-bbdb org-docview org-info org-jsinfo org-wl org-habit org-gnus org-habit org-inlinetask)
      org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "NOTES")
      org-cycle-separator-lines 0
      org-blank-before-new-entry '((heading) (plain-list-item . auto))
      org-clock-into-drawer nil
      org-clock-report-include-clocking-task t
      org-clock-history-length 20
      org-use-property-inheritance t
      org-link-abbrev-alist '(("CL" . "http://cl/%s") ("BUG" . "http://b/%s"))     
      org-refile-targets '((nil :maxlevel . 5))
      org-use-speed-commands t
      org-refile-targets '((nil . (:maxlevel . 3)))
      org-link-frame-setup '((gnus . gnus)
                             (file . find-file-other-window))
      org-speed-commands-user '(("w" . ash-org-start-work))
      org-completion-use-ido t
      org-use-fast-todo-selection t
      org-habit-show-habits t)
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

(use-package org-pomodoro
  :after (org-plus-contrib))

(use-package org-roam
  :straight (:host github :repo "jethrokuan/org-roam")
  :config
  (run-with-idle-timer 60 t 'org-roam-build-cache)
  (require 'org-roam)
  (org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org"))

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

(defun ash/tangle-config ()
  "Tangle the config file to a standard config file."
  (interactive)
  (org-babel-tangle 0 "~/.emacs.d/init.el"))

(general-define-key :keymaps 'org-mode-map
            :predicate '(string-equal "emacs.org" (buffer-name))
            "C-c t" 'ash/tangle-config)

(defun ash/find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/emacs.org"))

(defun ash/strdec-to-hex (n)
  "Given a decimal as a string, convert to hex.
This has to be done as a string to handle 64-bit or larger ints."
  (concat "0x" (replace-regexp-in-string "16#" "" (calc-eval `(,n calc-number-radix 16)))))

(let ((per-machine-filename "~/.emacs.d/permachine.el"))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

(setq epa-pinentry-mode 'loopback)

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

(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0))                               ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
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
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line.
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
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

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq-default use-package-always-ensure t)
(require 'use-package)

(use-package general
  :config
  ;; Let's make the top-level key categories here
  (general-create-definer ash/key-def :prefix "C-c"))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))
(use-package helm-proc)
(use-package helm-flycheck)
(use-package helm-notmuch)
(use-package helm-swoop
  :ensure t
  :bind (("M-m" . helm-swoop)
         ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))
(use-package helm-org-rifle)

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

(use-package counsel)

(use-package multiple-cursors
  :pin melpa
  :general)

(use-package phi-search
  :bind (("M-C-s" . phi-search)
         ("M-C-r" . phi-search-backward)))

(use-package expand-region)

(use-package swiper
  :bind (("M-s" . swiper)))

(use-package hydra
  :config
  ;; define everything here

  (defhydra hydra-jumps ()
    "
^Jump visually^    ^Jump via minibuffer^   ^Jump & go^  
---------------------------------------------------------
_j_: to word       _i_: to func heading      _u_: open url
_l_: to line                               _k_: open link
_c_: to char                               _b_: open bookmark
_r_: resume
"
    ("j" avy-goto-word-1)
    ("l" avy-goto-line)
    ("c" avy-goto-char)
    ("r" avy-resume)
    ("u" ash/avy-open-url)
    ("b" counsel-bookmark)
    ("i" counsel-imenu)
    ("k" counsel-ace-link)
    ("=" hydra-all/body "back" :exit t))
  (defhydra hydra-persp ()
    ("a" persp-add-buffer "add buffer")
    ("n" persp-add-new "new")
    ("r" persp-remove-bubber "remove buffer")
    ("s" persp-switch "switch")
    ("k" persp-kill "kill")
    ("=" hydra-all/body "back" :exit t))
  (defhydra hydra-structural ()
    ("i" sp-change-inner "change inner")
    ("k" sp-kill-sexp "kill sexp")
    ("b" sp-beginning-of-sexp "beginning of sexp")
    ("e" sp-end-of-sexp "end of sexp")
    ("d" sp-down-sexp "down sexp")
    ("e" sp-up-sexp "up sexp")
    ("]" sp-slurp-hybrid-sexp "slurp")
    ("/" sp-swap-enclusing-sexp "swap enclusing")
    ("r" sp-rewrap-sexp "rewrap")
    ("=" hydra-all/body "back" :exit t))
  (defhydra hydra-multiple-cursors ()
    ("l" mc/edit-lines "edit lines" :exit t)
    ("n" mc/mark-next-like-this "mark next like this")
    ("N" mc/skip-to-next-like-this "skip to next like this")
    ("M-n" mc/unmark-next-like-this "unmark next like this")
    ("p" mc/mark-previous-like-this "mark previous like this")
    ("P" mc/skip-to-previous-like-this "skip to previous like this")
    ("M-p" mc/unmark-previous-like-this "unmark previous like this")
    ("s" mc/mark-all-in-region-regexp "mark all in region re" :exit t)
    ("0" mc/insert-numbers "insert numbers" :exit t)
    ("a" mc/mark-all-like-this "mark all" :exit t)
    ("A" mc/insert-letters "insert letters" :exit t)
    ("d" mc/mark-all-dwim "mark dwim" :exit t)
    ("n" mc/mark-next-lines "mark next lines")
    ("=" hydra-all/body "back" :exit t))
  (defhydra hydra-expand ()
    ("e" er/expand-region "expand")
    ("c" er/contract-region "contract")
    ("d" er/mark-defun "defun")
    ("\"" er/mark-inside-quotes "quotes")
    ("'" er/mark-inside-quotes "quotes")
    ("p" er/mark-inside-pairs "pairs")
    ("." er/mark-method-call "call")
    ("=" hydra-all/body "back" :exit t))
  (defhydra hydra-flycheck ()
    ("n" flymake-goto-next-error "next error")
    ("p" flymake-goto-prev-error "previous error")
    ("d" flymake-goto-diagnostic "diagnostic")
    ("<" flycheck-prev-error "previous flycheck error")
    (">" flycheck-next-error "next flycheck error")
    ("l" flycheck-list-errors "list")
    ("=" hydra-all/body "back" :exit t))
  ;; notmuch is too specialized to be set up here, it varies from machine to
  ;; machine. At some point I should break it down into the general &
  ;; specialized parts.
  (defhydra hydra-mail ()
    ("n" notmuch-hello "notmuch")
    ("s" notmuch-search "search")
    ("h" helm-notmuch "helm search" :exit t)
    ("c" notmuch-mua-new-mail "compose")
    ("=" hydra-all/body "back" :exit t))
  (defhydra hydra-org-main ()
    ("a" org-agenda "agenda")    
    ("r" helm-org-rifle "rifle"))
  (defhydra hydra-all ()
    ("j" hydra-jumps/body "jumps" :exit t)
    ("p" hydra-persp/body "perspective" :exit t)
    ("s" hydra-structural/body  "structural" :exit t)
    ("c" hydra-multiple-cursors/body "multiple cursors" :exit t)
    ("e" hydra-expand/body "expand region" :exit t)
    ("m" hydra-mail/body "mail" :exit t)
    ("E" hydra-flycheck/body "errors" :exit t)
    ("o" hydra-org-main/body "org" :exit t))

  (global-set-key (kbd "M-p") 'hydra-all/body)
  (global-set-key (kbd "C-c c") 'hydra-all/body)
  (global-set-key (kbd "s-c") 'hydra-all/body))

(use-package major-mode-hydra
  :bind
  ("M-o" . major-mode-hydra)
  :config
  ;; Mode maps
  (major-mode-hydra-bind org-mode "Movement"
    ("u" org-up-element "up")
    ("n" org-next-visible-heading "next visible heading")
    ("j" (lambda () (interactive)
           (let ((org-goto-interface 'outline-path-completionp)
                 (org-outline-path-complete-in-steps nil))
             (org-goto))) "jump")
    ("l" org-next-link "next link")
    ("L" org-previous-link "previous link")
    ("b" org-next-block "next block")
    ("B" org-prev-block "previous block")
    ("o" org-open-at-point "open at point")
    ("i" org-insert-heading-respect-content "insert heading"))
  (major-mode-hydra-bind emacs-lisp-mode "Eval"
    ("b" eval-buffer "eval buffer")
    (";" eval-expression "eval expression")
    ("d" eval-defun "eval defun")
    ("D" edebug-defun "edebug defun")
    ("e" eval-last-sexp "eval last sexp")
    ("E" edebug-eval-last-sexp "edebug last sexp")
    ("i" ielm "ielm")))

(use-package persp-mode
  :config
  (setq persp-kill-foreign-buffer-behaviour 'kill)
  (defun persp-uncontained-buffer-p (buffer)
    (not (persp-contain-buffer-p buffer)))
  
  (persp-set-keymap-prefix (kbd "C-c l"))
  (persp-mode 1))

(with-eval-after-load "persp-mode"
  (with-eval-after-load "helm-mode"
    (require 'helm-for-files)
    (defvar helm-mini-tail-sources (cdr helm-mini-default-sources))
    (defvar helm-persp-completing-read-handlers
      '((switch-to-buffer                 . helm-persp-buffer-list-bridge)
        (kill-buffer                      . helm-persp-buffer-list-bridge)
        (persp-kill-buffer                . helm-persp-buffer-list-bridge)
        (persp-temporarily-display-buffer . helm-persp-buffer-list-bridge)
        (persp-add-buffer                 . helm-persp-buffer-list-bridge)
        (persp-remove-buffer              . helm-persp-buffer-list-bridge)))

    (defclass helm-persp-free-buffers-source (helm-source-buffers)
      ((buffer-list
        :initarg :buffer-list
        :initform #'(lambda () (mapcar #'buffer-name (persp-buffer-list-restricted nil 3)))
        :custom function
        :documentation
        "  A function with no arguments to create buffer list.")))

    (defvar helm-source-persp-free-buffers
      (helm-make-source "Free buffers"
          'helm-persp-free-buffers-source
        :fuzzy-match t))


    (defun helm-persp-buffers-list--init ()
      (let* ((buffers (funcall (helm-attr 'buffer-list)))
             (result (cl-loop for b in buffers
                              maximize (length b) into len-buf
                              maximize (length (with-current-buffer b
                                                 (format-mode-line mode-name)))
                              into len-mode
                              finally return (cons len-buf len-mode))))
        (unless (default-value 'helm-buffer-max-length)
          (helm-set-local-variable 'helm-buffer-max-length (car result)))
        (unless (default-value 'helm-buffer-max-len-mode)
          (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))
        (helm-attrset 'candidates buffers)))

    (defclass helm-persp-buffers-source (helm-source-buffers)
      ((buffer-list
        :initarg :buffer-list
        :initform #'(lambda () (mapcar #'buffer-name (persp-buffers (helm-attr 'persp))))
        :custom function
        :documentation
        "  A function with no arguments to create buffer list.")
       (persp
        :initarg :persp
        :initform (get-current-persp))
       (init :initform #'helm-persp-buffers-list--init)))

    (defvar helm-persp-sources-list '(helm-source-persp-free-buffers))
    (defvar helm-persp-source-name-prefix "helm-source-persp-buffers-list-")

    (defmacro persp-helm--liftup-source (source-name)
      `(progn
         (setq helm-persp-sources-list
               (cons ,source-name
                     (cl-delete ,source-name helm-persp-sources-list)))
         (setq helm-mini-default-sources
               (append helm-persp-sources-list
                       helm-mini-tail-sources))))

    (defmacro persp-helm--soure-name-from-persp-name (pn)
      `(intern (concat helm-persp-source-name-prefix ,pn)))

    (add-hook 'persp-created-functions
              #'(lambda (p ph)
                  (when (and (eq ph *persp-hash*) p)
                    (let* ((pn (persp-name p))
                           (source-name (persp-helm--soure-name-from-persp-name pn)))
                      (eval
                       `(defvar ,source-name
                          (helm-make-source ,(concat pn " buffers")
                              'helm-persp-buffers-source :persp ,p)))
                      (setq helm-persp-sources-list
                            (append helm-persp-sources-list (list source-name))))
                    (setq helm-mini-default-sources
                          (append helm-persp-sources-list
                                  helm-mini-tail-sources)))))

    ;; (add-hook 'persp-before-switch-functions
    ;;           #'(lambda (next-pn)
    ;;               (let ((p (get-current-persp)))
    ;;                 (when p
    ;;                   (persp-helm--liftup-source 'helm-source-persp-free-buffers)))))

    ;; (add-hook 'persp-activated-hook
    ;;           #'(lambda ()
    ;;               (let ((p (get-current-persp)))
    ;;                 (when p
    ;;                   (let* ((pn (persp-name p))
    ;;                          (source-name (intern (concat helm-persp-source-name-prefix pn))))
    ;;                     (persp-helm--liftup-source source-name))))))

    (add-hook 'persp-before-kill-functions
              #'(lambda (p)
                  (when p
                    (let* ((pn (persp-name p))
                           (source-name (persp-helm--soure-name-from-persp-name pn)))
                      (setq helm-persp-sources-list
                            (cl-delete source-name helm-persp-sources-list))
                      (setq helm-mini-default-sources
                            (append helm-persp-sources-list
                                    helm-mini-tail-sources))
                      (makunbound source-name)))))

    (add-hook 'persp-mode-hook #'(lambda ()
                                   (if persp-mode
                                       (persp-helm-setup-bridge)
                                     (persp-helm-destroy-bridge))))

    (defun helm-persp-mini ()
      (interactive)
      (persp-helm--liftup-source 'helm-source-persp-free-buffers)
      (let* ((cbuf (current-buffer))
             (cbn (buffer-name cbuf)))
        (let ((persp (get-current-persp)))
          (when (and persp (persp-contain-buffer-p cbuf persp))
            (let ((source-name (persp-helm--soure-name-from-persp-name (persp-name persp))))
              (persp-helm--liftup-source source-name))))
        (or
         (helm :sources helm-mini-default-sources
               :ff-transformer-show-only-basename nil
               :fuzzy-match helm-mode-fuzzy-match
               :buffer "*helm persp mini*"
               :keymap helm-buffer-map
               :truncate-lines helm-buffers-truncate-lines
               :default cbn
               :preselect (substring cbn 0 (min (string-width cbn) helm-buffer-max-length)))
         (helm-mode--keyboard-quit))))

    (defun helm-persp-buffer-list-bridge
        (prompt _collection &optional test _require-match init hist default _inherit-im name buffer)
      (persp-helm--liftup-source 'helm-source-persp-free-buffers)
      (let ((persp (get-current-persp)))
        (when (and persp (persp-contain-buffer-p (current-buffer) persp))
          (let ((source-name (persp-helm--soure-name-from-persp-name (persp-name persp))))
            (persp-helm--liftup-source source-name))))
      (let ((deflt (or default "")))
        (or
         (helm :sources helm-persp-sources-list
               :fuzzy-match helm-mode-fuzzy-match
               :prompt prompt
               :buffer buffer
               :input init
               :history hist
               :resume 'noresume
               :keymap helm-buffer-map
               :truncate-lines helm-buffers-truncate-lines
               :default deflt
               :preselect (substring deflt 0 (min (string-width deflt) helm-buffer-max-length)))
         (helm-mode--keyboard-quit))))

    (defun persp-helm-setup-bridge ()
      (setq helm-completing-read-handlers-alist
            (append helm-persp-completing-read-handlers
                    helm-completing-read-handlers-alist))
      (global-set-key (kbd "C-x b") #'helm-persp-mini))
    (defun persp-helm-destroy-bridge ()
      (setq helm-mini-default-sources
            (cons
             'helm-source-buffers-list
             helm-mini-tail-sources))
      (dolist (it helm-persp-completing-read-handlers)
        (setq helm-completing-read-handlers-alist
              (delete it helm-completing-read-handlers-alist)))
      (global-set-key (kbd "C-x b") #'helm-mini))

    (when (bound-and-true-p persp-mode)
      (persp-helm-setup-bridge))))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq-default yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yas-reload-all)
  (yas-global-mode 1))

(use-package magit)

(use-package smartparens
  :diminish ""
  :init (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  :config (require 'smartparens-config))

(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode))

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
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 0))

(use-package which-key
  :diminish
  :config (which-key-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h h" . helpful-at-point)
         ("C-h c" . helpful-command)))

(set-face-attribute 'default nil :family "Iosevka" :height 130)
(set-face-attribute 'fixed-pitch nil :family "Iosevka")
(set-face-attribute 'variable-pitch nil :family "EtBembo")
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (variable-pitch-mode 1))))
(use-package poet-theme)

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

(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package powerline
    :config
    (setq powerline-default-separator 'utf-8)
    (powerline-center-theme))

(use-package emacs-org-dnd
  :disabled
  :ensure nil
  :load-path "~/src/emacs-org-dnd"
  :config (require 'ox-dnd))

(use-package org
  :ensure org-plus-contrib
  :config
  (require 'org-checklist)
  :general
  ("C-c a" 'org-agenda))

(require 'org-tempo)

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
      org-clock-idle-time 10
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
          (org-agenda-skip-archived-trees nil)
          (org-agenda-files '("$HOME/org/work.org" "$HOME/org/journal.org")))))
      org-agenda-files '("$HOME/org/work.org" "$HOME/org/journal.org")
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
      org-startup-with-inline-images t
      org-agenda-prefix-format '((agenda . " %i %-18:c%?-12t% s")
                                 (timeline . "  % s")
                                 (todo . " %i %-18:c")
                                 (tags . " %i %-18:c")
                                 (search . " %i %-18:c"))
      org-modules '(org-bbdb org-docview org-info org-jsinfo org-wl org-habit org-gnus org-habit org-inlinetask)
      org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "NOTES")
      org-clock-into-drawer nil
      org-clock-report-include-clocking-task t
      org-clock-history-length 20
      org-archive-location "$HOME/org/journal.org::datetree/* Archived"
      org-use-property-inheritance t
      org-link-abbrev-alist '(("CL" . "http://cl/%s") ("BUG" . "http://b/%s"))
      org-agenda-clockreport-parameter-plist
      '(:maxlevel 2 :link nil :scope ("$HOME/org/work.org"))
      org-refile-targets '((nil :maxlevel . 5))
      org-use-speed-commands t
      org-refile-targets '((nil . (:maxlevel . 3)))
      org-link-frame-setup '((gnus . gnus)
                             (file . find-file-other-window))
      org-speed-commands-user '(("w" . ash-org-start-work))
      org-completion-use-ido t
      org-use-fast-todo-selection t
      org-habit-show-habits t
      org-capture-templates
      '(("n" "Note" entry
         (file+headline "notes.org" "Unfiled notes")
         "* %a%?\n%u\n%i")
        ("j" "Journal" entry
         (file+datetree "journal.org")
         "* %T %?")
        ("t" "Todo" entry
         (file+headline "work.org" "Inbox")
         "* TODO %?\n%a")
        ("a" "Act on email" entry
         (file+headline "work.org" "Inbox")
         "* TODO %?, Link: %a")))

(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

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

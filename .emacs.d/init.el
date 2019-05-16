(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (setq package-archive-priorities '(("melpa" . 0) ("melpa-stable" . 1)))
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(defun ash/tangle-config ()
  "Tangle the config file to a standard config file."
  (interactive)
  (org-babel-tangle 0 "~/.emacs.d/init.el"))

(defun ash/find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/emacs.org")
  (local-set-key (kbd "C-c T") 'ash/tangle-config))

(global-set-key (kbd "C-c I") 'ash/find-config)

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

(use-package general)

(use-package ivy
  :diminish ""
  :config (ivy-mode)
  (setq ivy-initial-inputs-alist nil))

(use-package avy
  :config
  (advice-add 'spacemacs/avy-goto-url :after (lambda () (browse-url-at-point)))
  :general
  (:prefix "C-c j"
	   "" '(nil :which-key "Jumps")
	   "j" 'avy-goto-word-1
	   "l" 'avy-goto-line
	   "c" 'avy-goto-char))

(use-package multiple-cursors
  :general
  (:prefix "C-c m"
           "" '(nil :which-key "Multiple cursors")
           ">" 'mc/mark-next-like-this
           "<" 'mc/mark-previous-like-this
           "a" 'mc/mark-all-like-this
           "m" 'mc/mark-all-dwim
           "d" 'mc/mark-all-like-this-in-defun
           "n" 'mc/mark-next-lines))

(use-package phi-search
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)))

(use-package expand-region
  :general
  (:prefix "C-c e"
           "" '(nil :which-key "Expand / Contract")
           "e" 'er/expand-region
           "c" 'er/contract-region
           "d" 'er/mark-defun
           "\"" 'er/mark-inside-quotes
           "'" 'er/mark-inside-quotes
           "p" 'er/mark-inside-pairs
           "." 'er/mark-method-call))

(use-package swiper
  :bind (("M-s" . swiper)))

(use-package hydra
  :config
  (require 'org)
  (defhydra hydra-org (org-mode-map "C-;")
    "
^Navigation^         ^Inserting^
^^^^^^^^^^^^^------------------------------
_u_: up to parent    _i_: insert heading
_n_: next heading  
_j_: jump          
_L_: prev link
_l_: next link
_B_: prev block
_b_: next block
_o_: open link
"
    ("u" org-up-element)
    ("n" org-next-visible-heading)
    ("j" (lambda () (interactive)
	   (let ((org-goto-interface 'outline-path-completionp)
		 (org-outline-path-complete-in-steps nil))
	     (org-goto))))
    ("l" org-next-link)
    ("L" org-previous-link)
    ("b" org-next-block)
    ("B" org-prev-block)
    ("o" org-open-at-point)
    ("i" org-insert-heading-respect-content)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq-default yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yas-reload-all)
  (yas-global-mode 1))

(use-package magit
  :general
  (:prefix "C-c s"
           "" '(nil :which-key "Source code")
           "m" 'magit-status))

(use-package smartparens
  :diminish ""
  :init (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  :general
  (:prefix "C-c p"
           "" '(nil :which-key "Parens")
           "i" 'sp-change-inner
           "k" 'sp-kill-sexp
           "b" 'sp-beginning-of-sexp
           "e" 'sp-end-of-sexp
           "d" 'sp-down-sexp
           "u" 'sp-up-sexp
           "]" 'sp-slurp-hybrid-sexp
           "/" 'sp-swap-enclusing-sexp
           "r" 'sp-rewrap-sexp)
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
  :init (add-hook 'after-init-hook 'global-company-mode))

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :prefix "C-c C-c"
 "" '(nil :which-key "elisp mode")
 "e b" 'eval-buffer
 "e e" 'eval-expression
 "e d" 'eval-defun
 "i" 'ielm)

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
	  (org-agenda-files '("/usr/local/google/home/ahyatt/org/work.org" "/usr/local/google/home/ahyatt/org/journal.org")))))
      org-agenda-files '("/usr/local/google/home/ahyatt/org/work.org" "/usr/local/google/home/ahyatt/org/journal.org")
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
      org-archive-location "/usr/local/google/home/ahyatt/org/journal.org::datetree/* Archived"
      org-use-property-inheritance t
      org-link-abbrev-alist '(("CL" . "http://cl/%s") ("BUG" . "http://b/%s"))
      org-agenda-clockreport-parameter-plist
      '(:maxlevel 2 :link nil :scope ("/usr/local/google/home/ahyatt/org/work.org"))
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
	 (file+headline "/usr/local/google/home/ahyatt/org/notes.org" "Unfiled notes")
	 "* %a%?\n%u\n%i")
	("j" "Journal" entry
	 (file+datetree "/usr/local/google/home/ahyatt/org/journal.org")
	 "* %T %?")
	("t" "Todo" entry
	 (file+headline "/usr/local/google/home/ahyatt/org/work.org" "Inbox")
	 "* TODO %?\n%a")
	("a" "Act on email" entry
	 (file+headline "/usr/local/google/home/ahyatt/org/work.org" "Inbox")
	 "* TODO %?, Link: %a")
	("c" "Contacts" entry (file "/usr/local/google/home/ahyatt/org/contacts.org")
	 "* %(org-contacts-template-name)
		  :PROPERTIES:
		  :EMAIL: %(org-contacts-template-email)
		  :END:")))

(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

(load "~/.emacs.d/google.el")

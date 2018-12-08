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
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
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

(use-package ivy
  :diminish ""
  :config (ivy-mode))

(use-package avy
  :bind (("C-c j j" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)))

(use-package objed
  :ensure nil
  :load-path "~/src/objed"
  :config (objed-mode))

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

(use-package smartparens
  :diminish ""
  :init (add-hook 'prog-mode-hook #'smartparens-strict-mode))

(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode))

(use-package multiple-cursors)

(use-package magit
  :bind ("C-x g" . magit-status)
  :disabled)

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

(use-package which-key
  :diminish
  :config (which-key-mode 1))

(set-face-attribute 'default nil :family "Iosevka" :height 130)
(set-face-attribute 'fixed-pitch nil :family "Iosevka")
(set-face-attribute 'variable-pitch nil :family "EtBembo")
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (variable-pitch-mode 1))))
(use-package poet-theme)

(use-package org-bullets
  :init (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package typo
  :diminish ""
  :init
  (add-hook 'org-mode-hook #'typo-mode))

(setq-default org-startup-indented t
              org-bullets-bullet-list '("①②③④⑤⑥⑦⑧⑨") 
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

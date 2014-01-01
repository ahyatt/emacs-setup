(add-to-list 'load-path "~/.emacs.d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General behavior setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General variable setting
(setq
 ;; Column numbers are useful when programming, in case you want to
 ;; check if things are over some line length limit. There are special
 ;; tools for this, such as fci-mode, but this is a good backup tool
 ;; to have around.
 column-number-mode t
 ;; No backup files.  Most things I do are under source control.
 make-backup-files nil
 ;; Update isn't paused just because input is detection. This might be
 ;; a speedup.
 redisplay-dont-pause t
 ;; Mac customization, from http://whattheemacsd.com/mac.el-01.html
 mac-command-modifier 'meta
 mac-option-modifier 'super
 ns-function-modifier 'hyper
 custom-file "~/.emacs.d/custom.el")

;; This seems to be needed due to some jabber-related auto-fill
;; corruption that happens, turning on c-do-auto-fill as the auto-fill
;; function for all buffers.  It is really mysterious.
(defun c-do-auto-fill ())

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per-machine variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ash/org-home "~/org/")

;; Load the customizations file, if it exists. If it doesn't exist,
;; don't throw an error or complain.
(load custom-file t t)

(require 'package)
;; Load work-specific file, if there.
(require 'ahyatt-google nil t)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; Package setup, taken from
;; https://github.com/zane/dotemacs/blob/master/zane-packages.el#L62
;; This is just the very basic set of packages.
(setq ash/bootstrap-packages
      '(use-package
           bang
         diminish))

(defun ash/show-trailing-whitespace ()
  (set (make-local-variable 'whitespace-style)
       '(face trailing)))

(add-hook 'c++-mode-hook 'ash/show-trailing-whitespace)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defadvice yank (after c-indent-after-yank activate)
  "Do an indent after a yank"
  (if (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode)
      (let ((transient-mark-mode nil))
        (indent-region (region-beginning) (region-end) nil))))

(savehist-mode 1)
(recentf-mode 1)
(tool-bar-mode -1)
(display-time-mode 1)

;; Recentf is useless without saving frequently
(run-with-idle-timer 1 nil 'recentf-save-list)

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))

(add-to-list 'Info-default-directory-list "~/.emacs.d/info/")

(define-key global-map "\C-x\C-j" 'dired-jump)
(setq nxml-slash-auto-complete-flag t)

;; from http://www.method-combination.net/blog/archives/2011/03/11/speeding-up-emacs-saves.htlm
(setq vc-handled-backends nil)

;; Save every time things are changed
(setq bookmark-save-flag 1)

(setq mode-line-modes nil)

(defun ash-set-frame-font-points (points)
  (interactive "nPoints: ")
  (require 'font-utils)
  (set-frame-parameter
   (selected-frame) 'font
   (concat
    (font-utils-name-from-xlfd (frame-parameter nil 'font)) "-"
    (int-to-string points))))

(defun ash-reapply-theme (frame)
  (save-excursion
    (dolist (theme custom-enabled-themes)
      (enable-theme theme))))

;; Adapted from http://irreal.org/blog/?p=1536
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-;") 'push-mark-no-activate)
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "C-:") 'jump-to-mark)

(defun mirror-buffer ()
  (interactive)
  (cond ((= (length (window-list)) 2)
         (switch-to-buffer (window-buffer (next-window))))
        ((= (length (window-list)) 1)
         (split-window))
        (t (error "There must be 2 or less windows to mirror the current buffer"))))

(setq comint-input-ignoredups t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap for package setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)
;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p ash/bootstrap-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))

;; So that we can require encyrpted files (this will ask for a password).
(add-to-list 'load-suffixes ".el.gpg")

;; This stops matching regardless of case, which is better for
;; camel-cased words.
(setq dabbrev-case-fold-search nil)

(require 'use-package)
(require 'diminish)

;;;;;;;;;;;;;;;;;;;
;; Package setup ;;
;;;;;;;;;;;;;;;;;;;

(use-package dynamic-fonts
  :ensure dynamic-fonts
  :init
  (progn (setq dynamic-fonts-preferred-proportional-fonts
               '("Source Sans Pro" "DejaVu Sans" "Helvetica"))
         (setq dynamic-fonts-preferred-monospace-fonts
               '("Source Code Pro" "Inconsolata" "Monaco" "Consolas" "Menlo"
                 "DejaVu Sans Mono" "Droid Sans Mono Pro" "Droid Sans Mono")))
  :config
  ;; If we started with a frame, just setup the fonts, otherwise wait until
  ;; we make a frame.
  ;; NOTE: This doesn't actually work with daemon mode.  I don't know
  ;; why not yet.  Maybe add a delay as a hack?
  (if initial-window-system
      (dynamic-fonts-setup)
    (add-to-list 'after-make-frame-functions
                 (lambda (frame) (dynamic-fonts-setup)))))

(use-package projectile
  :defer t
  :config (setq projectile-enable-caching t))

(use-package fill-column-indicator
  :ensure fill-column-indicator
  :defer t
  :init (progn (setq fci-style 'rule)
               (defun ash/c-fci ()
                 (setq fill-column 80)
                 (fci-mode))
               
               (defun ash/java-fci ()
                 (setq fill-column 100)
                 (fci-mode))
               (add-hook 'c++-mode-hook 'ash/c-cfi)
               (add-hook 'java-mode 'ash/java-fci)))

(use-package change-inner
  :ensure change-inner
  :bind (("\C-c i" . change-inner)
         ("s-i" . change-inner)
         ("\C-c u" . change-outer)
         ("s-o" . change-outer))
  :config (progn
            ;; This is much like change-inner, but doesn't require us to identify
            ;; additional input.
            (defun ash-clear ()
              (interactive)
              (require 'expand-region)
              (er/expand-region 1)
              (kill-region (region-beginning) (region-end))
              (er/expand-region 0))))

(use-package go-mode
  :ensure go-mode
  :mode ("\\.go$" . go-mode)
  :init (progn  (defun ash/go-initialization ()
                  (setq tab-width 2))
                (add-hook 'go-mode-hook 'ash/go-initialization)))

(use-package ido
  :ensure ido-ubiquitous
  :ensure ido-vertical-mode
  :ensure flx-ido
  :defer t
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x C-f" . ido-find-file))
  :config (progn 
            (setq ido-enable-tramp-completion nil)
            (setq ido-usr-url-at-point t)
            (add-to-list 'ido-ignore-files "flymake.cc")
            (require 'ido-vertical-mode)
            (ido-vertical-mode 1)
            ;; ido mode hack to get java to javatests rotation
            (defun ash/cycle-java ()
              (interactive)
              (cond ((string-match "java/" ido-current-directory)
                     (ido-set-current-directory (replace-regexp-in-string "java/" "javatests/" ido-current-directory)))
                    ((string-match "javatests/" ido-current-directory)
                     (ido-set-current-directory (replace-regexp-in-string "javatests/" "java/" ido-current-directory))))
              (setq ido-exit 'refresh
                    ido-rescan t
                    ido-rotate-temp t)
              (exit-minibuffer))
            (defun ash/ido-mode-map-setup ()
              (define-key ido-completion-map (kbd "C-;") 'ash/cycle-java))
            (add-hook 'ido-setup-hook 'ash/ido-mode-map-setup)))

(use-package paredit
  :ensure paredit
  :defer t
  :init
  (progn  (add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))
          (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))))

(use-package guide-key
  :ensure guide-key
  :init (progn
          (setq guide-key/guide-key-sequence
                '("C-x r" "C-x 4" "C-c ," "C-x x" "C-c" "C-c m" "C-c &"
                  "C-c o" "C-x RET" "C-x n"))
          (guide-key-mode 1)))

(use-package org 
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c g" . org-store-link))
  :init
  (progn
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
            ("l" "Agenda and live tasks" ((agenda "")
                                          (todo "PERMANENT")
                                          (todo "WAITING|EXTREVIEW")
                                          (tags-todo "-someday/!-WAITING-EXTREVIEW")))
            ("S" "Last week's snippets" tags "TODO=\"DONE\"+CLOSED>=\"<-1w>\""
             ((org-agenda-overriding-header "Last week's completed TODO: ")
              (org-agenda-skip-archived-trees nil)
              (org-agenda-files '((concat ash/org-home "work.org")
                                  (concat ash/org-home "journal.org"))))))
          org-enforce-todo-dependencies t
          org-agenda-todo-ignore-scheduled t
          org-agenda-dim-blocked-tasks 'invisible
          org-agenda-tags-todo-honor-ignore-options t
          org-agenda-skip-deadline-if-done 't
          org-agenda-skip-scheduled-if-done 't
          org-agenda-prefix-format '((agenda . " %i %-18:c%?-12t% s")
                                     (timeline . "  % s")
                                     (todo . " %i %-18:c")
                                     (tags . " %i %-18:c")
                                     (search . " %i %-18:c"))
          org-modules '(org-bbdb org-docview org-info org-jsinfo org-wl org-habit)
          org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "NOTES")
          org-clock-into-drawer nil
          org-clock-report-include-clocking-task t
          org-clock-history-length 20
          org-archive-location (concat ash/org-home "journal.org::datetree/* Archived")
          org-use-property-inheritance t
          org-hide-leading-stars nil
          org-startup-indented t
          org-agenda-clockreport-parameter-plist
          '(:maxlevel 2 :link nil :scope (concat ash/org-home "work.org"))
          org-refile-targets '((nil :maxlevel . 5)))

    ;; I like to cycle in the agenda instead of jump to state
    ;;  (defadvice org-agenda-todo (before ash-agenda-todo-prefer-cycling
    ;;                                   activate)
    ;; (ad-set-arg 0 (if (ad-get-arg 0) nil 'right)))

    (setq org-use-speed-commands t
          org-refile-targets '((nil . (:maxlevel . 3)))
          org-link-frame-setup '((gnus . gnus)
                                 (file . find-file-other-window))
          org-use-speed-commands t
          org-completion-use-ido t
          org-use-fast-todo-selection t)

    
    (defun ash-agenda ()
      (interactive)
      (let ((buf (get-buffer "*Org Agenda*")))
        (if buf
            (switch-to-buffer buf)
          (org-agenda-goto-today))
        (ash-jabber-colorize-tags)))
    (require 'org-gnus)

    (setq org-capture-templates
          '(("n" "Note" entry
             (file+headline (concat ash/org-home "notes.org") "Unfiled notes")
             "* %a%?\n%u\n%i")
            ("j" "Journal" entry
             (file+datetree (concat ash/org-home "journal.org"))
             "* %T %?")
            ("t" "Todo" entry
             (file+headline (concat ash/org-home "work.org") "Inbox")
             "* TODO %?\n%a")
            ("a" "Act on email" entry
             (file+headline (concat ash/org-home "work.org") "Inbox")
             "* TODO Respond to %:from on %:subject\n%U\n%a\n"
             :clock-in t :clock-resume t :immediate-finish t)
            ("c" "Contacts" entry (file (concat ash/org-home "contacts.org"))
             "* %(org-contacts-template-name)
                  :PROPERTIES:
                  :EMAIL: %(org-contacts-template-email)
                  :END:")))
    (defun ash-jabber-colorize-tags ()
      (when (featurep 'jabber)
        (let ((contact-hash (make-hash-table :test 'equal)))
          (dolist (jc jabber-connections)
            (dolist (contact (plist-get (fsm-get-state-data jc) :roster))
              (puthash (car (split-string (symbol-name contact) "@")) contact contact-hash)))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward ":\\(\\w+\\):" nil t)
              (let ((tag (match-string-no-properties 1)))
                (when (and tag (gethash tag contact-hash))
                  (let* ((js (jabber-jid-symbol (gethash tag contact-hash)))
                         (connected (get js 'connected))
                         (show (get js 'show)))
                    (if connected
                        (let ((o (make-overlay (match-beginning 1) (- (point) 1))))
                          (overlay-put o 'face
                                       (cons 'foreground-color
                                             (cond ((equal "away" show)
                                                    "orange")
                                                   ((equal "dnd" show)
                                                    "red")
                                                   (t "green")))))))))
              (backward-char))))))

    (defadvice org-agenda-finalize (after ash/after-agenda-display activate)
      (ash-jabber-colorize-tags))

    (setq org-default-notes-file (concat ash/org-home "notes.org"))
    (define-key global-map [f12] 'org-capture)

    (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

    (setq org-timer-default-timer 30)

    (setq org-export-babel-evaluate nil)

    (defun org-narrow-to-clocked-project ()
      (interactive)
      (save-excursion
        (org-clock-jump-to-current-clock)
        (switch-to-buffer (marker-buffer org-clock-marker))
        (org-up-heading-all 1)
        (org-narrow-to-subtree)
        (org-clock-jump-to-current-clock)
        (if (search-forward ":NOTES:" nil t)
            (progn (org-show-subtree)
                   (search-forward ":END:")
                   (forward-line -1)
                   (if (looking-at "^$") (insert "\t") (end-of-line)))
          (let ((begin (point)))
            (insert "\n:NOTES:\n\n:END:\n")
            (indent-region begin (point))
            (forward-line -2)
            (insert "\t")))))

    (defun org-widen-up ()
      (interactive)
      (widen)
      (org-up-heading-all 2)
      (org-narrow-to-subtree))

    (define-key global-map "\C-coj" 'org-narrow-to-clocked-project)
    (define-key global-map "\C-cou" 'org-widen-up)

    (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
    ;; always evaluate everything.  Livin' on the edge!
    (setq org-confirm-babel-evaluate (lambda (lang body) nil))
    (setq org-babel-sh-command "zsh")
    
    (defun ash/org-link-description (link)
      "Makes a useful description from a link."
      (cond ((string-match "^file:" link)
             (file-name-nondirectory link))
            (t nil)))
    
    (defun ash/org-paste-link ()
      "Paste all stored links without prompting."
      (interactive)
      (unwind-protect
          (flet ((read-string (prompt &optional initial-input history default-value
                                      inherit-input-method)
                              initial-input))
            (dolist (link (delete-duplicates org-stored-links :test 'equal))
              (org-insert-link nil (car link) (ash/org-link-description (car link)))))
        (setq org-stored-links nil)))
    
    (define-key org-mode-map (kbd "C-c p") 'ash/org-paste-link)
    ;; this function causes an annoying prompt for LAST_READ_MAIL.  Kill
    ;; the whole function for now.
    (eval-after-load 'org-contacts
      '(defun org-contacts-gnus-store-last-mail ()))
    ;; temporary fix for org-mode.  Why is this broken?  Must investigate...
    (defun org-indent-line-to (n))
    (modify-coding-system-alist 'file "\\.org\\'" 'utf-8)))

(use-package yasnippet
  :ensure yasnippet
  :diminish (yas-snippet . "")
  :defer t
  :init
  (progn
    (require 'dropdown-list)
    (setq yas/prompt-functions '(yas/dropdown-prompt
                                 yas/ido-prompt
                                 yas/completing-prompt)
          ;; Important to indent all the lines, including the first,
          ;; otherwise the snippet has wrong indentation.
          yas-also-auto-indent-first-line t
          yas-snippet-dirs (quote ("~/.emacs.d/snippets")))
    (yas-global-mode 1)))

(use-package jabber
  :ensure jabber
  :commands (jabber-connect jabber-connect-all)
  :init (progn
          ;; I don't like the jabber modeline having counts, it takes up too
          ;; much room.
          ;; (defadvice jabber-mode-line-count-contacts (around ash-remove-jabber-counts
          ;;    						(&rest ignore))
          ;;   "Override for count contacts, to remove contacts from modeline"
          ;;   (setq ad-return-value ""))
          ;; (ad-activate 'jabber-mode-line-count-contacts)

          (add-hook 'jabber-chat-mode-hook 'flyspell-mode)
          (when (featurep 'anything)
            (add-to-list 'anything-sources anything-c-source-jabber-contacts))
          (setq jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll)
                jabber-alert-muc-hooks '(jabber-muc-scroll)
                jabber-alert-presence-hooks (quote (jabber-presence-update-roster))
                jabber-autoaway-method (quote jabber-current-idle-time)
                jabber-mode-line-mode t
                jabber-vcard-avatars-retrieve nil)
          (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)
          ;; jabber roster redisplay is *slow* and slows down everything in
          ;; emacs.  I don't use the roster, so let's just make the
          ;; offending function a no-op
                     (defun jabber-display-roster ())))

(use-package edit-server
  :ensure edit-server
  :init (progn (edit-server-start)
               (add-hook 'edit-server-text-mode-hook (lambda () (visual-line-mode 1)))
               (add-hook 'edit-server-text-mode-hook (lambda () (flyspell-mode 1)))))

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :bind (("C-'" . ace-jump-mode)
         ("C-\"" . ace-jump-char-mode)))

(use-package key-chord
  :ensure key-chord
  :init (progn
          (key-chord-mode 1)
          (key-chord-define-global "jk" 'dabbrev-expand)
          (key-chord-define-global "l;" 'magit-status)
          (key-chord-define-global "`1" 'yas/expand)
          (key-chord-define-global "-=" (lambda () (interactive) (switch-to-buffer "*compilation*")))
          
          (key-chord-define-global "xb" 'recentf-ido-find-file)
          (key-chord-define-global "xg" 'smex)
          (key-chord-define-global "XG" 'smex-major-mode-commands)
          (key-chord-define-global "fj" 'ash-clear)
          (key-chord-define-global "0k" 'mirror-buffer)
          (key-chord-define-global "o\\" 'er/expand-region)
          (key-chord-define-global "p\\" 'er/contract-region)))

(use-package expand-region
  :ensure expand-region
  :commands (er/expand-region er/contract-region))

(use-package gnus
  :commands gnus
  :init
  (progn (setq gnus-agent t
               bbdb-always-add-addresses 'ash-add-addresses-p
               bbdb-complete-name-allow-cycling t
               bbdb-completion-display-record nil
               bbdb-silent-running t
               bbdb-use-pop-up nil
               bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
               bbdb/news-auto-create-p 'bbdb-ignore-some-messages-hook
               ;; This really speeds things up!
               gnus-nov-is-evil t
               nnimap-search-uids-not-since-is-evil t
               gnus-ignored-newsgroups "^$"
               mm-text-html-renderer 'w3m-standalone
               mm-attachment-override-types '("image/.*")
               ;; No HTML mail
               mm-discouraged-alternatives '("text/html" "text/richtext")
               gnus-message-archive-group "Sent"
               gnus-ignored-mime-types '("text/x-vcard")
               gnus-agent-queue-mail nil
               gnus-keep-same-level 't
               gnus-summary-ignore-duplicates t
               gnus-group-use-permanent-levels 't
               ;; From http://emacs.wordpress.com/2008/04/21/two-gnus-tricks/
               gnus-user-date-format-alist
               '(((gnus-seconds-today) . "Today, %H:%M")
                 ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
                 (604800 . "%A %H:%M") ;;that's one week
                 ((gnus-seconds-month) . "%A %d")
                 ((gnus-seconds-year) . "%B %d")
                 (t . "%B %d '%y"))
               ;; From http://www.emacswiki.org/emacs/init-gnus.el
               gnus-summary-line-format "%U%R%z%O %{%16&user-date;%}   %{%-20,20n%} %{%ua%} %B %(%I%-90,90s%)\n"
               gnus-summary-same-subject ""
               gnus-sum-thread-tree-indent "    "
               gnus-sum-thread-tree-single-indent "◎ "
               gnus-sum-thread-tree-root "● "
               gnus-sum-thread-tree-false-root "☆"
               gnus-sum-thread-tree-vertical "│"
               gnus-sum-thread-tree-leaf-with-other "├─► "
               gnus-sum-thread-tree-single-leaf "╰─► "
               gnus-single-article-buffer nil
               gnus-suppress-duplicates t)
         (defun gnus-user-format-function-a (header) 
           (let ((myself (concat "<" user-mail-address ">"))
                 (references (mail-header-references header))
                 (message-id (mail-header-id header)))
             (if (or (and (stringp references)
                          (string-match myself references))
                     (and (stringp message-id)
                          (string-match myself message-id)))
                 "X" "│")))))

(use-package term
  :commands ansi-term
  :init
  (progn
    (defun ash-term-hooks ()
      ;; dabbrev-expand in term
      (define-key term-raw-escape-map "/"
        (lambda ()
          (interactive)
          (let ((beg (point)))
            (dabbrev-expand nil)
            (kill-region beg (point)))
          (term-send-raw-string (substring-no-properties (current-kill 0)))))
      ;; yank in term (bound to C-c C-y)
      (define-key term-raw-escape-map "\C-y"
        (lambda ()
          (interactive)
          (term-send-raw-string (current-kill 0))))
      (setq term-default-bg-color (face-background 'default))
      (setq term-default-fg-color (face-foreground 'default)))
    (add-hook 'term-mode-hook 'ash-term-hooks)))

(use-package multiple-cursors
  :ensure multiple-cursors
  :bind (((kbd "C-c m m") . mc/edit-lines)
         ((kbd "C-c m a") . mc/edit-beginnings-of-lines)
         ((kbd "C-c m e") . mc/edit-ends-of-linds)
         ((kbd "C-c m r") . mc/set-rectangular-region-anchor)
         ((kbd "C-c m =") . mc/mark-all-like-this)
         ((kbd "C-c m n") . mc/mark-next-like-this)
         ((kbd "C-c m p") . mc/mark-previous-like-this)
         ((kbd "C-c m x") . mc/mark-more-like-this-extended)
         ((kbd "C-c m u") . mc/mark-all-in-region))
  :init (progn
          (key-chord-define-global "zm" 'mc/edit-lines)
          (key-chord-define-global "zr" 'set-rectangular-region-anchor)
          (key-chord-define-global "z=" 'mc/mark-all-like-this)
          (key-chord-define-global "i\\" 'mc/mark-all-like-this)
          (key-chord-define-global "zn" 'mc/mark-next-like-this)
          (key-chord-define-global "zp" 'mc/mark-previous-like-this)
          (key-chord-define-global "zx" 'mc/mark-more-like-this-extended)
          (key-chord-define-global "zu" 'mc/mark-all-in-region)))

(use-package undo-tree
  :ensure undo-tree
  :diminish ""
  :config
  (progn
    (setq undo-tree-auto-save-history nil
          undo-tree-visualizer-timestamps t)
    (global-undo-tree-mode 1)))

(use-package flyspell-lazy
  :ensure flyspell-lazy
  :defer t
  :init (flyspell-lazy-mode 1))

(use-package helm
  :ensure helm
  :disabled t
  :diminish ""
  :init (progn
          (add-to-list 'helm-c-boring-file-regexp-list "\\.~undo-tree~$")
          (add-to-list 'helm-c-boring-file-regexp-list "^#.*#$")
          (add-to-list 'helm-c-boring-file-regexp-list "^#.*#$")
          (setq helm-follow-mode-persistent t)
          (setq helm-input-idle-delay 0.001
                helm-idle-delay 0.001)))

(use-package smartparens
  :init
  (progn
    (sp-pair "'" nil :unless '(sp-point-after-word-p sp-in-string-p))
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode))
      (sp-local-pair mode "'" nil :actions nil)
      (sp-local-pair mode "`" nil :actions nil))
    (show-smartparens-global-mode +1)))

(use-package midnight
  :config (midnight-delay-set 'midnight-delay "4:30am"))

(use-package powerline
  :ensure powerline
  :init (powerline-default-theme))

(use-package color-theme-solarized
  :ensure color-theme-solarized
  :defer t)

(require 'ahyatt-google nil t)

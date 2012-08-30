;; emacs 24 seems to break if I don't do this
(require 'eieio)
;; This only gets set for real when cc mode is enabled
(setq c-buffer-is-cc-mode nil)
  (defun org-narrow-to-clocked-project ()
  (interactive)
  (save-excursion
    (org-clock-jump-to-current-clock)
    (switch-to-buffer (marker-buffer org-clock-marker))
    (org-up-heading-all 1)
    (org-narrow-to-subtree)
    (org-clock-jump-to-current-clock)))

(defun org-widen-up ()
  (interactive)
  (widen)
  (org-up-heading-all 2)
  (org-narrow-to-subtree))

(define-key global-map "\C-coj" 'org-narrow-to-clocked-project)
(define-key global-map "\C-cou" 'org-widen-up)
;; We don't really want to specify every single directory...
(let ((default-directory elisp-source-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; So that we can require encyrpted files (this will ask for a password).
(add-to-list 'load-suffixes ".el.gpg")

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(require 'fill-column-indicator)
(setq fci-style 'rule)
(add-hook 'c++-mode-hook (lambda ()
                           (setq fill-column 80)
                           (fci-mode 1)
                           (electric-pair-mode)
                           ;; compatible with fci-mode
                           (setq whitespace-style '(face trailing))))
(add-hook 'java-mode-hook (lambda ()
                            (setq fill-column 80)
                            (fci-mode 1)
                            (electric-pair-mode)))

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(defadvice yank (after c-indent-after-yank activate)
  "Do an indent after a yank"
  (if c-buffer-is-cc-mode
      (let ((transient-mark-mode nil))
        (indent-region (region-beginning) (region-end) nil))))

(defvar ash-clear-pairs '((?\( . ")")
                          (?\) . "(")
                          (?\[  . "]")
                          (?\] . "[")
                          (?< . ">")
                          (?> . "<")))

(defun ash-opener (char)
  "Return the opening equivalent of CHAR (a single char string),
as a string."
  (if (memq char '("(" "[" "<"))
      char (ash-pair-get-other char)))

(defun ash-pair-get-other (char)
  (or (cdr (assoc (string-to-char char) ash-clear-pairs)) char))

(defun ash-nearest-enclosing-pair-char ()
  (save-excursion
    (let ((orig-point (point))
          (min-elem (cons (buffer-size) ""))
          (paired-chars '("\"" "'" "(" ")" "[" "]" "{" "}" "," ";"
                          "<" ">" "/" "|")))
      (dolist (char paired-chars)
        (goto-char orig-point)
        (when (search-backward char nil t)
          (let ((begin (match-beginning 0)))
            (goto-char orig-point)
            (when (search-forward (ash-pair-get-other char) nil t)
              (let ((length (- (match-beginning 0) begin)))
                (when (< length (car min-elem))
                  (setq min-elem (cons length char))))))))
      (cdr min-elem))))

(defun ash-clear (&optional char)
  (interactive)
  (let* ((char (or char (ash-nearest-enclosing-pair-char)))
         (opener (ash-opener char))
         (closer (ash-pair-get-other opener))
         (begin (search-backward-regexp (format "%s\\|%s" (regexp-quote opener)
                                               (regexp-quote closer)))))
    (forward-char)
    (search-forward (ash-pair-get-other (match-string 0)))
    (kill-region (+ 1 begin) (- (point) 1))
    (goto-char (+ 1 begin))))

(setq semanticdb-default-save-directory "/tmp/semantic.cache")
  
;  (add-to-list 'load-path "/path/to/doc-mode")
;  (require 'doc-mode)
;  (add-hook 'c-mode-common-hook 'doc-mode)

(autoload 'company-mode "company" nil t)

(setq ediff-keep-variants nil)

;; The following was from a mail...

(add-hook 'ediff-keymap-setup-hook (lambda () (define-key 'ediff-mode-map "t" 'ediff-cycle-combination-pattern)))

(setq ediff-combination-patterns-available '())
(add-to-list 'ediff-combination-patterns-available
 ;; a, then b, then ancestor with markers
 '("<<<<<<< variant A" A ">>>>>>> variant B" B  "####### Ancestor" Ancestor "======= end") t)

(add-to-list 'ediff-combination-patterns-available
 ;; b, then a, then ancestor with markers
 '("<<<<<<< variant B" B ">>>>>>> variant A" A  "####### Ancestor" Ancestor "======= end") t)

(add-to-list 'ediff-combination-patterns-available
 ;; a, b, ancestor w/o markers
 '("" A "" B "" Ancestor "") t)

(add-to-list 'ediff-combination-patterns-available
 ;; b, a, ancestor w/o markers
 '("" B "" A "" Ancestor "") t)

;; add more possibliities to ediff-combination-patterns-available

;;; some elisp here to cycle thru patterns (probably ugly).
(defun ediff-cycle-combination-pattern ()
  "Change ediff-combination-pattern"
  (interactive)
  (setq ediff-combination-pattern
        (pop ediff-combination-patterns-available))
  (add-to-list 'ediff-combination-patterns-available ediff-combination-pattern t)
  (ediff-combine-diffs nil))

(add-to-list 'load-path "~/.emacs.d/src/html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/src/html5-el/schemas.xml"))

(require 'whattf-dt)

(add-to-list 'ido-ignore-files "flymake.cc")

(setq org-clock-string-limit 80
      org-log-done t
      org-agenda-include-diary t
      org-deadline-warning-days 1
      org-clock-idle-time 10
      org-agenda-start-with-log-mode nil)

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)"
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
         ((org-agenda-overriding-header "Last week's completed TODO: "))))
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
      org-archive-location "/home/ahyatt/org/notes.org::datetree/* Archived"
      org-use-property-inheritance t
      org-agenda-clockreport-parameter-plist
      '(:maxlevel 2 :link nil :scope ("/home/ahyatt/org/work.org"))
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

(global-set-key [M-f11] 'ash-agenda)
(global-set-key [print] 'ash-agenda)

(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline "/home/ahyatt/org/notes.org" "Unfiled notes")
         "* %a%?\n%u\n%i")
        ("j" "Journal" entry
         (file+datetree "/home/ahyatt/org/notes.org")
         "* %T %?")
        ("t" "Todo" entry
         (file+headline "/home/ahyatt/org/work.org" "Inbox")
         "* TODO %?\n%a")
        ("a" "Act on email" entry
         (file+headline "/home/ahyatt/org/work.org" "Inbox")
         "* TODO Process [%a]\n" :immediate-finish t)))
(setq org-default-notes-file "~/work/work.org")
(define-key global-map [f12] 'org-capture)

(add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

(defun ash-jabber-colorize-tags ()
  (when (featurep 'emacs-jabber)
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
        (progn (org-cycle)
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

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq enable-recursive-minibuffers t)
(setq redisplay-dont-pause t)  
(setq x-select-enable-clipboard t)
(savehist-mode 1)
(recentf-mode 1)
(tool-bar-mode -1)
(display-time-mode 1)
;; Recentf is useless without saving frequently
(run-with-idle-timer 1 nil 'recentf-save-list)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("java" (mode . java-mode))
               ("shell" (mode . shell-mode))
               ("eshell" (mode . eshell-mode))
               ("lisp" (mode . emacs-lisp-mode))
               ("erc" (mode . erc-mode))
               ("org" (mode . org-mode))
               ("git" (mode . git-status-mode))
               ("c++" (or
                       (mode . cc-mode)
                       (mode . c++-mode)))
               ("rcirc" (name . "@localhost$"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble"))))))
      ibuffer-sorting-mode 'recency)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))

(add-to-list 'Info-default-directory-list "~/.emacs.d/info/")

(define-key global-map "\C-x\C-j" 'dired-jump)
(setq nxml-slash-auto-complete-flag t)

(eval-after-load 'yasnippet
  '(progn
     (require 'dropdown-list)
     (setq yas/prompt-functions '(yas/dropdown-prompt
                                  yas/ido-prompt
                                  yas/completing-prompt))))

(eval-after-load "jabber"
  (progn
    ;; I don't like the jabber modeline having counts, it takes up too
    ;; much room.
    (defadvice jabber-mode-line-count-contacts (around ash-remove-jabber-counts
                                                       (&rest ignore))
      "Override for count contacts, to remove contacts from modeline"
      (setq ad-return-value ""))
    (ad-activate 'jabber-mode-line-count-contacts)
    (add-hook 'jabber-chat-mode-hook 'flyspell-mode)
    (when (featurep 'anything)
      (add-to-list 'anything-sources anything-c-source-jabber-contacts))
    (setq jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll)
          jabber-alert-muc-hooks '(jabber-muc-scroll)
          jabber-alert-presence-hooks (quote (jabber-presence-update-roster))
          jabber-autoaway-method (quote jabber-current-idle-time)
          jabber-mode-line-mode t
          jabber-vcard-avatars-retrieve nil)
    (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)))

(require 'smex)
;; This stopped being defined, so let's just define it ourselves
(defun smex-update-and-run ()
  (interactive)
  (smex-update)
  (smex))
(add-hook 'after-init-hook 'smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; edit server, a Chrome extension
(if (and (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (edit-server-start)))

(require 'ace-jump-mode)
(define-key global-map (quote [Scroll_Lock]) 'ace-jump-mode)
(define-key global-map (kbd "C-'") 'ace-jump-char-mode)

(setq ido-use-virtual-buffers t)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'dabbrev-expand)
(key-chord-define-global "l;" 'magit-status)
(key-chord-define-global "`1" 'yas/expand)
(key-chord-define-global "-=" (lambda () (interactive) (switch-to-buffer "*compilation*")))

(key-chord-define-global "xb" 'recentf-ido-find-file)
(key-chord-define-global "xg" 'smex)
(key-chord-define-global "XG" 'smex-major-mode-commands)
(key-chord-define-global "fj" 'ash-clear)

(autoload 'gnus "gnus-load" nil t)

(eval-after-load "gnus"
  ;; gnus-agent and nnimap don't always work well together,
  ;; but maybe things have gotten better.  Setting to 't again, if it
  ;; fails again let's record why.
  (setq gnus-agent t
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
        gnus-suppress-duplicates t))

(defun gnus-user-format-function-a (header) 
   (let ((myself (concat "<" user-mail-address ">"))
         (references (mail-header-references header))
         (message-id (mail-header-id header)))
     (if (or (and (stringp references)
                  (string-match myself references))
             (and (stringp message-id)
                  (string-match myself message-id)))
         "X" "│")))

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
(add-hook 'term-mode-hook 'ash-term-hooks)

(setq ido-enable-tramp-completion nil)

;; from http://www.method-combination.net/blog/archives/2011/03/11/speeding-up-emacs-saves.htlm
(setq vc-handled-backends nil)

(setq erc-modules '(autoaway autojoin completion fill irccontrols log match menu move-to-prompt noncommands notify readonly ring scrolltobottom smiley stamp track)
      erc-hide-list (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE"))
      erc-autoaway-mode t
      erc-notify-mode t
      erc-echo-notices-in-minibuffer-flag t
      erc-auto-query t  ;; nil = no new buffer
      erc-autoaway-idletimer 'emacs
      erc-user-full-name user-full-name
      erc-track-when-inactive 'nil
      ;; also "324" "329" "332" "333" "353" "477" ?
      erc-track-exclude-types '(("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353"))
      erc-track-exclude-server-buffer 'nil
      erc-autoaway-idle-seconds 300
      erc-track-showcount t
      erc-track-shorten-names nil)

(require 'erc-track)  ;; to load the default definitions

(defface erc-modeline
  '((((class color)) (:foreground "ping"))
    (t (:italic t) (:bold t)))
  "Face used for ERC modeline."
  :group 'erc)


(defun erc-track-find-face (faces)
  "Just return a reasonable face"
  'erc-modeline)

(setq rcirc-max-message-length 5000)
(eval-after-load "rcirc"
  '(progn (add-hook 'rcirc-mode-hook (lambda () 
                                       (flyspell-mode 1)
                                       (rcirc-track-minor-mode 1)
                                       (setq rcirc-fill-flag nil
                                             rcirc-fill-column 'frame-width
                                             rcirc-omit-mode t)))
          (defun rcirc-handler-MODE (process sender args text))))

(defun ash-switch-to-rcirc-buffer ()
  (interactive)
  (switch-to-buffer (ido-completing-read "Conversation: "
                                         (mapcar 'buffer-name
                                                 (remove-if-not (lambda (buf)
                                                                  (with-current-buffer buf
                                                                    (eq major-mode 'rcirc-mode)))
                                                                (buffer-list))))))

;; From http://www.emacswiki.org/emacs/rcircAutoAway
(defvar rcirc-auto-away-server-regexps nil
  "List of regexps to match servers for auto-away.")

(defvar rcirc-auto-away-after 3600
  "Auto-away after this many seconds.")

(defvar rcirc-auto-away-reason "idle"
  "Reason sent to server when auto-away.")

(defun rcirc-auto-away ()
  (message "rcirc-auto-away")
  (rcirc-auto-away-1 rcirc-auto-away-reason)
  (add-hook 'post-command-hook 'rcirc-auto-unaway))

(defun rcirc-auto-away-1 (reason)
  (let ((regexp (mapconcat (lambda (x) (concat "\\(" x "\\)")) 
                           rcirc-auto-away-server-regexps "\\|")))
    (dolist (process (rcirc-process-list))
      (when (string-match regexp (process-name process))
        (rcirc-send-string process (concat "AWAY :" reason))))))

(defun rcirc-auto-unaway ()
  (remove-hook 'post-command-hook 'rcirc-auto-unaway)
  (rcirc-auto-away-1 ""))

(run-with-idle-timer rcirc-auto-away-after t 'rcirc-auto-away)
;;(cancel-function-timers 'rcirc-auto-away)

(scroll-bar-mode -1)

; Save every time things are changed
(setq bookmark-save-flag 1)

(add-hook 'edit-server-text-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'edit-server-text-mode-hook (lambda () (flyspell-mode 1)))

;; (require 'anything)
;; (require 'anything-config)

;; (setq anything-sources
;;       (remove-duplicates (append anything-for-files-prefered-list
;;                                  '(anything-c-source-buffers+
;;                                    anything-c-source-imenu
;;                                    anything-c-source-info-emacs
;;                                    anything-c-source-org-keywords
;;                                    anything-c-source-info-org
;;                                    anything-c-source-info-cl
;;                                    anything-c-source-info-elisp))))

;; ;; This makes sense on kinesys keyboards
;; (key-chord-define-global "=1" 'anything)
;; ;; This makes sense on normal keyboards
;; (key-chord-define-global "`1" 'anything)

(autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
(autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
(autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
(autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
(autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
(autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
(autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
(autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)

(key-chord-define-global "9m" 'bc-set)
(key-chord-define-global "9p" 'bc-previous)
(key-chord-define-global "9n" 'bc-next)
(key-chord-define-global "9P" 'bc-local-previous)
(key-chord-define-global "9N" 'bc-local-next)
(key-chord-define-global "9l" 'bc-list)
(key-chord-define-global "9c" 'bc-clear)

;; (message "autocomplete")
;; (add-to-list 'load-path "~/.emacs.d/auto-complete")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
;; (ac-config-default)

;; (defun ielm-auto-complete ()
;;   "Enables `auto-complete' support in \\[ielm]."
;;   (setq ac-sources '(ac-source-functions
;;                      ac-source-variables
;;                      ac-source-features
;;                      ac-source-symbols
;;                      ac-source-words-in-same-mode-buffers))
;;   (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
;;   (auto-complete-mode 1))
;; (add-hook 'ielm-mode-hook 'ielm-auto-complete)
(add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))

(setq mode-line-modes nil)

(defun ash-set-frame-font-points (points)
  (interactive "nPoints: ")
  (set-frame-parameter (selected-frame) 'font (concat "Monaco-" (int-to-string points))))

;; what about custom-theme-load-path?
(add-to-list 'load-path "~/.emacs.d/src/emacs-color-theme-solarized/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/src/emacs-color-theme-solarized")
(defun ash-reapply-theme (frame)
  (save-excursion
    (dolist (theme custom-enabled-themes)
      (enable-theme theme))))
;; (add-hook 'after-make-frame-functions
;;           'ash-reapply-theme)
(setq color-theme-is-global nil)

;; (message "midnight")
;; (require 'midnight)
;; (setq clean-buffer-list-delay-general 2)
;; (add-to-list 'clean-buffer-list-kill-never-regexps ".*@localhost")

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

(define-abbrev-table
  'global-abbrev-table '(
                         (";G"  "Google" nil 0)
                         (";i18n" "internationalization" nil 0)
                         (";l10n" "localization" nil 0)))
(abbrev-mode 1)

(defun mirror-buffer ()
  (interactive)
  (cond ((= (length (window-list)) 2)
         (switch-to-buffer (window-buffer (next-window))))
        ((= (length (window-list)) 1)
         (split-window))
        (t (error "There must be 2 or less windows to mirror the current buffer"))))
(key-chord-define-global "0k" 'mirror-buffer)

(require 'expand-region)
(key-chord-define-global "o\\" 'er/expand-region)
(key-chord-define-global "p\\" 'er/contract-region)

(setq comint-input-ignoredups t)

(require 'multiple-cursors)
(global-set-key (kbd "C-c m m") 'mc/edit-lines)
(key-chord-define-global "zm" 'mc/edit-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)
(key-chord-define-global "za" 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(key-chord-define-global "ze" 'mc/edit-lines)
(global-set-key (kbd "C-c m r") 'mc/set-rectangular-region-anchor)
(key-chord-define-global "zr" 'mc/set-rectangular-region-anchor)
(global-set-key (kbd "C-c m =") 'mc/mark-all-like-this)
(key-chord-define-global "z=" 'mc/mark-all-like-this)
(key-chord-define-global "i\\" 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(key-chord-define-global "zn" 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(key-chord-define-global "zp" 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m x") 'mc/mark-more-like-this-extended)
(key-chord-define-global "zx" 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c m u") 'mc/mark-all-in-region)
(key-chord-define-global "zu" 'mc/mark-all-in-region)

(require 'google-contacts)
(require 'google-contacts-gnus)
(require 'google-contacts-message)
(require 'ahyatt-google)

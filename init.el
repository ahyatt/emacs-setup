(require 'cl)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("technomancy" . "http://repo.technomancy.us/emacs/")))

;; Package setup, taken from
;; https://github.com/zane/dotemacs/blob/master/zane-packages.el#L62
(setq ash-packages
      '(ace-jump-mode
        anaphora
        autopair
        bang
        color-theme-solarized
        cppcheck
        dart-mode
        dynamic-fonts
        expand-region
        fill-column-indicator
        flex-autopair
        flymake-cursor
        flyspell-lazy
        font-utils
        idle-highlight-mode
        ido-ubiquitous
        jabber
        js2-mode
        key-chord
        list-utils
        magit
        oauth2
        paredit
        rainbow-delimiters
        rainbow-mode
        smex
        starter-kit
        undo-tree
        yasnippet
        ))

(require 'package)
(package-initialize)
;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p ash-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))

;; This only gets set for real when cc mode is enabled
(setq c-buffer-is-cc-mode nil)

;; We don't really want to specify every single directory...
(let ((default-directory "~/.emacs.d"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; So that we can require encyrpted files (this will ask for a password).
(add-to-list 'load-suffixes ".el.gpg")

;; This stops matching regardless of case, which is better for
;; camel-cased words.
(setq dabbrev-case-fold-search nil)

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

(eval-after-load 'ido
  '(add-to-list 'ido-ignore-files "flymake.cc"))

(eval-after-load 'org
  '(progn  
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

     (setq org-default-notes-file "~/work/work.org")
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
     ;; always evaluate everything.  Livin' on the edge!
     (setq org-confirm-babel-evaluate (lambda (lang body) nil))
     (setq org-babel-sh-command "zsh")
     
     (global-set-key (kbd "C-c a") 'org-agenda)
     (global-set-key (kbd "C-c c") 'org-capture)))

;; this function causes an annoying prompt for LAST_READ_MAIL.  Kill
;; the whole function for now.
(eval-after-load 'org-contacts
  '(defun org-contacts-gnus-store-last-mail ()))

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
  '(progn
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
     (defun jabber-display-roster ())
     ))

;; edit server, a Chrome extension
(if (and (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (edit-server-start)
      (add-hook 'edit-server-text-mode-hook (lambda () (visual-line-mode 1)))
      (add-hook 'edit-server-text-mode-hook (lambda () (flyspell-mode 1)))))

(eval-after-load 'ace-jump-mode
  '(progn (define-key global-map (quote [Scroll_Lock]) 'ace-jump-mode)
	  (define-key global-map (kbd "C-'") 'ace-jump-char-mode)
          (define-key global-map (kbd "C-\"") 'ace-jump-line-mode)))

(eval-after-load 'key-chord
  '(progn
     (key-chord-mode 1)
     (key-chord-define-global "jk" 'dabbrev-expand)
     (key-chord-define-global "l;" 'magit-status)
     (key-chord-define-global "`1" 'yas/expand)
     (key-chord-define-global "-=" (lambda () (interactive) (switch-to-buffer "*compilation*")))

     (key-chord-define-global "xb" 'recentf-ido-find-file)
     (key-chord-define-global "xg" 'smex)
     (key-chord-define-global "XG" 'smex-major-mode-commands)
     (key-chord-define-global "fj" 'ash-clear)))

(eval-after-load 'smex
  ;; Workaround for https://github.com/nonsequitur/smex/issues/21
  ;; Also see https://github.com/technomancy/ido-ubiquitous/issues/17
  '(defun smex-completing-read (choices initial-input)
     (let ((ido-completion-map ido-completion-map)
           (ido-setup-hook (cons 'smex-prepare-ido-bindings ido-setup-hook))
           (ido-enable-prefix nil)
           (ido-enable-flex-matching smex-flex-matching)
           (ido-max-prospects 10))
    (ido-completing-read (smex-prompt-with-prefix-arg) choices nil t initial-input nil (car choices)))))

(autoload 'gnus "gnus-load" nil t)

(eval-after-load "gnus"
  ;; gnus-agent and nnimap don't always work well together,
  ;; but maybe things have gotten better.  Setting to 't again, if it
  ;; fails again let's record why.
  '(setq gnus-agent t
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

(setq rcirc-max-message-length 5000)
(eval-after-load "rcirc"
  '(progn (add-hook 'rcirc-mode-hook (lambda () 
                                       (flyspell-mode 1)
                                       (rcirc-track-minor-mode 1)
                                       (setq rcirc-fill-flag nil
                                             rcirc-fill-column 'frame-width
                                             rcirc-omit-mode t)))
          (defun rcirc-handler-MODE (process sender args text))
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
	  (run-with-idle-timer rcirc-auto-away-after t 'rcirc-auto-away)))

; Save every time things are changed
(setq bookmark-save-flag 1)

(eval-after-load 'bc
  '(progn 
     (autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
     (autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
     (autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
     (autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
     (autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
     (autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
     (autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
     (autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)
     (eval-after-load 'key-chord
       (key-chord-define-global "9m" 'bc-set)
       (key-chord-define-global "9p" 'bc-previous)
       (key-chord-define-global "9n" 'bc-next)
       (key-chord-define-global "9P" 'bc-local-previous)
       (key-chord-define-global "9N" 'bc-local-next)
       (key-chord-define-global "9l" 'bc-list)
       (key-chord-define-global "9c" 'bc-clear))))

(add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))
(setq mode-line-modes nil)

(defun ash-set-frame-font-points (points)
  (interactive "nPoints: ")
  (set-frame-parameter (selected-frame) 'font (concat "Monaco-" (int-to-string points))))

(defun ash-reapply-theme (frame)
  (save-excursion
    (dolist (theme custom-enabled-themes)
      (enable-theme theme))))

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

(eval-after-load 'key-chord
  '(key-chord-define-global "0k" 'mirror-buffer))

(require 'expand-region)
(eval-after-load 'key-chord
  '(progn
     (key-chord-define-global "o\\" 'er/expand-region)
     (key-chord-define-global "p\\" 'er/contract-region)))

(setq comint-input-ignoredups t)

(eval-after-load 'multiple-cursors
  '(progn 
     (global-set-key (kbd "C-c m m") 'mc/edit-lines)
     (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)
     (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
     (global-set-key (kbd "C-c m r") 'mc/set-rectangular-region-anchor)
     (global-set-key (kbd "C-c m =") 'mc/mark-all-like-this)
     (global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
     (global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
     (global-set-key (kbd "C-c m x") 'mc/mark-more-like-this-extended)
     (global-set-key (kbd "C-c m u") 'mc/mark-all-in-region)
     (eval-after-load 'key-chord
       '(progn
	  (key-chord-define-global "zm" 'mc/edit-lines)
	  (key-chord-define-global "za" 'mc/edit-lines)
	  (key-chord-define-global "ze" 'mc/edit-lines)
	  (key-chord-define-global "zr" 'set-rectangular-region-anchor)
	  (key-chord-define-global "z=" 'mc/mark-all-like-this)
	  (key-chord-define-global "i\\" 'mc/mark-all-like-this)
	  (key-chord-define-global "zn" 'mc/mark-next-like-this)
	  (key-chord-define-global "zp" 'mc/mark-previous-like-this)
	  (key-chord-define-global "zx" 'mc/mark-more-like-this-extended)
	  (key-chord-define-global "zu" 'mc/mark-all-in-region)))))

(eval-after-load 'flyspell-lazy
  '(flyspell-lazy-mode 1))

(eval-after-load 'undo-tree '(progn
			       (setq undo-tree-auto-save-history nil
				     undo-tree-visualizer-timestamps t)
			       (global-undo-tree-mode 1)))

(defun ash-shorten-minor-mode (minor-mode)
  (list (car minor-mode)
        (let ((mode (car minor-mode)))
          (cond ((eq mode 'eldoc-mode) "文档·")
                ((eq mode 'yas-minor-mode) "模板·")
                ((eq mode 'paredit-mode) "插入语·")
                ((eq mode 'undo-tree-mode) "复原·")
                ((eq mode 'googlemenu-mode) "")
                (t (second minor-mode))))))

;; from http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html,
;; with modifications.
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   mode-line-buffer-identification
   ; narrow [default -- keep?]
   " %n "
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line (mapcar #'ash-shorten-minor-mode minor-mode-alist))
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "gray20" :family "SourceSansPro-Bold"
    :inverse-video nil
    :box '(:line-width 6 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :inverse-video nil
    :box '(:line-width 6 :color "gray40" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Monaco" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

(require 'org)
(require 'multiple-cursors nil t)
;; This isn't working correctly right now, much investigate why.
;; (require 'google-contacts nil t)
;; (require 'google-contacts-gnus nil t)
;; (require 'google-contacts-message nil t)
(require 'ahyatt-google nil t)
(require 'ace-jump-mode nil t)
(require 'yasnippet nil t)
(require 'undo-tree nil t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(column-number-mode t)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "5debeb813b180bd1c3756306cd8c83ac60fda55f85fb27249a0f2d55817e3cab" "117284df029007a8012cae1f01c3156d54a0de4b9f2f381feab47809b8a1caef" "0174d99a8f1fdc506fa54403317072982656f127" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "c3fbf1e1469afba60543a39792be147c1cc33189" "b03af7ef60f7163c67e0984d0a54082d926f74ac" "937a688137bf1e6e4df5c8805ba7caad8d411d5d" default)))
 '(google-flymake-run-only-after-saving t)
 '(ido-everywhere t)
 '(ido-use-url-at-point t)
 '(jabber-avatar-cache-directory "/usr/local/google/.jabber-avatars/")
 '(make-backup-files nil)
 '(offlineimap-command "offlineimap -u ttyui -1")
 '(org-agenda-files (quote ("/home/ahyatt/org/work.org")))
 '(org-agenda-span (quote day))
 '(org-agenda-sticky t)
 '(yas-also-auto-indent-first-line t)
 '(yas-global-mode t nil (yasnippet))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")) nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar ash-vc 'fig "Which VC to use, 'g4, 'git5, 'fig")
(defvar ash-devel-host nil "Which hostname to use, or nil for localhost")

(require 'google)

(use-package notmuch
  :general
  (:prefix "C-c n"
           "" '(nil :which-key "Notmuch")
           "n" 'notmuch-hello
           "s" 'notmuch-search)
  (:keymaps 'notmuch-search-mode-map
            "A" (lambda () (interactive) (notmuch-search-tag-all '("-inbox" "-folder:INBOX" "-unread"))))
  :config
  (require 'org-notmuch)
  (setq-default notmuch-archive-tags '("-inbox" "-folder:INBOX" "-unread")
                notmuch-address-internal-completion '(sent "date:1y..now and not @gmail.com and not @docs.google.com and not from:buganizer-system and not hume and not noreply")
                notmuch-fcc-dirs nil)
  (add-hook 'notmuch-message-mode-hook (lambda () (variable-pitch-mode 1)))
  (add-hook 'messages-buffer-mode-hook
            (lambda () (variable-pitch-mode 1)))
  (add-hook 'notmuch-show-mode-hook (lambda () (variable-pitch-mode 1)))
  (defun ash-compose-summary-mail-notmuch ()
    (interactive)
    (notmuch-mua-mail "Local Search Quality <local-search-quality@google.com>, Geodoc Team <geodoc-team@google.com>, Geo Serving Integration Team <geo-serving-integration-team@google.com>, Geo Notes <geo-notes@google.com>, Localweb Indexing  <localweb-indexing-eng@google.com>, kczuba@google.com, nbenayoun@google.com" (concat "Local Search Infrastructure Status Update " (format-time-string "%m/%d/%Y"))))

  (defun ash-compose-things-at-places-summary-mail-notmuch ()
    (interactive)
    (notmuch-mua-mail "geo-offerings@google.com, Geo Notes <geo-notes@google.com>" (concat "Things at Places Status Update " (format-time-string "%Y-%m-%d"))))

  (setq
   user-full-name  "Andrew Hyatt"
   ;; include in message with C-c C-w
   message-signature nil))

;; I'm not so sure this works...
;; (defun ash-org-capture-replace ()
;;   (goto-char 1)
;;   (let ((search-invisible t))
;;     (replace-string "INBOX" "Archive")))
;; (add-hook 'org-capture-prepare-finalize-hook 'ash-org-capture-replace)
(define-key global-map [f12] 'org-capture)
(setq org-timer-default-timer 30)
(defun ash/org-link-description (link)
  "Makes a useful description from a link."
  (cond ((string-match "^file:" link)
	 (file-name-nondirectory link))
	(t nil)))
(defun ash/org-paste-link ()
  "Paste all stored links without prompting."
  (interactive)
  (unwind-protect
      (cl-flet ((read-string (prompt &optional initial-input history default-value
				     inherit-input-method)
			     initial-input))
	(dolist (link (delete-duplicates org-stored-links :test 'equal))
	  (org-insert-link nil (car link) (ash/org-link-description (car link)))))
    (setq org-stored-links nil)))
(add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "C-c M-p") 'ash/org-paste-link)))
;; TODO(ahyatt) Move to main config.
;; Save every time things are changed
(setq bookmark-save-flag 1)
;; from http://www.method-combination.net/blog/archives/2011/03/11/speeding-up-emacs-saves.htlm
(setq vc-handled-backends nil)

;; Google specific
;; So that we can require encyrpted files (this will ask for a password).
(add-to-list 'load-suffixes ".el.gpg")

(setq google-update-load-path (lambda (path)
				(cl-notany (lambda (regex) (string-match-p regex path))
					   '("/third_party/elisp/+git_modes"
					     "/third_party/elisp/+magit"))))

(setq eww-search-prefix "https://google.com/search?q=")

(require 'compilation-colorization) ;; colorizes output of (i)grep
(require 'rotate-among-files)       ;; google-rotate-among-files
(require 'google3) ;; magically set paths for compiling google3 code
(require 'google-imports)
(require 'google3-build-fn)
(require 'google-paths)
(require 'google-cc-extras)
(require 'google-coding-style)
(require 'google3-build)

(general-define-key
 :prefix "C-c g"
 "" '(nil :which-key "google")
 "b" 'google3-build
 "t" 'google3-test
 "g" 'ash-grab-filename)

;; Has to come after we load google libraries since we need to use url-sso
;; TODO: This isn't working, it seems to be ignoring this entirely.
;; (add-to-list 'package-archives '("GELPA" . "http://gelpa-182518.googleplex.com/packages/") t)
;; (let ((package-check-signature nil))
;;   (use-package git-gutter :demand :diminish "" :ensure t)
;;   (use-package ob-dremel :demand :ensure t))

(defun ash-notmuch-mark-all-read ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (notmuch-search-remove-tag '("-inbox" "-unread"))))

(global-set-key [f5] 'fig-status)
(global-set-key [C-f5] 'gtags-show-tag-locations-under-point)
(global-set-key [f6] 'google-next-tag)
(global-set-key [f7] 'clang-include-fixer-at-point)
(global-set-key [f8] 'google-imports-grab-import)
(global-set-key [f9] 'google-imports-add-grabbed-imports)
(global-set-key [f10] 'google-rotate-among-files)
(defconst ash-invalid-roots-regexs '("_blaze" "READONLY"))

(defun ash-open-google3-roots ()
  (remove-if (lambda (path) (member* path ash-invalid-roots-regexs
				     :test (lambda (a b) (string-match b a))))
	     (delete-dups (loop for buffer in (buffer-list)
				when (and (buffer-file-name buffer)
					  (is-google3-filename (buffer-file-name buffer)))
				collect (car (google3-split-depot-path (buffer-file-name buffer)))))))

(defun ash-dir-for-buffer (buffer)
  (save-excursion (set-buffer buffer) default-directory))

;; Remove when this is in some ELPA
(load-file "~/src/org-projprop/org-projprop.el")
(defun org-projprop-list-google-citc-client-dirs ()
  (mapcar (lambda (c)
	    (cons (car c)
		  (concat (directory-file-name (cdr c)) "/google3")))
	  (org-projprop-list-directory (concat "/google/src/cloud/"
					       (getenv "USER")))))
(setq org-projprop-list-funcs
      '((google-citc-client-dirs)
	(directory "~/src/")))
(org-projprop-setup)

(defun ash-buffers-in-client (&optional client-path)
  (loop with current-client = (or client-path
				  (when (is-google3-filename (ash-dir-for-buffer (current-buffer)))
				    (car (google3-split-depot-path (ash-dir-for-buffer (current-buffer))))))
	for buffer in (buffer-list)
	when (let ((dir (ash-dir-for-buffer buffer)))
	       (and current-client
		    dir
		    (is-google3-filename dir)
		    (string-match (concat "^" current-client) dir)))
	collect buffer))

(defun ash/client-files-not-in-buffers (files)
  "FILES must be relative to google3."
  (require 'dash)
  (let ((project-buffer-files
	 (-filter 'identity
		  (mapcar (lambda (f) (when (is-google3-filename f) (google3-relative-path f)))
			  (-reject 'null
				   (mapcar 'buffer-file-name (ash-buffers-in-client)))))))
    (-difference files project-buffer-files)))

(defun ash-switch-buffer-in-client ()
  (interactive)
  (switch-to-buffer (funcall (if (featurep 'ido) 'ido-completing-read 'completing-read)
			     "Buffer: "
			     (mapcar 'buffer-name (ash-buffers-in-client)))))

(require 'rotate-clients)
(defadvice google-rotate-client (before ash-get-clients-before-rotate activate)
  (setq google-rotate-clients-client-list (ash-clients)))

(defvar ash/get-files-from-cl-cache (make-hash-table :test 'equal)
  "A hash of google3 dir + vc type cons to results")

(defun ash/get-files-from-cl (&optional vc)
  (let ((vc (or vc (if (file-exists-p ".git") 'git 'g4))))
    (google-ascend-to-google3-dir
     (or (gethash (cons default-directory vc) ash/get-files-from-cl-cache)
         (let ((result (cond ((eq 'git vc)
                              (mapcan (lambda (line)
                                        (when (string-match "^\\(google3/\\)" line)
                                          (list (replace-match "" nil nil line))))
                                      (split-string
                                       (shell-command-to-string "git5 diff --name-only"))))
                             ((eq 'g4 vc)
                              (mapcar (lambda (line)
                                        (replace-regexp-in-string
                                         "-unopened\\(edit\\|add\\)" ""
                                         (replace-regexp-in-string
                                          "#.*$" ""
                                          (replace-regexp-in-string
                                           "[[:space:]]+" "" line))))
                                      (remove-if-not
                                       (lambda (line) (string-match "^  " line))
                                       (split-string
                                        (shell-command-to-string
                                         "g4 pending -s relativepath") "\n")))))))
           (puthash (cons default-directory vc) result ash/get-files-from-cl-cache)
           result)))))

(defun ash-find-file-in-client ()
  (interactive)
  (require 'google-paths)
  (google-ascend-to-google3-dir
   (let ((vc (if (file-exists-p ".git") 'git 'g4)))
     (find-file (ido-completing-read
		 "Which file from current CL? "
		 (ash/get-files-from-cl))))))

(defun ash-switch-to-genfiles ()
  (interactive)
  (find-file (replace-regexp-in-string "google3/"
                                       "google3/blaze-genfiles/"
                                       (file-name-directory (buffer-file-name)))))

(defun ash-open-file-in-build ()
  "Open current file in /home/build/google3."
  (interactive)
  (find-file
   (concat "/home/build/google3/" (google-imports-make-relative (or buffer-file-name default-directory)))))

(defvar ash-client-root-parents '("/google/src/cloud/$USER/")
  "A list of client root parents, the children of which are different client rootdirs")

;; Should be obsolete
(defun ash-clients ()
  "Get all citc client names."
  (append '("/home/build")
	  (remove-duplicates
	   (remove-if (lambda (filename) (string-match "review$" (file-name-nondirectory filename)))
		      (remove-if-not 'file-directory-p
				     (mapcan (lambda (parent-dir)
					       (directory-files parent-dir t "^[^\\.]"))
					     (mapcar 'substitute-in-file-name ash-client-root-parents)))))
	  (directory-files (ash-devel-path "~/src/") t "^[^.]")))

(defun ash-get-client ()
  "Get the current client name."
  (file-name-nondirectory (or (google-paths-client-root) "")))

(defvar ash-client-mode-old-buffer-name nil
  "Holds the other value of `mode-line-buffer-identification' for `ash-client-mode'.")

(define-minor-mode ash-client-mode
  "Shows the client root name in the modeline."
  :global t
  :group 'google
  (if ash-client-mode
      (progn
	(unless ash-client-mode-old-buffer-name
	  (setq ash-client-mode-old-buffer-name
		(car mode-line-buffer-identification)))
	(setcar mode-line-buffer-identification
		'((:eval (ash-get-client)) (":%12b"))))
    (setcar mode-line-buffer-identification
	    ash-client-mode-old-buffer-name)))

(defun ash-go-client (client)
  "Open up a shell for citc CLIENT."
  (interactive (list nil))
  (let* ((p (point))
	 (clients (ash-clients))
	 (client (or client
		     (ido-completing-read
		      "Client: "
		      (mapcar 'file-name-nondirectory clients) nil t
		      (ash-get-client))))
	 (eshell-buf-name (format "*%s-eshell*" client))
	 (shell-buf-name (format "*%s-shell*" client))
	 (term-buf-inner-name (format "%s-term" client))
	 (term-buf-name (format "*%s*" term-buf-inner-name))
	 (client-path (concat (find-if (lambda (path)
					 (string-match (format "%s$" client) path))
				       clients) "/google3/"))
	 (org-mode-buf (when (eq major-mode 'org-mode)
			 (current-buffer)))
	 (default-directory client-path)
	 (persp-name client))
    (if (not (eq (persp-get-by-name persp-name) :nil))
	(persp-switch persp-name)
      (persp-add-new persp-name)
      (persp-switch persp-name)
      (unless (get-buffer eshell-buf-name)
	(eshell 1)
	(rename-buffer eshell-buf-name)
	(persp-add-buffer eshell-buf-name))
      (switch-to-buffer eshell-buf-name)
      (delete-other-windows)
      (when org-mode-buf
	(split-window-right)
	(let* ((bufname (concat "*" client "-org*"))
	       (buf (or (get-buffer bufname) (make-indirect-buffer org-mode-buf bufname t))))
	  (with-current-buffer buf (goto-char p) (org-narrow-to-subtree))
	  (persp-add-buffer buf)
	  (switch-to-buffer buf))))))

(defun ash-org-start-work ()
  (interactive)
  (if (org-entry-get (point) "Project" t)
      (ash-go-client nil)
    (ash-work-on-item)))

(defun ash-org-item-to-name ()
  "Get a dashed string representation of the org item"
  (downcase
   (replace-regexp-in-string "\s+" "-"
			     (replace-regexp-in-string "^[A-Z]+ " ""
						       (substring-no-properties (org-get-heading))))))

(defun ash-work-on-item (&optional name)
  "From an org item open a perpsective for that item."
  (interactive)
  (let* ((name (or name (ash-org-item-to-name)))
         (bufname (format "*%s-org*" name))
         (buf (or (get-buffer name) (make-indirect-buffer (current-buffer) name t))))
    (with-current-buffer buf
      (org-narrow-to-subtree))
    (if (persp-get-by-name name)
        (persp-switch name)
      (persp-add-new name)
      (persp-switch name))
    (persp-add-buffer buf)
    (switch-to-buffer buf)))


(defun ash-devel-path (filename)
  (if ash-devel-host
      (concat "/" ash-devel-host ":" filename)
    filename))

(defun ash-new-client (client)
  "New citc CLIENT."
  (interactive "MNew client name: ")
  (cond ((eq ash-vc 'g4)
	 (shell-command (format "g4 citc %s" client)))
	((eq ash-vc 'git5)
	 (mkdir (ash-devel-path (concat "~/src/" client))))
	((eq ash-vc 'fig)
	 (shell-command (format "hg citc %s" client))))
  (ash-go-client client))

(defun ash-delete-client (client)
  "Delete citc CLIENT."
  (interactive (list nil))
  (let ((client (or client
		    (ido-completing-read
		     "Client: "
		     (mapcar 'file-name-nondirectory (ash-clients)) nil t
		     (ash-get-client)))))
    (persp-remove-by-name client)
    (shell-command (format "g4 citc -d %s" client))
    (let ((buf (get-buffer (format "*%s-eshell*" client))))
      (when buf (kill-buffer)))))

(define-key global-map "\C-cxn" 'ash-new-client)
(define-key global-map "\C-cxd" 'ash-delete-client)
(define-key global-map "\C-cxc" 'org-projprop-open)
(define-key global-map "\C-cxb" 'ash-switch-buffer-in-client)
(define-key global-map "\C-cxf" 'ash-find-file-in-client)

(defun ash-copy-url (url &optional new-window)
  (x-select-text url))

(setq browse-url-browser-function 'ash-copy-url
      browse-url-generic-program "google-chrome"
      android-mode-sdk-dir "org/android-sdk-linux_86"
      starttls-program "/usr/bin/starttls"
      starttls-extra-args nil)

(defun ash-kill-params (url &rest params)
  "Kill uninteresting params in a url."
  (let* ((u (url-generic-parse-url
	     (org-trim (substring-no-properties url))))
	 (path-parts (split-string (url-filename u) "?")))
    (if (> (length path-parts) 1)
	(url-normalize-url
	 (format "%s://%s:%d%s?%s" (url-type u) (url-host u) (url-port u)
		 (first path-parts)
		 (mapconcat (lambda (param-cons)
			      (format "%s=%s" (car param-cons)
				      (car (cdr param-cons))))
			    (remove-if
			     (lambda (param-cons)
			       (member (car param-cons) params))
			     (url-parse-query-string
			      (second path-parts)))
			    "&")))
      url)))

(defvar ash-google-search-params-to-keep '("q" "tbs" "tbm" "e" "expflags" "opts" "deb" "expid" "authuser"
					   "ludocid" "near" "hl" "stick" "host" "useragent" "uuld")
  "Search params that are worth keeping around")

(defun ash-shorten-search (url)
  "Shorten URL, removing all the unnecessary params."
  (require 'url)
  (let ((u (url-generic-parse-url url)))
    (url-normalize-url
     (format "%s://%s%s/search?%s" (url-type u) (url-host u)
	     (if (string-match "borg" (url-host u))
		 ""
	       (format ":%d" (url-port u)))
	     (mapconcat (lambda (param-cons)
			  (format "%s=%s" (car param-cons)
				  (replace-regexp-in-string
				   " " "+" (car (cdr param-cons)))))
			(reverse
			 (remove-if-not
			  (lambda (param-cons)
			    (member (car param-cons)
				    ash-google-search-params-to-keep))
			  (url-parse-query-string
			   (or (url-target u)
			       (second (split-string (url-filename u) "?"))))))
			"&")))))

(defvar google-yank-shorteners
  '(("https?://mondrian.corp.google.com/changelist/" .
     (lambda (s)
       (replace-regexp-in-string "http://cl/" "http://cl/" s)))
    ("https?://cr.corp.google.com/.*" .
     (lambda (s)
       (format "http://cl/%s" (progn (string-match "[[:digit:]]\\{7,\\}" s)
				     (match-string-no-properties 0 s)))))
    ("https?://mondrian.corp.google.com" .
     (lambda (s)
       (replace-regexp-in-string "https://mondrian.corp.google.com" "http://cl" s)))
    ("https://wiki.corp.google.com/twiki/bin/view" .
     (lambda (s)
       (replace-regexp-in-string "https://wiki.corp.google.com/twiki/bin/view" "http://wiki" s)))
    ("https://b.corp.google.com/issues?" .
     (lambda (s)
       (replace-regexp-in-string "https://b.corp.google.com/?\\(issues/\\)?"
				 "http://b/"
				 (ash-kill-params s "query" "cookieId" "clientLatencyRequestId"))))
    ("http://buganizer/issue?" .
     (lambda (s) (replace-regexp-in-string
		  "buganizer"
		  "b"
		  (ash-kill-params s "query" "cookieId" "clientLatencyRequestId" "gsessionid"))))
    ("https://critique.corp.google.com/#review/[[:digit:]]\\{7,\\}" .
     (lambda (s)
       (format "http://cl/%s" (progn (string-match "[[:digit:]]\\{7,\\}" s)
				     (match-string-no-properties 0 s)))))
    ("\\`http.*/#sclient=" . ash-shorten-search)
    ("\\`http.*/search\\?" . ash-shorten-search)
    ("\\`https?:.*/webhp\\?" . ash-shorten-search)
    ("" . identity))
  "A list of regex to function conses.

If the regex matches, the function is applied to the url to
shorten it.  Only the first one to match takes effect.")

(defadvice yank (before shortening-yank last (&optional arg) activate)
  "Shorten before yank."
  (let ((kill (current-kill (or arg 0))))
    (kill-new
     (let ((new-kill nil))
       (dolist (shortener-cons google-yank-shorteners new-kill)
         (when (and (not new-kill)
                    (string-match (car shortener-cons) kill))
           (setq new-kill (funcall (cdr shortener-cons) kill))))))))

;; (eval-after-load "org"
;;   '(add-to-list 'org-property-allowed-value-functions 'ash-org-projprop-property))

(defun ash-org-get-project ()
  "Get the org-defined project name."
  (org-entry-get (point) "Project" t))

(defadvice ash-go-client (before ash-org-go-client activate)
  "Making ash-go-client be autoselected in org.
Also auto-selects a perpsective."
  (let ((project (ash-org-get-project)))
    (when (and (eq major-mode 'org-mode)
	       project)
      (ad-set-arg 0 project))))

(defun unobfuscate-at-point ()
  "Unobfuscate a GAIA id at point."
  (interactive)
  (let ((unobfuscated
	 (second (split-string
		  (shell-command-to-string (concat "/home/ahyatt/bin/unobfuscator " (thing-at-point 'word)))))))
    (beginning-of-thing 'word)
    (kill-word nil)
    (insert unobfuscated)))

(require 'clang-fixit)
(add-hook 'next-error-hook 'clang-fixit-offer-fixes)
;; (define-key c++-mode-map [C-c o f] 'clang-fixit-apply-fixit)

(defun ash-try-squery (squery)
  (interactive "Msquery: ")
  (browse-url (concat "http://google.com/search?qf=s&noj=1&q="
		      (url-hexify-string squery))))

(defun ash-org-query-link (query)
  (interactive "Murl: ")
  (insert (format "[[%s][%s]]" query
		  (progn (string-match "[?&]q=\\([^\\'&]+\\)" query)
			 (url-unhex-string (match-string 1 query))))))

(defun ash-goto-searchz (&optional dc)
  "Go to some SR searchz page"
  (interactive)
  (browse-url (concat "http://go/srjump/" (or dc "vj") "/searchz")))

(setq user-mail-address "ahyatt@google.com")

(defun ash-grab-filename ()
  "Grab the google3-relative filename at buffer."
  (interactive)
  (let ((filename (google-imports-make-relative (or buffer-file-name default-directory))))
    (save-excursion
      (with-temp-buffer
        (insert filename)
        (kill-ring-save (point-min) (point-max))))))


;; Get rid of that annoying output filter calculation...
(defun google3-build-make-options ()
  )

(require 'google-lsp)
(google-lsp-init)
;; Optional: Set keybinding for documentation with Kythe.
(global-set-key (kbd "C-c h") #'google-lsp-describe-thing-at-point)

(setq g4-gutter:update-threshold 5)

(defun ash/bookmark-files ()
  (mapcar (lambda (item)
	    (let ((file (bookmark-get-filename item)))
	      (if (is-google3-filename file)
		  (replace-regexp-in-string "\\`\\(.*\\)google3"
					    (concat (google-paths-client-root) "/google3/")
					    file)
		file)))
	  bookmark-alist))

(defvar ash/google-client-force nil
  "True when we want to force something to be opened in the
  current client instead of asking.")

(defadvice bookmark-default-handler (before ash-google3-relativize
					    activate)
  (when (is-google3-filename (bookmark-get-filename (ad-get-arg 0)))
    (let* ((bmk-record (copy-list (ad-get-arg 0)))
	   (file (bookmark-get-filename bmk-record))
	   (clients (ash-clients))
	   (client (if ash/google-client-force
		       (ash-get-client)
		     (ido-completing-read
		      "Which client to open in? "
		      (mapcar 'file-name-nondirectory clients) nil t
		      (ash-get-client)))))
      (bookmark-set-filename
       bmk-record
       (replace-regexp-in-string "\\`\\(.*\\)google3"
				 (concat (find-if (lambda (path)
						    (string-match (format "%s$" client) path))
						  clients) "/")
				 file nil nil 1))
      (ad-set-arg 0 bmk-record))))

(defun ash/expanded-bookmark-files ()
  (require 'bookmark)
  (unless bookmark-alist
    (bookmark-load bookmark-default-file))
  (mapcan (lambda (file-or-dir)
	    (if (file-directory-p file-or-dir)
		(directory-files file-or-dir t)
	      (list file-or-dir))) (ash/bookmark-files)))


;; For ob-dremel
(eval-after-load 'org
  '(add-to-list 'org-structure-template-alist
		'("d" "#+BEGIN_SRC dremel\n?\n#+END_SRC")))
(eval-after-load 'org
  '(progn
     ;; otherwise babel breaks on dremel...
     (require 'ob-lob)
     ;; (require 'ob-dremel)
     (setq org-confirm-babel-evaluate nil)))

(defun eshell/sr-run (&rest args)
  "Run a superroot in eshell, with ARGS as the command args."
  (let ((command "superroot/tools/run_sr_www_locally.py")
	(script-args '("--binary=blaze-bin/superroot/servers/sr_www"))
	(sr-args '("--rpclog=-1" "--test_only_skip_common_badurls=true")))
    (when (integerp (car args))
      (let ((port (car args)))
	(setq script-args (append script-args
				  (list (concat "--port=" port)
					(concat "--statusz_port=" (+ 1 port)))))))
    (let ((cmd (mapconcat #'identity (append (list command) script-args '("--") sr-args (cdr args)) " ")))
      cmd)))
(defun resolve-bns (bns)
  (let ((result
	 (split-string (shell-command-to-string (concat "lockserv resolve " bns)) " ")))
    (concat (nth 1 result) ":" (nth 2 result))))
;; sending mail --
(require 'google-sendgmr)
(setq send-mail-function #'google-sendgmr-send-it)
(setq message-send-mail-function #'google-sendgmr-send-it)

;; just use my own yasnippets
(setq yas-snippet-dirs '("~/.spacemacs.d/snippets/"))

(defconst ash-citc-root "/google/src/cloud/ahyatt/")

(eval-after-load 'ivy
  '(progn
     (setq ivy-use-virtual-buffers t)
     (global-set-key (kbd "C-c v") 'ivy-push-view)
     (global-set-key (kbd "C-c V")  'ivy-pop-view)
     ;; No regex by default
     (setq ivy-initial-inputs-alist nil)))

(pixel-scroll-mode)

(add-to-list 'load-path "/google/data/ro/projects/cymbal/tools/include-fixer")
(require 'clang-include-fixer)

;; slowdown fix from Raman.
(defadvice system-users (around  fix pre act comp)
  "Just return user real name."
  (list user-real-login-name))
(global-set-key [f4] (lambda ()
		       (interactive)
		       (persp-switch "@gnus")
		       (unless (get-buffer "*Group*")
			 (gnus))))

;; Stop asking me about freaking .org-gcal-token all the time
(defun ash-ignore-file-threats (original &rest arguments)
  (message "in ash-ignore-file-threads in buffer: %s" (buffer-file-name))
  (unless (equal (buffer-file-name) ".org-gcal-token") (apply original arguments)))

(advice-add 'ask-user-about-supersession-threat :around #'ash-ignore-file-threats)

;; Tramp
(setq tramp-default-method "ssh"
      tramp-default-user "ahyatt"
      tramp-default-host "ahyatt.c.googlers.com"
      tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPersist=yes")


(require 'google3-language-services)
(google3-language-services-setup)
(setq help-at-pt-timer-delay 0
      help-at-pt-display-when-idle 'flymake-diagnostic)

(require 'browse-url)

(setenv "GOPATH" "/usr/local/google/home/ahyatt/go")
(setenv "PATH" (concat (concat (getenv "GOPATH") "/bin") ":" (getenv "PATH")))

(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin/"))
(require 'golint)
(require 'clang-include-fixer)

;; From https://explog.in/dot/emacs/config.html
(defun desaturate-color (color-hex)
  "Converts a color string to its desaturated equivalent hex string"
  (require 'color)
  (apply
   'color-rgb-to-hex
   (append (apply
	    'color-hsl-to-rgb
	    (apply
	     'color-desaturate-hsl
	     `(,@(apply 'color-rgb-to-hsl (color-name-to-rgb color-hex)) 100)))
	   '(2))))

(defun transform-theme-colors (fn)
  "Apply FN to the colors on every active face.

   FN should accept the face symbol and the current color,
   and return the new color to be applied."
  (interactive)
  (mapc
   (lambda (face)
     (mapc
      (lambda (attr)
	(let ((current (face-attribute face attr)))
	  (unless (or (not current)
		      (listp current)
		      (string= current "unspecified")
		      (string= current "t"))
	    (set-face-attribute face nil attr (funcall fn face current)))))
      '(:foreground :background :underline :overline :box :strike-through
		    :distant-foreground))
     (mapc
      (lambda (complex-attr)
	(let* ((full (copy-tree (face-attribute face complex-attr)))
	       (current (if (listp full) (member :color full))))
	  (unless (or (not current)
		      (not (listp full)))
	    (setcar (cdr current) (funcall fn face (cadr current)))
	    (set-face-attribute face nil complex-attr full))))
      '(:underline :overline :box)))
   (face-list)))

(defun desaturate-theme ()
  "As title: desaturate all currently active face colorsj."
  (interactive)
  (transform-theme-colors
   (lambda (face color)
     (desaturate-color color))))

(defun invert-theme ()
  "Take the complement of all currently active colors."
  (interactive)
  (require 'color)
  (transform-theme-colors
   (lambda (face color)
     (apply
      'color-rgb-to-hex
      (color-complement color))))
  (let ((current-ns-appearance (assoc 'ns-appearance default-frame-alist)))
    (cond ((eq (cdr current-ns-appearance) 'light)
	   (setf (cdr current-ns-appearance) 'dark))
	  ((eq (cdr current-ns-appearance) 'dark)
	   (setf (cdr current-ns-appearance) 'light)))))

;; From http://www.modernemacs.com/post/custom-eshell/

(require 'dash)
(require 's)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
	 (lambda () (when ,FORM
		      (-> ,ICON
			  (concat esh-section-delim ,FORM)
			  (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
	    (if (s-blank? acc)
		it
	      (concat acc esh-sep it))
	    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
	  (-reduce-from 'esh-acc "" eshell-funcs)
	  "\n"
	  eshell-prompt-string))

(esh-section esh-dir
             "\xf07c"  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "blue" :bold ultra-bold :underline t))

(require 'magit-branch)
(esh-section esh-git
             "\xe907"  ;  (git icon)
             (magit-get-current-branch)
             '(:foreground "pink"))

(esh-section esh-clock
	     "\xf017"  ;  (clock icon)
	     (format-time-string "%H:%M" (current-time))
	     '(:foreground "forest green"))

;; Below I implement a "prompt number" section
(setq esh-prompt-num 0)
(add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
	    (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))

(esh-section esh-num
	     "\xf0c9"  ;  (list icon)
	     (number-to-string esh-prompt-num)
	     '(:foreground "brown"))

;; Separator between esh-sections
(setq esh-sep "  ")  ; or " | "

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header "\n ")  ; or "\n┌─"

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp " ")   ; or "└─> "
(setq eshell-prompt-string " ")   ; or "└─> "

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)

(require 'google-tricorder)

(setq notmuch-saved-searches (quote
			      ((:name "inbox" :query "tag:inbox" :key "i")
			       (:name "unread" :query "tag:unread" :key "u")
			       (:name "flagged" :query "tag:flagged" :key "f")
			       (:name "sent" :query "tag:sent" :key "t")
			       (:name "drafts" :query "tag:draft" :key "d")
			       (:name "industryinfo" :query "tag:industryinfo is:unread")
			       (:name "misc" :query "tag:misc is:unread")
			       (:name "emacs" :query "tag:emacs is:unread")
			       (:name "geo-offererings" :query "geo-offerings is:unread")
			       (:name "eval-cy" :query "eval-cy is:unread")
			       (:name "bikeshare" :query "bikeshare-in-maps is:unread")
			       (:name "biketowork" :query "biketowork-ny is:unread"))))

;; auth-source-xoauth2
(load "~/src/auth-source-xoauth2/auth-source-xoauth2.el")

;; Fix an issue with sendgmr pointing to the wrong place
(setq google-sendgmr-program "/google/data/ro/projects/gws-sre/sendgmr")

(persp-def-auto-persp "Mail"
                      :parameters '((dont-save-to-file . t))
                      :predicate (lambda (b state)
                                   (when (or
                                          (string-match "notmuch" (buffer-name b))
                                          (string-match "message" (buffer-name b)))
                                     (or state t))))

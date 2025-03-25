;;; -*- lexical-binding: t -*-
;;; Features like Toolbar, Menubar and Scrollbar is not necessary for Emacs.
(mapc (lambda (fn-symbol)
	(and (fboundp fn-symbol) (funcall fn-symbol -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;;; Many places in this script need to know the OS type, cache it to a variable.
(defconst wg-system-is-not-unix (cl-case system-type
				  ((windows-nt ms-dos cygwin) t)
				  (t nil)))

;;; Introduce the keybinding for scroll down from uemacs.
(keymap-global-set "M-SPC" #'set-mark-command)
(keymap-global-set "C-z" #'scroll-down-command)
(keymap-global-set "C-M-z" #'scroll-other-window-down)

;;; Miscellaneous configurations to make Emacs more comfortable.
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

(setq display-time-format "%H:%M")
;; (setq display-time-interval 1)
(display-time-mode 1)

(defmacro wg-path-segment-match (pathname path-string)
  "The path segment PATHNAME could be at the start, at the end, or
in the middle of PATH-STRING.  Any one of the situations make the
match success."
  (let ((p (gensym)) (ps (gensym)))
    `(let ((,p ,pathname) (,ps ,path-string))
       (or ,@(mapcar (lambda (segment)
		       `(string-match-p ,segment ,ps))
		     `((concat "^" (regexp-quote (concat ,p ,path-separator)))
		       (regexp-quote (concat ,path-separator ,p ,path-separator))
		       (concat (regexp-quote (concat ,path-separator ,p)) "$")))))))

;; (wg-path-segment-match "/" "/bin:/usr/bin:/")
;; (wg-path-segment-match "/" "/:/bin:/usr/bin")
;; (wg-path-segment-match "/" "/bin:/:/usr/bin")
;; (wg-path-segment-match "/" "/bin:/usr/bin")

(defun wg-try-drop-trailing (str trailing-str)
  "Drop TRAILING-STR from STR if it is the end of STR.  Do nothing
and return STR otherwise.  There is one exception when STR is
equal to TRAILING-STR, in which case we return STR directly."
  (if (equal str trailing-str)
      str
    (if (string-suffix-p trailing-str str)
	(substring str 0 (- (length str)
			    (length trailing-str)))
      str)))

;; (wg-try-drop-trailing "a/" "/")
;; (wg-try-drop-trailing "a" "/")
;; (wg-try-drop-trailing "/" "/")

(defun wg-add-to-exec-and-env (raw-pathname)
  "Add PATHNAME to both environment variable PATH and
`exec-path'. Adding to `exec-path' won't make shell see the
  commands in PATHNAME.  That's why we need to add it to `PATH',
  too."
  (interactive "DPath to add: ")
  (let ((pathname (wg-try-drop-trailing raw-pathname
					(if wg-system-is-not-unix "\\" "/"))))
    (add-to-list 'exec-path pathname)
    (let ((env-path (getenv "PATH")))
      (unless (wg-path-segment-match pathname env-path)
	(setenv "PATH" (concat pathname path-separator env-path))
	(message "\"%s\" was added to PATH" pathname)))))

(defun wg-find-file-by-pattern (directory pattern)
  "Find the first file in DIRECTORY that matching PATTERN and return
its full path.  PATTERN is the regular expression to match
filename.
e.g. (wg-find-file-by-pattern \"/usr/local/lib/erlang/lib/\" \"^tools\")"
  (let ((files (directory-files directory t)))
    (seq-find (lambda (file)
		(string-match-p pattern (file-name-nondirectory file)))
	      files)))

;;; My Utilities
(wg-add-to-exec-and-env (file-name-concat (getenv "HOME") ".local/bin"))

;;; Windows specific configurations for basic shell functions.
(when wg-system-is-not-unix
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash-args '("--login" "-i"))
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (wg-add-to-exec-and-env "C:/Program Files/Git/usr/bin"))

;;; Use dynamic-bindings variables to re-configure it in more situations.
(defvar wg-prefered-fonts '("cascadia code" "menlo" "consolas" "monospace"))
(defvar wg-font-size 18)

(defun wg-gui-font-config (&optional font-string)
  "Setting a font when running in GUI mode, and the font exists."
  (let* ((prefered-font (seq-find #'x-list-fonts wg-prefered-fonts))
	 (font (let ((font-name (or font-string prefered-font)))
		 (if font-name
		     (format "%s-%d" font-name wg-font-size)))))
    (when font
      (message "Setting font to %s" font)
      (set-frame-font font))))

;; (let ((wg-font-size 18)) (wg-gui-font-config))
;; (let ((wg-font-size 20)) (wg-gui-font-config))

;;; This function is not prefixed on purpose.
(defun load-theme-single (theme)
  "Themes are NOT exclusive, they may affect each other.  This
function disables other themes and left only one."
  (interactive (list (intern (completing-read "Load custom theme: "
					      (mapcar #'symbol-name
						      (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (dolist (old-theme custom-enabled-themes)
    (disable-theme old-theme))
  (load-theme theme t))

(defun wg-prepare-face ()
  "Set font for GUI only.  This function is called on startup, on
new frame creation, and on new connection from clients."
  (when window-system
    (wg-gui-font-config)))

;;; Enabling fullscreen in default-frame-alist will cause some problems on Windows.
;;; Use `M-x' `toggle-frame-fullscreen' to toggle fullscreen.
(defvar wg-default-frame-alist '((width . 80) (height . 32)))

;;; Functions hooked on `emacs-startup-hook' will only run once.  We can safely
;;; reload this file without calling these functions again.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (dolist (a wg-default-frame-alist)
	      (add-to-list 'default-frame-alist a))
	    (wg-prepare-face)
	    ;;(load-theme-single 'modus-vivendi)
	    ))

(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (message "New client is connected to emacs daemon...")
	    (wg-prepare-face)))

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (with-selected-frame frame
	      (wg-prepare-face))))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (show-paren-mode 1)))

;;; Line number is useful, enable it globally.
;;(setq-default display-line-numbers-width 8)
;;(setq-default display-line-numbers t)

(setq-default column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EWW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Disable image loading of EWW.  (Image loading can slow down EWW)
(add-hook 'eww-mode-hook
	  (lambda ()
	    (setq shr-inhibit-images t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionary (which is provided since Emacs 28)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Install `dictd' first: apt install dictd dict dict-{wn,vera,jargon,devil,gcide,foldoc}
;;; Start `dictd' on startup: systemctl enable dictd

;;; Use `M-x' `dictionary-lookup-definition' on words directly.

(when wg-system-is-not-unix
  (setq dictionary-server "dict.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pixel scrolling mode.  (Supported since Emacs 29)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pixel-scroll-precision-mode 1)
;;; Interpolate scrolling via the Page Down and Page Up keys.
(setq pixel-scroll-precision-interpolate-page t)
;;; Make it fluent when bottom got scrolled out of screen and then
;;; scrolled back.
(setq scroll-conservatively 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IDO mode.  (Included in Emacs since 23.1 (2017))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update package indexes: `M-x' `package-refresh-contents'.
;;; Install new package: `M-x' `package-install'.
;;; Delete installed package: `M-x' `package-delete'.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq url-proxy-services '(("http" . "localhost:7890")
			   ("https" . "localhost:7890")))

;;; Install selected packages: `M-x' `package-install-selected-packages'.
(setq package-selected-packages
      '(macrostep company magit paredit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wg-indent-elisp-if-like-cl (indent-point state)
  "For the `if' form of Emacs Lisp.  Align the 2 branches like CL."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (car state)))
    (parse-partial-sexp (point) indent-point 0 t)
    (+ normal-indent 1)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    ;;(put 'if 'lisp-indent-function #'wg-indent-elisp-if-like-cl)
	    (keymap-local-set "C-c e" #'macrostep-expand)))

(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (keymap-local-set "C-<return>" #'eval-print-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar quicklisp-software-dir "~/quicklisp/dists/quicklisp/software/")

(defun wg-slime-init ()
  "Initialize `slime'.  Make sure that slime is installed and can be
found by Emacs before invoking this function."
  (require 'slime-autoloads)
  ;; (setq inferior-lisp-program "clisp")
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-cl-indent))

  ;; Call slime-setup to make macrostep work for common lisp.
  (slime-setup)

  (add-hook 'slime-mode-hook
	    (lambda ()
	      (setq browse-url-browser-function #'eww-browse-url)
	      (keymap-local-set "C-<return>" #'slime-eval-print-last-expression)
	      (keymap-local-set "C-c e" #'macrostep-expand)))

  )

;;; Use the Slime installed by quicklisp to avoid conflicts.
;;;
;;; Install CL HyperSpec: (ql:quickload "clhs")
;;; Create `clhs-use-local.el': (clhs:install-clhs-use-local)
;;; There are some URL related problem with EWW on Windows.

(let ((p (wg-find-file-by-pattern quicklisp-software-dir "^slime")))
  (when p
    (add-to-list 'load-path p)
    (wg-slime-init)
    (unless wg-system-is-not-unix
      (load (expand-file-name "~/quicklisp/clhs-use-local.el") t))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme (Install geiser (geiser-guile, geiser-racket, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq scheme-program-name "guile")
;; (setq scheme-program-name "scheme")
;; (setq scheme-program-name "racket")

;; (setq geiser-mode-eval-last-sexp-to-buffer t)
;; (setq geiser-mode-eval-to-buffer-prefix "\n")

;; (add-hook 'scheme-mode-hook
;; 	  (lambda ()
;; 	    (put 'with-ellipsis 'scheme-indent-function 1)))

;; (add-hook 'geiser-mode-hook
;; 	  (lambda ()
;; 	    (keymap-local-set "C-<return>" #'geiser-eval-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(defun wg-paredit-customize ()
  "This function should be put into hooks of modes where you want to
enable paredit mode."
  (paredit-mode 1)
  ;; "M-(" and "M-)" are already bound by paredit, rebind it with define-key
  (define-key paredit-mode-map (kbd "M-(") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-)") #'paredit-forward-slurp-sexp)

  (keymap-local-set "M-{" #'paredit-backward-barf-sexp)
  (keymap-local-set "M-}" #'paredit-forward-barf-sexp)

  ;; Let's keep `M-?' for `xref'.
  (define-key paredit-mode-map (kbd "M-?") nil)

  ;; Do not insert spaces automatically.
  (add-to-list 'paredit-space-for-delimiter-predicates
	       (lambda (endp delimiter) nil)))

(add-hook 'emacs-lisp-mode-hook #'wg-paredit-customize)
(add-hook 'lisp-mode-hook #'wg-paredit-customize)
(add-hook 'scheme-mode-hook #'wg-paredit-customize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Erlang (Not installed from elpa, but from the OTP library)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (executable-find "erl")
  (add-to-list 'load-path (file-name-concat
			   (shell-command-to-string
			    "erl -noinput -eval 'io:put_chars(code:lib_dir(tools)), halt()'")
			   "emacs"))
  (require 'erlang-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wg-fix-gnu-style-after-complete (s)
  "Fix the GNU coding style problem with `company' mode. GNU style
need a space after function names."
  (save-excursion
    (when (and (search-backward s nil t)
	       (looking-at (concat s "(")))
      (search-forward "(")
      (backward-char 1)
      (insert " "))))

(defun wg-use-linux-kernel-style ()
  (setq indent-tabs-mode t)
  (setq tab-width 8)
  (c-set-style "linux"))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (add-hook 'company-after-completion-hook #'wg-fix-gnu-style-after-complete)
	    (keymap-local-set "C-c e" #'macrostep-expand)))

(add-hook 'c-mode-common-hook
	  #'wg-use-linux-kernel-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tree-sitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use `M-x' `treesit-install-language-grammar' to install grammars.
(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TypeScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)

(setq org-agenda-files '("~/org/work.org" "~/org/home.org" "~/org/misc.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
	(sequence "|" "CANCELED(c)")))

(setq org-agenda-include-diary t)

(defun wg-org-add-links-to-bottom-p ()
  "Run this function with current buffer bound to the org contents.
Point should be at the beginning of the buffer."
  (search-forward-regexp "#\\+OPTIONS:.*html-links:footnotes"
			 (save-excursion (search-forward "\n\n" nil t))
			 t))

;;; Just for making the indentation of `org-element-map' right.
(require 'org-element)

(defun wg-org-add-links-to-bottom (backend)
  "When `#+OPTIONS: html-links:footnote' is found in the head of the
Org file, automatically add a list of all links at the bottom of
the Org document before HTML exporting."
  (when (and (eq backend 'html)
	     (wg-org-add-links-to-bottom-p))
    (save-excursion
      (goto-char (point-max))
      (insert "\n#+BEGIN_EXPORT html\n<br/><ul>\n")
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (link)
	  (let ((url (org-element-property :raw-link link)))
	    (when url
	      (let ((s (org-element-property :contents-begin link))
		    (e (org-element-property :contents-end link)))
		(insert (format "<li>%s: %s</li>\n"
				(buffer-substring-no-properties s e) url)))))))
      (insert "</ul>\n#+END_EXPORT\n"))))

(add-hook 'org-export-before-processing-functions
	  #'wg-org-add-links-to-bottom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E-Mail & USENET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-mail-address "wallacegibbon@aliyun.com")
(setq user-full-name "Wallace Gibbon")

;;; Avoid the image display on startup.
(setq gnus-inhibit-startup-message t)

;;; Passwords for USENET, IMAP and SMTP servers are all configured in `~/.authinfo'.
;;; e.g.
;;; machine news.eternal-september.org login wallacegibbon force yes password xxx
;;; machine imap.aliyun.com login wallacegibbon@aliyun.com password xxx
;;; machine smtp.aliyun.com login wallacegibbon@aliyun.com password xxx

(setq gnus-select-method '(nntp "news.eternal-september.org"))

(setq gnus-posting-styles '(("*.*"
			     (name "Wallace Gibbon")
			     (address "wallacegibbon@aliyun.com"))))

(setq gnus-secondary-select-methods
      '((nnimap "mail.aliyun.com"
		(nnimap-address "imap.aliyun.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl))
	(nnimap "mail.qq.com"
		(nnimap-address "imap.qq.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl))))

(setq gnus-home-directory "~/.gnus/")
(setq gnus-directory "~/Mail/")
(setq gnus-article-date-format-alist '((t . "%Y-%m-%d %H:%M")))

;;; SMTP settings.
(setq send-mail-function 'smtpmail-send-it)

(setq smtpmail-smtp-server "smtp.aliyun.com")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-user "wallacegibbon@aliyun.com")
(setq smtpmail-smtp-pass
      (auth-source-user-and-password "smtp.aliyun.com"))

(defun wg-p-to (buffer-to-print &rest args)
  "In some modes (like gnus) you can not print to *Messages* buffer.
This function write data to a temporary buffer for debugging."
  (declare (indent 1))
  (let ((running-buffer (buffer-name (current-buffer))))
    (with-current-buffer (or (get-buffer buffer-to-print)
			     (generate-new-buffer buffer-to-print))
      (goto-char (point-max))
      (insert (format "<in buffer %s>\n" running-buffer))
      (insert (apply #'format args))
      (insert "\n"))))

(defvar wg-smtp-accounts
  ;; Format: Sender Mail address - SMTP Server - Port - type - Username
  '(("wallacegibbon@aliyun.com" "smtp.aliyun.com" 465 ssl "Wallace Gibbon")
    ("opf-programming@qq.com" "smtp.qq.com" 465 ssl "OPF Creator")))

(defun wg-message-send-and-exit ()
  "Choose the right SMTP configuration from `wg-smtp-accounts' and
then send the mail by calling `message-send-and-exit'."
  (interactive)
  (let* ((sender (message-fetch-field "From"))
	 (account (seq-find (lambda (c)
			      (string-match-p (regexp-quote (car c)) sender))
			    wg-smtp-accounts)))
    (unless account
      (error "Failed finding configuration for %s" sender))
    (pcase-let ((`(,email ,smtp-server ,port ,type ,name) account))
      (cl-progv
	  (list 'smtpmail-smtp-server 'smtpmail-smtp-service 'smtpmail-stream-type
		'smtpmail-smtp-user 'smtpmail-smtp-pass)
	  (list smtp-server port type
		email (auth-source-user-and-password smtp-server))
	(message "Fixing the `From' field to %s <%s>" name email)
	(message-replace-header "From"
				(format "%s <%s>" name email))
	(message-send-and-exit)))))

;;; Global variables for holding current active mail.
(defvar wg-current-mail-from nil)
(defvar wg-current-mail-to nil)

(defun wg-gnus-fix-mail-on-replying ()
  "Fix the From and To headers of E-mail response.  The right values
are stored in `wg-current-mail-from' and `wg-current-mail-to'."
  (interactive)
  (unless (and wg-current-mail-from wg-current-mail-to)
    (error "This is not an E-mail replying, nothing to fix."))
  (message-replace-header "To" wg-current-mail-from)
  (message-replace-header "From" wg-current-mail-to)
  (setq wg-current-mail-to nil)
  (setq wg-current-mail-from nil))

(defun wg-improve-gnus-email-experience ()
  (add-hook 'gnus-message-setup-hook
	    (lambda ()
	      ;;(wg-p-to "gnus-debug" "gnus-message-setup-hook called...")
	      (local-set-key (kbd "C-c C-c") #'wg-message-send-and-exit)
	      (local-set-key (kbd "C-c f") #'wg-gnus-fix-mail-on-replying)))
  (add-hook 'gnus-article-decode-hook
	    (lambda ()
	      ;;(wg-p-to "gnus-debug" "article-decode-hook called...")
	      (setq wg-current-mail-from (message-fetch-field "From"))
	      (setq wg-current-mail-to (message-fetch-field "To")))))

(add-hook 'gnus-mode-hook #'wg-improve-gnus-email-experience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Company (auto complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Emacs Lisp Utilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar wg-elisp-files
  '("~/playground/emacs-lisp-playground/dired-util.el"))

(mapc #'load wg-elisp-files)

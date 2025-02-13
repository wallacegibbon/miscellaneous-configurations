;;; -*- lexical-binding: t -*-
;;; The most practical way to use Emacs ergonomically in a normal keyboard:
;;;   Using SPACE as both SPACE and CTRL (when hold). Using CAPSLOCK as ESCAPE.
;;;
;;; Background: https://www.emacswiki.org/emacs/MovingTheCtrlKey
;;;
;;; Projects that make this possible:
;;; - Linux: https://github.com/pietroiusti/janus-key
;;; - MSWin: https://github.com/lydell/dual
;;;
;;; Forks (modified) I am using:
;;; - Linux: https://github.com/wallacegibbon/janus-key
;;; - MSWin: https://github.com/wallacegibbon/dual
;;;
;;; This could cause some delay during content input, which is annoying for
;;; writting documents but okay for writting code.

;;; Since the SPACE key is used for CTRL, we can't press Ctrl-Space anymore.
(keymap-global-set "C-2" #'set-mark-command)

;;; The "C-z" (suspend-frame) is not useful in GUI environment.
(when window-system
  (keymap-global-unset "C-z"))

;;; Miscellaneous configurations to make Emacs more comfortable.
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

;;; A wider `fill-column' (whose default value `70') is better for documents.
(setq-default fill-column 80)

;;; The Toolbar, Menubar and Scrollbar is not necessary for Emacs.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Displaying system time is sometimes useful in terminal, we prepare the
;;; format and enable it when necessary.
(setq display-time-format "%H:%M")
;; (setq display-time-interval 1)
(display-time-mode 1)

(defconst system-is-not-unix (cl-case system-type
			       ((windows-nt ms-dos cygwin) t)
			       (t nil)))

(defmacro path-segment-match (pathname path-string)
  "The path segment PATHNAME could be at the start, at the end, or in the middle
of PATH-STRING.  Any one of the situations make the match success."
  (let ((p (gensym)) (ps (gensym)))
    `(let ((,p ,pathname) (,ps ,path-string))
       (or ,@(mapcar (lambda (segment)
		       `(string-match-p ,segment ,ps))
		     `((concat "^" (regexp-quote (concat ,p ,path-separator)))
		       (regexp-quote (concat ,path-separator ,p ,path-separator))
		       (concat (regexp-quote (concat ,path-separator ,p)) "$")))))))

;; (path-segment-match "/" "/bin:/usr/bin:/")
;; (path-segment-match "/" "/:/bin:/usr/bin")
;; (path-segment-match "/" "/bin:/:/usr/bin")
;; (path-segment-match "/" "/bin:/usr/bin")

(defun add-to-exec-and-env (raw-pathname)
  "Add PATHNAME to both environment variable PATH and `exec-path'. Adding to
  `exec-path' won't make shell see the commands in PATHNAME.  That's why we need
  to add it to `PATH', too."
  (interactive "DPath to add: ")
  (cl-labels ((drop-tailing (string trailing-str)
		(if (equal string trailing-str)
		    string
		  (if (string-suffix-p trailing-str string)
		      (substring string 0 (- (length string) (length trailing-str)))
		    string))))
    (let ((pathname (drop-tailing raw-pathname
				  (if system-is-not-unix "\\" "/"))))
      (add-to-list 'exec-path pathname)
      (let ((env-path (getenv "PATH")))
	(unless (path-segment-match pathname env-path)
	  (setenv "PATH" (concat pathname path-separator env-path))
	  (message "Environment preparing: \"%s\" was added to PATH"
		   pathname))))))

;;; My Utilities
(add-to-exec-and-env (file-name-concat (getenv "HOME") ".local/bin"))

;;; Windows specific configurations for basic shell functions.
(when system-is-not-unix
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash-args '("--login" "-i"))
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (add-to-exec-and-env "C:/Program Files/Git/usr/bin"))

;;; Use dynamic-bindings variables to re-configure it in more situations.
(defvar *my-prefered-fonts* '("cascadia code" "menlo" "consolas" "monospace"))
(defvar *my-font-size* 20)

(defun config-non-console-font (&optional font-string)
  "Setting a font when possible (Running GUI version Emacs, and font exists)."
  (let* ((prefered-font (seq-find #'x-list-fonts *my-prefered-fonts*))
	 (font (let ((font-name (or font-string prefered-font)))
		 (if font-name
		     (format "%s-%d" font-name *my-font-size*)))))
    (when font
      (message "Setting font to %s" font)
      (set-frame-font font))))

;; (let ((*my-font-size* 16)) (config-non-console-font))
;; (let ((*my-font-size* 20)) (config-non-console-font))

(defun load-theme-single (theme)
  "Themes are NOT exclusive, they may affect each other.  This function disables
other themes and left only one."
  (interactive (list (intern (completing-read "Load custom theme: "
					      (mapcar #'symbol-name
						      (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (dolist (old-theme custom-enabled-themes)
    (disable-theme old-theme))
  (load-theme theme t))

;;; Functions hooked on `emacs-startup-hook' will only run once.  We can safely
;;; reload this file without calling these functions again.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (when window-system
	      (add-to-list 'default-frame-alist '(fullscreen . maximized))
	      (add-to-list 'default-frame-alist '(undecorated . t))
	      (config-non-console-font)
	      (load-theme-single 'modus-vivendi))))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (show-paren-mode 1)))

;;; Line number is useful, enable it globally.
(setq-default display-line-numbers-width 8)
(setq-default display-line-numbers t)

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

(when system-is-not-unix
  (setq dictionary-server "dict.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-emacs-lisp-in-cl-style (indent-point state)
  "Indent the `if' form in Common Lisp style.  (Align both condition branch)

In Emacs 29, `lisp-indent-function' was changed to improve the way indentation
is handled, and `common-lisp-indent-function' no longer works the same way for
Emacs Lisp."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (car state)))
    (parse-partial-sexp (point) indent-point 0 t)
    (+ normal-indent 1)))

;; (add-hook 'emacs-lisp-mode-hook
;; 	  (lambda ()
;; 	    (put 'if 'lisp-indent-function #'indent-emacs-lisp-in-cl-style)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (keymap-local-set "C-c C-m" #'macrostep-expand)))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (put 'with-ellipsis 'scheme-indent-function 1)))

(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (keymap-local-set "C-<return>" #'eval-print-last-sexp)))

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
;;; Install selected packages: `M-x' `package-install-selected-packages'.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq url-proxy-services '(("http" . "localhost:7890")
			   ("https" . "localhost:7890")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'paredit)

(defun customize-paredit-mode ()
  "This function should be put into hooks of modes where you want to enable
paredit mode."
  ;; "M-(" and "M-)" are already bound by paredit, rebind it with define-key
  (define-key paredit-mode-map (kbd "M-(") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-)") #'paredit-forward-slurp-sexp)

  (keymap-local-set "M-{" #'paredit-backward-barf-sexp)
  (keymap-local-set "M-}" #'paredit-forward-barf-sexp)

  ;; Let's keep `M-?' for `xref'.
  (define-key paredit-mode-map (kbd "M-?") nil)

  ;; Do not insert spaces automatically.
  (add-to-list 'paredit-space-for-delimiter-predicates (lambda (endp delimiter) nil))

  (paredit-mode 1)
  (auto-fill-mode 1))

(add-hook 'emacs-lisp-mode-hook #'customize-paredit-mode)
(add-hook 'lisp-mode-hook #'customize-paredit-mode)
(add-hook 'scheme-mode-hook #'customize-paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq inferior-lisp-program "sbcl")
;; ;; (setq inferior-lisp-program "clisp")
;; (setq slime-contribs '(slime-fancy slime-cl-indent))
;; (require 'slime)

;;; Make HyperSpec installed by `(ql:quickload "clhs")' available to emacs.
;; (load "/home/wallace/.quicklisp/clhs-use-local.el" t)
;; (setq browse-url-browser-function 'eww-browse-url)

;; (add-hook 'slime-mode-hook
;; 	  (lambda ()
;; 	    (keymap-local-set "C-<return>" #'slime-eval-print-last-expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme (Install geiser (geiser-guile, geiser-racket, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq scheme-program-name "guile")
;; (setq scheme-program-name "scheme")
;; (setq scheme-program-name "racket")

;; (setq geiser-mode-eval-last-sexp-to-buffer t)
;; (setq geiser-mode-eval-to-buffer-prefix "\n")

;; (add-hook 'geiser-mode-hook
;; 	  (lambda ()
;; 	    (keymap-local-set "C-<return>" #'geiser-eval-last-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fix-gnu-style-after-complete (s)
  "Fix the GNU style problem with `company' mode.  (No space after function name)"
  (save-excursion
    (when (and (search-backward s nil t)
	       (looking-at (concat s "(")))
      (search-forward "(")
      (backward-char 1)
      (insert " "))))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (add-hook 'company-after-completion-hook
		      #'fix-gnu-style-after-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Erlang (Not installed from elpa, but from the OTP library)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function was used to find the erlang's "tools-xx" directory by pattern.
;;; e.g. (find-file-by-pattern (concat erlang-root-dir "/lib") "^tools*")
(defun find-file-by-pattern (directory pattern)
  "Find the first file in DIRECTORY that matching PATTERN and return its full
path.  PATTERN is the regular expression to match filename."
  (let ((files (directory-files directory t)))
    (seq-find (lambda (file)
		(string-match-p pattern (file-name-nondirectory file)))
	      files)))

;;; Add Erlang package path to `load-path'.
(defun erlang-package-path ()
  (file-name-concat (shell-command-to-string
		     "erl -noinput -eval 'io:put_chars(code:lib_dir(tools)), halt()'")
		    "emacs"))

(when (executable-find "erl")
  (add-to-list 'load-path (erlang-package-path))
  (require 'erlang-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Company (auto complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
	(sequence "|" "CANCELED(c)")))

(setq org-agenda-include-diary t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E-Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Both IMAP and SMTP use ~/.authinfo for password management.

;;; Avoid the image display on startup.
(setq gnus-inhibit-startup-message t)

(setq gnus-select-method
      '(nnimap "mail.aliyun.com"
	       (nnimap-address "imap.aliyun.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      '((nnimap "mail.qq.com"
		(nnimap-address "imap.qq.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl))))

;;; Make sure read messages are visible.
(setq gnus-summary-hide-read nil)

;;; Ensure that read messages are not collapsed or excluded in threads.
(setq gnus-summary-ignore-threads nil)
(setq gnus-summary-visible-threads t)

(setq gnus-article-sort-functions '(gnus-article-sort-by-date))

(setq gnus-home-directory "~/.gnus/")
(setq gnus-directory "~/Mail/")
(setq gnus-article-date-format-alist '((t . "%Y-%m-%d %H:%M")))

;;; SMTP settings.
(setq send-mail-function 'smtpmail-send-it)
(setq user-mail-address "wallacegibbon@aliyun.com")
(setq user-full-name "Wallace Gibbon")

;;; These default settings will be overrided by *my-smtp-accounts*.
(setq smtpmail-smtp-server "smtp.aliyun.com")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-user "wallacegibbon@aliyun.com")
(setq smtpmail-smtp-pass
      (auth-source-user-and-password "smtp.aliyun.com"))

(defvar *my-smtp-accounts*
  ;; Format: Sender Mail address - SMTP Server - Port - type - Username
  '(("wallacegibbon@aliyun.com" "smtp.aliyun.com" 465 ssl "Wallace Gibbon")
    ("opf-programming@qq.com" "smtp.qq.com" 465 ssl "OPF Creator")))

(defmacro dynamic-let (var-binds &rest body)
  "This is not real dynamic scoping, but a dirty emulation.  Variables will be
  restored after the finish of body."
  (declare (indent 1))
  (let ((tmpvars (mapcar (lambda (x) (gensym))
			 var-binds)))
    (cl-labels ((>> (name value set)
		  (cons `(setq ,name ,value) set))
		(prepare (tmpvars binds ops1 ops2 ops3)
		  (pcase (list tmpvars binds)
		    (`((,t1 . ,t-rest) ((,n1 ,v1) . ,b-rest))
		     (prepare t-rest b-rest (>> t1 n1 ops1) (>> n1 v1 ops2) (>> n1 t1 ops3)))
		    ('(() ())
		     (mapcar #'reverse (list ops1 ops2 ops3)))
		    (data
		     (error "invalid data: %S" data)))))
      (pcase-let ((`(,ops1 ,ops2 ,ops3)
		   (prepare tmpvars var-binds '() '() '())))
	`(let ,tmpvars
	   (unwind-protect
	       (progn ,@ops1 ,@ops2 ,@body)
	     ,@ops3))))))

(defun advanced-message-send-and-exit ()
  "Choose the right SMTP configuration from `*my-smtp-accounts*' and then send
the mail by calling `message-send-and-exit'."
  (interactive)
  (let* ((sender (message-fetch-field "From"))
	 (account (seq-find (lambda (c)
			      (string-match (regexp-quote (car c)) sender))
			    *my-smtp-accounts*)))
    (unless account
      (error "Failed finding configuration for %s" sender))
    (pcase-let ((`(,email ,smtp-server ,port ,type ,name) account))
      (dynamic-let ((smtpmail-smtp-server smtp-server)
		    (smtpmail-smtp-service port)
		    (smtpmail-stream-type type)
		    (smtpmail-smtp-user email)
		    (smtpmail-smtp-pass
		     (auth-source-user-and-password smtp-server)))
	(message-replace-header "From"
				(format "%s <%s>" name email))
	(message-send-and-exit)))))

(add-hook 'gnus-message-setup-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") #'advanced-message-send-and-exit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Emacs Lisp Utilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *my-elisp-files* nil)

(add-to-list '*my-elisp-files* "~/playground/emacs-lisp-playground/dired-util.el")

(mapc (lambda (filename)
	(and (file-exists-p filename)
	     (load filename)))
      *my-elisp-files*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/work.org" "~/org/home.org" "~/org/misc.org"))
 '(package-selected-packages '(company magit paredit)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

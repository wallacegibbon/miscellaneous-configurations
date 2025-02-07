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
(setq display-time-format "%F %R")
;; (setq display-time-interval 1)
;; (display-time-mode 1)

(defconst system-is-not-unix (cl-case system-type
			       ((windows-nt ms-dos cygwin) t)
			       (t nil)))

(defconst directory-separator (if system-is-not-unix "\\" "/")
  "\"/\" on Unix and \"\\\" on Windows")

(defmacro path-segment-match (pathname path-string)
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

;; (macroexpand '(path-segment-match "/" "/bin:/usr/bin:/:"))
;; (path-segment-match "/" "/bin:/usr/bin:/")
;; (path-segment-match "/" "/:/bin:/usr/bin")
;; (path-segment-match "/" "/bin:/:/usr/bin")
;; (path-segment-match "/" "/bin:/usr/bin")

(defun drop-tailing (string trailing-str)
  (if (equal string trailing-str)
      string
    (if (string-suffix-p trailing-str string)
	(substring string 0 (1- (length string)))
      string)))

;; (drop-tailing "a/b/c/" "/")
;; (drop-tailing "a/b/c" "/")
;; (drop-tailing "/" "/")

(defun add-to-exec-and-env (raw-pathname)
  "Add PATHNAME to both environment variable PATH and `exec-path'.
Adding to to `exec-path' won't make shell see the commands in
PATHNAME.  That's why we need to add it to `PATH', too."
  (interactive "DPath to add: ")
  (let ((pathname (drop-tailing raw-pathname directory-separator)))
    (add-to-list 'exec-path pathname)
    (let ((env-path (getenv "PATH")))
      (unless (path-segment-match pathname env-path)
	(setenv "PATH" (concat pathname path-separator env-path))
	(message "\"%s\" is added to PATH" pathname)))))

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

;; (let ((*my-font-size* 16)) (config-non-console-font))
;; (let ((*my-font-size* 20)) (config-non-console-font))

(defun config-non-console-font (&optional font-string)
  (let* ((default-font (seq-find #'x-list-fonts *my-prefered-fonts*))
	 (font (let ((font-name (or font-string default-font)))
		 (if font-name
		     (format "%s-%d" font-name *my-font-size*)
		   "NOFONT"))))
    (message "Setting font to %s" font)
    (set-frame-font font)))


;;; Themes are NOT exclusive, they may affect each other.  This function
;;; disables other themes and left only one.
(defun load-theme-single (theme)
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
	    (setq-local display-line-numbers-width 8)
	    (setq-local column-number-mode 1)
	    (display-line-numbers-mode 1)
	    (show-paren-mode 1)))


;;; Disable image loading of EWW.  (Image loading can slow down emacs)
(setq shr-inhibit-images t)


;;; Dictionary (which is provided since Emacs 28) setting:
;;; Install `dictd' first: apt install dictd dict dict-{wn,vera,jargon,devil,gcide,foldoc}
;;; Start `dictd' on startup: systemctl enable dictd

(add-hook 'eww-mode-hook
	  (lambda ()
	    (keymap-local-set "C-c l" #'dictionary-lookup-definition)
	    (text-scale-adjust -1)))

(add-hook 'Info-mode-hook
	  (lambda ()
	    (keymap-local-set "C-c l" #'dictionary-lookup-definition)))

(add-hook 'dictionary-mode-hook
	  (lambda ()
	    (keymap-local-set "C-c l" #'dictionary-lookup-definition)
	    (text-scale-adjust -2)))

(when system-is-not-unix
  (setq dictionary-server "dict.org"))


;;; In Emacs 29, `lisp-indent-function' was changed to improve the way
;;; indentation is handled, and `common-lisp-indent-function' no longer works
;;; the same way for Emacs Lisp.
(defun my-common-lisp-if-indent (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (car state))) ; Go to the second element (the body)
    (parse-partial-sexp (point) indent-point 0 t)
    (+ normal-indent 1)))

;; (add-hook 'emacs-lisp-mode-hook
;; 	  (lambda ()
;; 	    (put 'if 'lisp-indent-function #'my-common-lisp-if-indent)))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (put 'with-ellipsis 'scheme-indent-function 1)))

(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (keymap-local-set "C-<return>" #'eval-print-last-sexp)))


;;; Enable the pixel scrolling mode.  (Supported since Emacs 29)
(pixel-scroll-precision-mode 1)
;;; Interpolate scrolling via the Page Down and Page Up keys.
(setq pixel-scroll-precision-interpolate-page t)
;;; Make it fluent when bottom got scrolled out of screen and then
;;; scrolled back.
(setq scroll-conservatively 10000)


;;; Enable the awesome IDO mode.  (Included in Emacs since 23.1 (2017))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; Update package indexes: `M-x' and `package-refresh-contents'.
;;; Install new package: `M-x' and `package-install'.
;;; Install selected packages: `M-x' and `package-install-selected-packages'.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq url-proxy-services '(("http" . "localhost:7890")
			   ("https" . "localhost:7890")))


;;; When paredit keybindings (like `C-)') are not working on Windows.  Check
;;; system input method and disable the keybinding for input method switching.

(require 'paredit)

(defun my-paredit-hookfn ()
  "This function should be put into hooks of modes where you want to
enable paredit mode."
  ;; The default key bindings for `paredit' is good but requiring `Shift' key.
  ;; We use more ergonomic keybindings for common operations.
  (keymap-local-set "C-8" #'paredit-backward-slurp-sexp)
  (keymap-local-set "C-9" #'paredit-forward-slurp-sexp)
  (keymap-local-set "C-," #'paredit-backward-barf-sexp)
  (keymap-local-set "C-." #'paredit-forward-barf-sexp)
  ;; Let's keep `M-?' for `xref'.
  (define-key paredit-mode-map (kbd "M-?") nil)
  ;; Do not insert spaces automatically.
  (add-to-list 'paredit-space-for-delimiter-predicates (lambda (endp delimiter) nil))
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook #'my-paredit-hookfn)
(add-hook 'lisp-mode-hook #'my-paredit-hookfn)
(add-hook 'scheme-mode-hook #'my-paredit-hookfn)


;;; Common Lisp
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


;;; Scheme (Install geiser (geiser-guile, geiser-racket, etc.)
;; (setq scheme-program-name "guile")
;; (setq scheme-program-name "scheme")
;; (setq scheme-program-name "racket")

;; (setq geiser-mode-eval-last-sexp-to-buffer t)
;; (setq geiser-mode-eval-to-buffer-prefix "\n")

;; (add-hook 'geiser-mode-hook
;; 	  (lambda ()
;; 	    (keymap-local-set "C-<return>" #'geiser-eval-last-sexp)))


;;; To solve the GNU-style problem of company. (no space after function name)
(defun fix-gnu-style-after-complete (s)
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


;;; This function was used to find the erlang's "tools-xx" directory by pattern.
;;; Usage: (find-file-by-pattern (concat erlang-root-dir "/lib") "^tools*")
(defun find-file-by-pattern (directory pattern)
  "Find the first file in DIRECTORY that matching PATTERN and return
its full path.  PATTERN is the regular expression to match
filename."
  (let ((files (directory-files directory t)))
    (seq-find (lambda (file)
		(string-match-p pattern (file-name-nondirectory file)))
	      files)))

;;; Erlang (Not installed from elpa, but from the OTP library)
;;; Add Erlang package path to `load-path'.
(defun erlang-package-path ()
  (file-name-concat (shell-command-to-string
		     "erl -noinput -eval 'io:put_chars(code:lib_dir(tools)), halt()'")
		    "emacs"))

(when (executable-find "erl")
  (add-to-list 'load-path (erlang-package-path))
  (require 'erlang-start))


;;; Company (auto complete)
(add-hook 'after-init-hook #'global-company-mode)


;;; Magit
(require 'magit)


;;; Org
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c c" 'org-capture)


(defun load-when-exist (filename)
  (and (file-exists-p filename)
       (load filename)))

(load-when-exist "~/playground/emacs-lisp-playground/dired-util.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages '(company magit paredit)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

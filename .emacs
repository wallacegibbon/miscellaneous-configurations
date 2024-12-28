(defun config-non-console-font (&optional font-string)
  (let* ((prefered-fonts '("cascadia code" "consolas" "ubuntu mono" "menlo" "monospace"))
	 (default-font (seq-find #'x-list-fonts prefered-fonts))
	 (font (appropriate-font (or font-string default-font))))
    (message "Setting font to %s" font)
    (set-frame-font font)))

(defun appropriate-font (font-name)
  "Make a valid font string who can be used as the argument of `set-frame-font'."
  (let ((font-size (if (is-high-resolution-screen) 20 16)))
    (and font-name
	 (format "%s-%d" font-name font-size))))

;;; TODO: check screen resolution on runtime
(defun is-high-resolution-screen ()
  t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (when window-system
	      (add-to-list 'default-frame-alist '(fullscreen . maximized))
	      ;;(load-theme 'deeper-blue t)
	      (load-theme 'dichromacy t)
	      (config-non-console-font))))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (show-paren-mode t)
	    (setq-local column-number-mode t)
	    (display-line-numbers-mode t)
	    (setq-local display-line-numbers-width 8)
	    (pixel-scroll-precision-mode)))

;;; In Emacs 29, `lisp-indent-function' was changed to improve the way indentation is handled,
;;; and `common-lisp-indent-function' no longer works the same way for Emacs Lisp.
(defun my-common-lisp-if-indent (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (car state))) ; Go to the second element (the body)
    (parse-partial-sexp (point) indent-point 0 t)
    (+ normal-indent 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (put 'if 'lisp-indent-function #'my-common-lisp-if-indent)))

;;; Windows lack some basic tools like `diff`. use those provided by Git toolchain.
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin"))

(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Display time in mode lines.
;;(setq display-time-interval 1)
(setq display-time-format "%F %R")
(display-time-mode)

(defun filter-mwheel-always-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (apply orig args)
      (setq mwheel-coalesce-scroll-events t)))

(defun filter-mwheel-never-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
non-coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (setq mwheel-coalesce-scroll-events nil)
      (apply orig args)))

(advice-add 'pixel-scroll-precision
	    :around #'filter-mwheel-never-coalesce)

;;; Coalesce for default scrolling (which is still used for horizontal scrolling)
;;; and text scaling (bound to ctrl + mouse wheel by default).
(advice-add 'mwheel-scroll
	    :around #'filter-mwheel-always-coalesce)
(advice-add 'mouse-wheel-text-scale
	    :around #'filter-mwheel-always-coalesce)


;;; Update package indexes: `M-x' and `package-refresh-contents'.
;;; Install new package: `M-x' and `package-install'.
;;; Install selected packages: `M-x' and `package-install-selected-packages'.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq url-proxy-services '(("http" . "localhost:7890")
			   ("https" . "localhost:7890")))

(custom-set-variables
 '(package-selected-packages '(company magit paredit)))

(custom-set-faces
 )


(require 'paredit)

;;; `paredit' is useful for all lisp dialects.
(defun shared-lisp-configuration ()
  (paredit-mode)
  (local-set-key (kbd "C-0") #'paredit-forward-slurp-sexp)
  (local-set-key (kbd "C-9") #'paredit-forward-barf-sexp))

(add-hook 'lisp-mode-hook #'shared-lisp-configuration)
(add-hook 'emacs-lisp-mode-hook #'shared-lisp-configuration)
(add-hook 'scheme-mode-hook #'shared-lisp-configuration)


;;; Scheme
(setq scheme-program-name "scheme")	; Chez Scheme
;; (setq scheme-program-name "racket")	; Racket

(require 'cmuscheme)
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-<return>") #'scheme-send-last-sexp-split-window)))


;;; Bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
	       (get-buffer scheme-buffer)
	       (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun switch-other-window-to-buffer (name)
  (other-window 1)
  (switch-to-buffer name)
  (other-window 1))

(defun my-split-window (window-name)
  (cond ((= 1 (count-windows))
	 (split-window-vertically (floor (* 0.5 (window-height))))
	 (switch-other-window-to-buffer window-name))
	((not (member window-name
		      (mapcar (lambda (w) (buffer-name (window-buffer w)))
			      (window-list))))
	 (switch-other-window-to-buffer window-name))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (my-split-window "*scheme*")
  (scheme-send-last-sexp))


(defun find-file-by-pattern (directory pattern)
  "Find the first file in DIRECTORY matching PATTERN and return its full path.
PATTERN is a regular expression to match file names."
  (let ((files (directory-files directory t)))
    (seq-find (lambda (file)
		(string-match-p pattern (file-name-nondirectory file)))
	      files)))

(defun first-existing-path (path-list)
  (seq-find #'file-exists-p path-list))


;;; Erlang (Not installed from elpa, but from the OTP library)
(setq erlang-root-dir
      (cond ((eq system-type 'windows-nt)
	     "C:/Program Files/Erlang OTP")
	    (t
	     (seq-find #'file-directory-p '("/usr/local/lib/erlang" "/usr/lib/erlang")))))

(add-to-list 'load-path
	     (concat (find-file-by-pattern (concat erlang-root-dir "/lib") "^tools*")
		     "/emacs"))

(require 'erlang-start)


;;; Company (auto complete)
(add-hook 'after-init-hook #'global-company-mode)

;;; Magit
(require 'magit)


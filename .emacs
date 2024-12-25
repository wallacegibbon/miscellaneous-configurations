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

(defun customize-window-system ()
  (config-non-console-font)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (load-theme 'deeper-blue t))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (when window-system
	      (customize-window-system))))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (show-paren-mode t)
	    (setq-local column-number-mode t)
	    (display-line-numbers-mode t)
	    (setq-local display-line-numbers-width 8)
	    (pixel-scroll-precision-mode)))

(setq lisp-indent-function #'common-lisp-indent-function)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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

(advice-add 'pixel-scroll-precision :around #'filter-mwheel-never-coalesce)

;; Coalesce for default scrolling (which is still used for horizontal scrolling)
;; and text scaling (bound to ctrl + mouse wheel by default).
(advice-add 'mwheel-scroll :around #'filter-mwheel-always-coalesce)
(advice-add 'mouse-wheel-text-scale :around #'filter-mwheel-always-coalesce)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq url-proxy-services '(("http" . "localhost:8082")
			   ("https" . "localhost:8082")))

;;; `slime' and `auto-complete' are not builtin packages, we need to install them.


;;; Common Lisp
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-cl-indent))
(require 'slime)
(add-hook 'slime-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-j") #'slime-eval-print-last-expression-split-window)))


;;; Scheme
(setq scheme-program-name "scheme")
(require 'cmuscheme)
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-j") #'scheme-send-last-sexp-split-window)))


;; bypass the interactive question and start the default interpreter
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

(defun slime-eval-print-last-expression-split-window (string)
  "This function is modified from `slime-eval-print-last-expression'."
  (interactive (list (slime-last-expression)))
  (my-split-window "*slime-repl sbcl*")
  (insert "\n")
  (slime-eval-print string))


(defun find-file-by-pattern (directory pattern)
  "Find the first file in DIRECTORY matching PATTERN and return its full path.
PATTERN is a regular expression to match file names."
  (let* ((files (directory-files directory t)))
    (seq-find (lambda (file)
		(string-match-p pattern (file-name-nondirectory file)))
	      files)))

;;; Erlang
(setq erlang-root-dir (cond ((memq system-type '(windows-nt ms-dos))
			     "C:/Program Files/Erlang OTP")
			    (t
			     "/usr/local/lib/erlang")))

(add-to-list 'load-path
	     (concat (find-file-by-pattern (concat erlang-root-dir "/lib") "^tools*")
		     "/emacs"))

(require 'erlang-start)


;;; Auto complete
(require 'auto-complete)
(ac-config-default)

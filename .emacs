;;; -*- mode: Lisp -*-

(defun add-to-exec-and-env (pathname)
  "Add `pathname' to environment variable PATH and emacs's `exec-path'.
return t when it is added, and nil when it's already in PATH."
  (add-to-list 'exec-path pathname t)
  (let ((env-path (getenv "PATH")))
    (if (string-match-p (regexp-quote pathname) env-path)
        nil
	(progn (setenv "PATH" (concat pathname ":" env-path))
               t))))

;;(add-to-exec-and-env "/usr/local/bin")

(defun appropriate-font (font-name)
  "Make a valid font string who can be used as the argument of `set-frame-font'."
  (let ((font-size (if (is-high-resolution-screen) 20 16)))
    (and font-name
	 (format "%s-%d" font-name font-size))))

(defun is-high-resolution-screen ()
  t)

(defun config-non-console-font (&optional font-string)
  (let* ((prefered-fonts '("cascadia code" "consolas" "ubuntu mono" "menlo" "monospace"))
	 (default-font (seq-find #'x-list-fonts prefered-fonts))
	 (font (appropriate-font (or font-string default-font))))
    (message "Setting font to %s" font)
    (set-frame-font font)))

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
	    (setq-local display-line-numbers-width 8)))

(setq lisp-indent-function 'common-lisp-indent-function)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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
	    (local-set-key (kbd "C-j") #'slime-eval-print-last-expression)))

;;; Scheme
;;; The scheme configuration comes from YinWang's configuration:
;;; https://www.yinwang.org/blog-cn/2013/04/11/scheme-setup
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

(defun scheme-split-window ()
  (cond
    ((= 1 (count-windows))
     (split-window-vertically (floor (* 0.68 (window-height))))
     ;; (split-window-horizontally (floor (* 0.5 (window-width))))
     (switch-other-window-to-buffer "*scheme*"))
    ((not (member "*scheme*"
		  (mapcar (lambda (w) (buffer-name (window-buffer w)))
			  (window-list))))
     (switch-other-window-to-buffer "*scheme*"))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

;;; Auto complete
(require 'auto-complete)
(ac-config-default)

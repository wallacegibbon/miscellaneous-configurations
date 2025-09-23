;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatible keybindings with Modified Micro Emacs (`me`).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "C-x m" #'set-mark-command)
(keymap-global-set "C-z" #'scroll-down-command)
(keymap-global-set "C-x x" #'next-buffer)
(keymap-global-set "C-x ," #'beginning-of-buffer)
(keymap-global-set "C-x ." #'end-of-buffer)
(keymap-global-set "C-x j" #'kill-ring-save)
(keymap-global-set "C-x k" #'kill-region)
(keymap-global-set "C-x C-k" #'ido-kill-buffer)
(keymap-global-set "C-x r" #'query-replace)
(keymap-global-set "C-x g" #'goto-line)

(defun me-newline-and-indent ()
  "Insert a newline, and keep its indentation same as current line."
  (interactive)
  (let* ((n-spaces (current-indentation))
	 (n-tabs (/ n-spaces 8))
	 (n-trailing (% n-spaces 8)))
    (insert ?\n)
    (dotimes (i n-tabs) (insert ?\t))
    (dotimes (i n-trailing) (insert ?\s))))

(defun me-use-normal-tab ()
  "Micro Emacs use TABs only, we make it default here, too."
  (keymap-local-set "TAB" (lambda () (interactive) (insert ?\t)))
  (keymap-local-set "DEL" (lambda () (interactive) (backward-delete-char 1)))
  (keymap-local-set "C-j" #'me-newline-and-indent)
  (electric-indent-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous configurations to make Emacs more comfortable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Features like Toolbar, Menubar and Scrollbar is not necessary for Emacs.
(mapc (lambda (fn-symbol) (and (fboundp fn-symbol) (funcall fn-symbol -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;;; Many places in this script need to know the OS type, cache it to a variable.
(defconst wg-system-is-not-unix (cl-case system-type
				  ((windows-nt ms-dos cygwin) t)
				  (t nil)))

(setq ring-bell-function #'ignore)
(setq inhibit-startup-screen t)

(add-hook 'prog-mode-hook (lambda () (show-paren-mode 1)))
(setq-default column-number-mode 1)

;;; Windows specific configurations for basic shell functions.
(when wg-system-is-not-unix
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash-args '("--login" "-i"))
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (wg-add-to-exec-and-env "C:/Program Files/Git/usr/bin"))

;;; Use dynamic-bindings variables to re-configure it in more situations.
(defvar wg-prefered-fonts '("cascadia code" "menlo" "consolas" "monospace"))
(defvar wg-font-size 20)

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
(defvar wg-default-frame-alist '((width . 128) (height . 32)))

;;; Functions hooked on `emacs-startup-hook' will only run once.  We can safely
;;; reload this file without calling these functions again.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (dolist (a wg-default-frame-alist)
	      (add-to-list 'default-frame-alist a))
	    (wg-prepare-face)
	    (load-theme-single 'modus-vivendi)
	    ))

(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (message "New client is connected to emacs daemon...")
	    (wg-prepare-face)))

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (with-selected-frame frame
	      (wg-prepare-face))))

;;; Line number is useful, enable it globally on GUI version.
(when window-system
  (setq-default display-line-numbers-width 8)
  (setq-default display-line-numbers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pixel scrolling mode.  (Supported since Emacs 29)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pixel-scroll-precision-mode 1)
;;; Interpolate scrolling via the Page Down and Page Up keys.
(setq pixel-scroll-precision-interpolate-page t)
;;; Make it fluent when bottom got scrolled out of screen and then
;;; scrolled back.
(setq scroll-conservatively 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update package indexes: `M-x' `package-refresh-contents'.
;;; Install new package: `M-x' `package-install'.
;;; Delete installed package: `M-x' `package-delete'.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq url-proxy-services '(("http" . "localhost:7890")
			   ("https" . "localhost:7890")))

(package-initialize)

;;; Install selected packages: `M-x' `package-install-selected-packages'.
(setq package-selected-packages
      '(company magit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IDO mode.  (Included in Emacs since 23.1 (2017))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
;;(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Company (auto complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If I don't need `eglot`, I will use `me` (Modified Micro Emacs) instead.
(require 'eglot)
(define-key eglot-mode-map (kbd "C-c f") #'eglot-format)
(define-key eglot-mode-map (kbd "C-c h") #'eglot-inlay-hints-mode)
(add-hook 'eglot-managed-mode-hook
	  (lambda ()
	    (eglot-inlay-hints-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook (lambda () (c-set-style "linux")))
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c-mode-common-hook #'me-use-normal-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Erlang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (executable-find "erl")
  (add-to-list 'load-path (file-name-concat
			   (shell-command-to-string
			    "erl -noinput -eval 'io:put_chars(code:lib_dir(tools)), halt()'")
			   "emacs"))
  (require 'erlang-start))

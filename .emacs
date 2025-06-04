;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Micro Emacs compatible keybindings
;;; 	(uEmacs/PK, Micro Emacs 3.9e variant).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "M-SPC" #'set-mark-command)
(keymap-global-set "C-z" #'scroll-down-command)
(keymap-global-set "C-M-z" #'scroll-other-window-down)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (cadr (buffer-list))))

(keymap-global-set "M-p" #'switch-to-last-buffer)

;;; `me` (Modified Micro Emacs) use TABs only, we make it default here, too.
(defun wg-use-normal-tab ()
  (keymap-local-set "TAB" (lambda () (interactive) (insert ?\t)))
  (keymap-local-set "DEL" (lambda () (interactive) (backward-delete-char 1)))
  (electric-indent-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous configurations to make Emacs more comfortable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)

;;; Features like Toolbar, Menubar and Scrollbar is not necessary for Emacs.
(mapc (lambda (fn-symbol)
	(and (fboundp fn-symbol) (funcall fn-symbol -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(defconst wg-system-is-not-unix (cl-case system-type
				  ((windows-nt ms-dos cygwin) t)
				  (t nil)))

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

(add-hook 'prog-mode-hook (lambda () (show-paren-mode 1)))
(setq-default column-number-mode 1)

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

;;; Windows specific configurations for basic shell functions.
(when wg-system-is-not-unix
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash-args '("--login" "-i"))
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  (wg-add-to-exec-and-env "C:/Program Files/Git/usr/bin"))

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
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
;;(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Company (auto complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tree-sitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use `M-x' `treesit-install-language-grammar' to install grammars.
(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If I don't need `eglot`, I will use `me` (Modified Micro Emacs) instead.
(require 'eglot)
(define-key eglot-mode-map (kbd "C-c f") 'eglot-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook (lambda () (c-set-style "linux")))

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(add-hook 'c-mode-common-hook #'wg-use-normal-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TypeScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'js-mode-hook #'eglot-ensure)

(add-hook 'typescript-ts-mode-hook #'wg-use-normal-tab)
(add-hook 'js-mode-hook #'wg-use-normal-tab)

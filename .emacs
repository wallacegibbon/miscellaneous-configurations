;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatible keybindings with Modified Micro Emacs (`me`).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keymap-global-set "M-SPC" #'set-mark-command)
(keymap-global-set "C-z" #'scroll-down-command)
(keymap-global-set "C-M-z" #'scroll-other-window-down)
(keymap-global-set "C-x x" #'next-buffer)

(defun me-switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (cadr (buffer-list))))

(keymap-global-set "M-p" #'me-switch-to-last-buffer)

(defun me-newline-and-indent ()
  "Indent use TABs only.  If there are spaces, fill the space with a TAB."
  (interactive)
  (let* ((n-spaces (current-indentation))
	 (n-tabs (/ (+ n-spaces 7) 8)))
    (insert ?\n)
    (dotimes (i n-tabs)
      (insert ?\t))))

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

(setq ring-bell-function #'ignore)
(setq inhibit-startup-screen t)

(add-hook 'prog-mode-hook (lambda () (show-paren-mode 1)))
(setq-default column-number-mode 1)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LLVM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copy the following 3 files from llvm project to `.emacs.d/llvm/`:
;;; - llvm-project/llvm/utils/emacs/llvm-mode.el
;;; - llvm-project/llvm/utils/emacs/tablegen-mode.el
;;; - llvm-project/mlir/utils/emacs/mlir-mode.el
(add-to-list 'load-path "~/.emacs.d/llvm")
(require 'llvm-mode)
(require 'tablegen-mode)
(require 'mlir-mode)

(add-to-list 'auto-mode-alist '("\\.ll\\'" . llvm-mode))
(add-to-list 'auto-mode-alist '("\\.td\\'" . tablegen-mode))
(add-to-list 'auto-mode-alist '("\\.mlir\\'" . mlir-mode))

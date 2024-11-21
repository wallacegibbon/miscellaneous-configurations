(defun add-to-exec-and-env (pathname)
  "Add `pathname' to environment variable PATH and emacs's exec-path. return t when it is added, and nil when it's already in PATH"
  (add-to-list 'exec-path pathname t)
  (let ((env-path (getenv "PATH")))
    (if (string-match-p (regexp-quote pathname) env-path)
        nil
      (setenv "PATH" (concat pathname ":" env-path))
      t)))


(defun surfacepro-p ()
  "Because of the high DPI, Surface Pro needs some special configurations."
  (equal (getenv "SURFACEPRO") "1"))


(defun appropriate-font (font-name)
  "Make valid font string that can be the argument of `set-frame-font'."
  (let ((font-size (if (surfacepro-p) 14 11)))
    (and font-name
         (format "%s-%d" font-name font-size))))


(defun config-non-console-height ()
  (let ((height (if (surfacepro-p) 48 56)))
    (message "Setting height to %d" height)
    (set-frame-height nil height)))


(defun config-non-console-font (&optional font-str)
  (let* ((prefered-fonts '("ubuntu mono" "menlo" "consolas" "monospace"))
         (default-font (seq-find #'x-list-fonts prefered-fonts))
         (font (appropriate-font (or font-str default-font))))
    (message "Setting font to %s" font)
    (set-frame-font font)))


(defun customize-window-system ()
  (config-non-console-font)
  (config-non-console-height)
  (set-frame-width nil 80)
  (set-frame-position nil 585 0)
  (load-theme 'misterioso t))


;;; the elpa
(require 'package)

(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/")
             t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/")
             t)

(package-initialize)


;;; configure the face
(add-hook 'window-setup-hook
          (lambda ()
            (when window-system (customize-window-system))))


(column-number-mode t)
(show-paren-mode t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(display-time)


;; (set-face-attribute 'mode-line nil
;;                     :box nil :foreground "white" :background "blue")
;; (set-face-attribute 'mode-line-inactive nil
;;                     :box nil)


(add-to-exec-and-env "/usr/local/bin")


;;; for magit
(global-set-key (kbd "C-x g") 'magit-status)


;;; CC Mode style fix (based on the gnu style)
(defun alist-update (target-alist pairs)
  "Update the alist TARGET-ALIST with argument PAIRS"
  (dolist (d pairs)
    (setcdr (assoc (car d) target-alist)
            (cdr d))))


;;; change the indent of enum definition and array initialization.
(add-hook 'c-mode-common-hook
          (lambda ()
            (alist-update c-offsets-alist
                          '((brace-list-intro . +)))))

;;; change the indent of struct definition.
(add-hook 'c-mode-hook
          (lambda ()
            (alist-update c-offsets-alist
                          '((class-open . +)
                            (class-close . +)))))


;;; configurations for web
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))


;; the clojure mode will use space rather than tab, fix it
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))


;;; miscellaneous functions
(let ((misc-script "~/.emacs.d/misc.el"))
  (if (file-exists-p misc-script)
      (load-file misc-script)))


;;; Rust
;; (autoload 'rust-mode "rust-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))


;;; Common Lisp
;; (setq inferior-lisp-program "sbcl")
;; (setq slime-contribs '(slime-fancy slime-cl-indent))
;; (require 'slime)
;; (add-hook 'slime-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-j") #'slime-eval-print-last-expression)))


;;; Scheme
;; (setq scheme-program-name "guile")
;; (require 'cmuscheme)


;;; Yasnippet
;; (require 'yasnippet)
;; (setq yas-snippet-dirs (dot-emacs-d-path "yasnippet-snippets/snippets"))
;; (yas-global-mode 1)
;; (yas-reload-all)

;; (dolist (x '(js-mode-hook html-mode-hook c-mode-hook))
;;   (add-hook x #'yas-minor-mode))


;;; Auto complete
;; (require 'auto-complete-config)
;; (ac-config-default)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (lfe-mode clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

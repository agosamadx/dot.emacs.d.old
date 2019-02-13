;;; setting
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq completion-ignore-case t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq kill-whole-line t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq custom-file (locate-user-emacs-file "custom.el"))

;;; theme
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'custom t)

;;; func
(load-file "~/.emacs.d/func.el")

;;; for mac
(when (memq window-system '(mac ns))
  (load-file "~/.emacs.d/mac.el"))

;;; for windows
(when window-system '(windows-nt)
      (load-file "~/.emacs.d/windows.el"))

;;; frame
(defvar frame-parameters
  '((width . 180)
    (height . 50)
    (top . 64)
    (left . 192)
    (tool-bar-lines . nil)))

(when (memq window-system '(x w32 mac ns))
  (set-scroll-bar-mode nil)
  (setq frame-title-format '(multiple-frames "%b" ("" invocation-name)))
  (setq default-frame-alist frame-parameters))

;;; paren-mode(対応する括弧の強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-ce" 'eshell)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-a" 'beggining-of-indented-line)
(global-set-key "\C-k" 'kill-line)
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)))
(add-hook 'electric-buffer-menu-mode-hook
          '(lambda()
             (local-set-key "x" 'Buffer-menu-execute)))

;;; scratch自動復活
(add-hook
 'kill-buffer-query-functions
 (lambda ()
   (if (string= "*scratch*" (buffer-name))
       (progn (make-scratch nil) nil)
     t)))
(add-hook
 'after-save-hook
 (lambda ()
   (unless (member (get-buffer "*scratch*") (buffer-list))
     (make-scratch t))))

;;; c-mode
(add-hook 'c-mode-common-hook
          '(lambda()
             (setq c-default-style "stroustrup")
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (local-set-key "\C-h" 'hungry-backspace)
             (local-set-key "\C-d" 'hungry-delete)
             (local-set-key "\C-f" 'hungry-forward-char)
             (local-set-key "\C-b" 'hungry-backward-char)))

;;; c++-mode
(setq auto-mode-alist
      (append
       '(("\\.h$" . c++-mode)
         ("\\.hpp$"     . c++-mode))
       auto-mode-alist))
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-offset 'innamespace 0)
             (c-set-offset 'inlambda 0)
             (c-set-offset 'arglist-close 0)
             (c-set-offset 'arglist-cont-nonempty 0)))

;;; js-mode
(add-hook 'js-mode-hook
          '(lambda()
             (setq js-indent-level 2)
             (setq tab-width 2)
             (setq indent-tabs-mode nil)
             (local-set-key "\C-h" 'hungry-backspace)
             (local-set-key "\C-d" 'hungry-delete)
             (local-set-key "\C-f" 'hungry-forward-char)
             (local-set-key "\C-b" 'hungry-backward-char)))

;;; white space
(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         space-mark
                         tab-mark
                         empty))
(set-face-attribute 'whitespace-tab nil
                    :background nil
                    :foreground nil
                    :underline nil)
(set-face-attribute 'whitespace-empty nil
                    :background nil)
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(setq whitespace-action '(auto-cleanup))
(add-hook 'c++-mode-hook 'whitespace-mode)
(add-hook 'js2-mode-hook 'whitespace-mode)

;;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-user-dir "~/.emacs.d/packages/")
(package-initialize)

;; install packages
(eval-when-compile (require 'cl))
(defvar package-list '(undo-tree ddskk yasnippet irony flycheck flycheck-irony flycheck-pos-tip company company-irony rtags cmake-ide cmake-mode web-mode js2-mode))
(let ((not-installed
       (loop for x in package-list
             when (not (package-installed-p x))
             collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;;; ddskk
(when (require 'skk nil t)
  (require 'skk-study)
  (setq default-input-method "japanese-skk")
  (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
  (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
  (setq skk-isearch-start-mode 'latin))

;;; redo+
(when (require 'undo-tree nil t)
  (global-undo-tree-mode)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))

;;; web-mode
(when (require 'web-mode nil t)
  (setq auto-mode-alist
        (append
         '(("\\.php$" . web-mode)
           ("\\.tpl$" . web-mode)
           ("\\.html?$" . web-mode))
         auto-mode-alist))
  (add-hook 'web-mode-hook
            '(lambda()
               (setq web-mode-markup-indent-offset 4)
               (setq web-mode-enable-auto-pairing nil)
               (setq web-mode-enable-auto-quoting nil)
               (setq web-mode-script-padding 0)
               (setq web-mode-style-padding 0)
               (setq indent-tabs-mode nil)
               (setq web-mode-enable-auto-indentation nil)
               (local-set-key "\C-h" 'hungry-backspace)
               (local-set-key "\C-d" 'hungry-delete)
               (local-set-key "\C-f" 'hungry-forward-char)
               (local-set-key "\C-b" 'hungry-backward-char))))

(when (require 'js2-mode nil t)
  (setq auto-mode-alist
        (append
         '(("\\.js$" . js2-mode)
           ("\\.json$" . js2-mode)
           ("\\.jsx$" . js2-jsx-mode))
         auto-mode-alist))
  (add-hook 'js2-mode-hook
            '(lambda()
               (setq js2-basic-offset 2)
               (setq tab-width 2)
               (local-set-key "\C-h" 'hungry-backspace)
               (local-set-key "\C-d" 'hungry-delete)
               (local-set-key "\C-f" 'hungry-forward-char)
               (local-set-key "\C-b" 'hungry-backward-char))))

(when (require 'cmake-mode nil t)
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt$" . cmake-mode)
           ("\\.cmake$" . cmake-mode))
         auto-mode-alist)))

;; yasnippet
(when (require 'yasnippet nil t)
  (yas-global-mode))

;; irony
(when (require 'irony nil t)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  ;; flycheck
  (when (require 'flycheck nil t)
    (add-hook 'c-mode-hook 'flycheck-mode)
    (add-hook 'c++-mode-hook 'flycheck-mode)
    (when (require 'flycheck-irony nil t)
      (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
    (when (require 'flycheck-pos-tip nil t)
      (flycheck-pos-tip-mode)))
  ;; company
  (when (require 'company nil t)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (setq company-selection-wrap-around t)
    (add-hook 'c-mode-hook 'company-mode)
    (add-hook 'c++-mode-hook 'company-mode)
    ;;(define-key company-active-map (kbd "C-h") nil))
    (when (require 'company-irony nil t)
      (add-to-list 'company-backends 'company-irony)
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (add-hook 'irony-mode-hook
                '(lambda()
                   (define-key irony-mode-map
                     [remap completion-at-point]
                     'irony-completion-at-point-async)
                   (define-key irony-mode-map
                     [remap complete-symbol]
                     'irony-completion-at-point-async))))))

;; cmake-ide
(let ((rdm (executable-find "rdm"))
      (rc (executable-find "rc")))
  (when (and rdm rc)
    (when (require 'cmake-ide nil t)
      (setq cmake-ide-rdm-executable (executable-find "rdm"))
      (setq cmake-ide-rc-executable  (executable-find "rc"))
      (setq cmake-ide-build-pool-dir "~/tmp/")
      (setq cmake-ide-build-pool-use-persistent-naming t))
    (when (require 'rtags nil t)
      (rtags-enable-standard-keybindings c-mode-base-map)
      (cmake-ide-setup)
      (define-key c++-mode-map "\C-t" 'rtags-find-symbol-at-point)
      (define-key c++-mode-map "\C-cc" 'cmake-ide-compile))))

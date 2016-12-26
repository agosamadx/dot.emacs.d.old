;;; scratch復活
(defun make-scratch (&optional arg)
  (interactive)
  (progn
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

;;; 行頭とインデント末をトグル
(defun beggining-of-indented-line (point)
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          point)))
      (beginning-of-line)
    (back-to-indentation)))

;;; 行頭のスペースをtab-width分ずつ消す
(defun hungry-backspace (arg)
  (interactive "*P")
  (let ((line-point (- (point) (line-beginning-position))))
    (if (string-match
         "^\s+$"
         (buffer-substring-no-properties
          (line-beginning-position)
          (if (< (+ (point) (% line-point tab-width)) (line-end-position))
              (+ (point) (% line-point tab-width))
            (line-end-position))))
        (if (= (% line-point tab-width) 0)
            (backward-delete-char tab-width)
          (backward-delete-char (% line-point tab-width)))
      (backward-delete-char 1))))

;;; スペースが続いている時はtab-widthずつ消す
(defun hungry-delete (arg)
  (interactive "*P")
  (let ((here (point)))
    (skip-chars-forward " \t")
    (if (/= (point) here)
        (delete-region (point) here)
      (delete-char 1))))

;;; スペースが続いている時はタブ幅ごとまとめて移動する
(defun hungry-forward-char (arg)
  (interactive "*P")
  (let* ((line-current-position (- (point) (line-beginning-position)))
         (line-next-by (- tab-width (% line-current-position tab-width)))
         (line-next-position
          (if (< (+ (point) line-next-by) (line-end-position))
              (+ (point) line-next-by)
            (line-end-position)))
         (forward-count
          (if (= (- line-next-position (point)) 0) 1
            (- line-next-position (point)))))
    (if (string-match
         "^\s+$"
         (buffer-substring-no-properties
          (line-beginning-position)
          line-next-position))
        (forward-char forward-count)
      (forward-char 1))))

;;; スペースが続いている時はタブ幅ごとまとめて移動する
(defun hungry-backward-char (arg)
  (interactive "*P")
  (let ((line-current-position (- (point) (line-beginning-position))))
    (if (string-match
         "^\s+$"
         (buffer-substring-no-properties
          (line-beginning-position)
          (point)))
        (if (= (% line-current-position tab-width) 0)
            (backward-char tab-width)
          (backward-char (- tab-width (% line-current-position tab-width))))
      (backward-char 1))))

;;; .settings.el をロードする
(defun recursive-load-dir-settings (currentfile)
  (let ((lds-dir (locate-dominating-file currentfile ".settings.el")))
    (when lds-dir
      (progn
        (load-file (concat lds-dir ".settings.el"))
        (recursive-load-dir-settings
         (file-truename(concat lds-dir "..")))))))
(defun load-dir-settings()
  (interactive)
  (when buffer-file-name
    (recursive-load-dir-settings buffer-file-name)))

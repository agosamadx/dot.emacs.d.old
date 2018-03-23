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
(defun hungry-backspace ()
  (interactive)
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
(defun hungry-delete ()
  (interactive)
  (let ((here (point)))
    (skip-chars-forward " \t")
    (if (/= (point) here)
        (delete-region (point) here)
      (delete-char 1))))

;;; スペースが続いている時はタブ幅ごとまとめて移動する
(defun hungry-forward-char ()
  (interactive)
  (let* ((current (- (point) (line-beginning-position)))
         (step (- tab-width (% current tab-width)))
         (candidate (min (+ (point) step) (line-end-position))))
    (if (and (string-match "^\s+$" (buffer-substring-no-properties (line-beginning-position) candidate))
             (not (= (point) (line-end-position)))
             (= (% (- candidate (line-beginning-position)) tab-width) 0))
        (goto-char candidate)
      (forward-char 1))))

;;; スペースが続いている時はタブ幅ごとまとめて移動する
(defun hungry-backward-char ()
  (interactive)
  (let ((current (- (point) (line-beginning-position))))
    (if (string-match "^\s+$" (buffer-substring-no-properties (line-beginning-position) (point)))
        (if (= (% current tab-width) 0)
            (backward-char tab-width)
          (backward-char (- tab-width (% current tab-width))))
      (backward-char 1))))

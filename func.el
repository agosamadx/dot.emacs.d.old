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
  (let* ((current (- (point) (line-beginning-position)))
         (step (- tab-width (% current tab-width)))
         (nextperiod (min (+ (point) step) (line-end-position))))
    (if (and (string-match "^\s+$" (buffer-substring-no-properties (line-beginning-position) nextperiod))
             (not (= (point) (line-end-position)))
             (= (% (- nextperiod (line-beginning-position)) tab-width) 0))
        (delete-char step)
      (delete-char 1))))

;;; スペースが続いている時はタブ幅ごとまとめて移動する
(defun hungry-forward-char ()
  (interactive)
  (let* ((current (- (point) (line-beginning-position)))
         (nextperiod (min (+ (point) (- tab-width (% current tab-width))) (line-end-position))))
    (if (and (string-match "^\s+$" (buffer-substring-no-properties (line-beginning-position) nextperiod))
             (not (= (point) (line-end-position)))
             (= (% (- nextperiod (line-beginning-position)) tab-width) 0))
        (forward-char (- tab-width (% current tab-width)))
      (forward-char 1))))

;;; スペースが続いている時はタブ幅ごとまとめて移動する
(defun hungry-backward-char ()
  (interactive)
  (backward-char 1)
  (let* ((current (- (point) (line-beginning-position)))
         (nextperiod (min (+ (point) (- tab-width (% current tab-width))) (line-end-position))))
    (when (and (string-match "^\s+$" (buffer-substring-no-properties (line-beginning-position) nextperiod))
               (not (= (point) (line-beginning-position)))
               (= (% (- nextperiod (line-beginning-position)) tab-width) 0))
      (backward-char (% current tab-width)))))

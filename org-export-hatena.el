;;; org-export-hatena.el

(require 'simple-hatena-mode)

(defvar org-export-hatena-decrease-level-num 1 "*org-mode のヘッドラインのレベルを減らす数．

すべてのヘッドラインの * の数をこの変数の数値だけ減らす．")

(defvar org-export-hatena-notation-1st-level "^\\* \\([^:\t\n\r\f]*\\)\\(:?\\([^:\t\n\r\f]+:\\)*\\)$")
(defvar org-export-hatena-notation-2nd-level "^\\*\\* \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-3rd-level "^\\*\\*\\* \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-http-link "\\[\\[\\(https?:[^]\n\r]*\\)\\]\\(\\[\\([^]\n\r]*\\)\\]\\)?\\]")
(defvar org-export-hatena-notation-quote '("[ ]*#\\+BEGIN_QUOTE" ">>" "[ ]*#\\+END_QUOTE" "<<"))
(defvar org-export-hatena-notation-super-pre '("[ ]*#\\+BEGIN_EXAMPLE" ">||" "[ ]*#\\+END_EXAMPLE" "||<"))
(defvar org-export-hatena-notation-src '("[ ]*#\\+BEGIN_SRC[ ]*\\([^ \t\n\r]+\\)" ">|\\1|" "[ ]*#\\+END_SRC" "||<"))

(defun org-export-hatena-decrease-level (num)
  "すべてのヘッドラインの * の数をこの変数の数値だけ減らす．"
  (goto-char (point-min))
  (while (re-search-forward "^\\*.*$" nil t)
    (delete-region (match-beginning 0) (+ (match-beginning 0) num)))
  )

(defun org-export-hatena-replace-one-line ()
  "1行の記法を置き換える．"
  (org-export-hatena-replace-1st-level)
  ;(org-export-hatena-replace-2nd-level)
  ;(org-export-hatena-replace-3rd-level)
  (org-export-hatena-replace-http-link))

(defun org-export-hatena-replace-http-link ()
  ""
  ;; replace http link
  (goto-char (point-min))
  (while (re-search-forward org-export-hatena-notation-http-link nil t)
    (if (null (match-string 3))
        (replace-match "[\\1]")
      (replace-match "[\\1:title=\\3]"))))

(defun org-export-hatena-replace-1st-level ()
  "section 部分をはてな記法に置き換える．"
  ;; replace section
  (goto-char (point-min))
  (while (re-search-forward org-export-hatena-notation-1st-level nil t)
    (replace-match "*t*\\2 \\1"))
  ; delete trailing white spaces
  (skip-chars-backward " \t")
  (delete-region (point) (progn (end-of-line) (point)))
  ; replace tag format from :HOGE: to [HOGE]
  (let ((bound (point)))   ; point of end-of-line
    (beginning-of-line)
    ; assume section doesn't include ":"
    (when (re-search-forward ":" bound t)
      (replace-match "["))
    (while (re-search-forward ":" bound t)
      (replace-match "]["))
    (delete-char -1)
    ; delete [Blog] tag
    (beginning-of-line)
    (if (re-search-forward "\\[Blog\\]" bound t)
        (replace-match ""))))

;; (defun org-export-hatena-replace-2nd-level ()
;;   ""
;;   (goto-char (point-min))
;;   (while (re-search-forward org-export-hatena-notation-2nd-level nil t)
;;     (replace-match "** \\1")))

;; (defun org-export-hatena-replace-3rd-level ()
;;   ""
;;   (goto-char (point-min))
;;   (while (re-search-forward org-export-hatena-notation-3rd-level nil t)
;;     (replace-match "*** \\1")))


(defun org-export-hatena-begin-to-end (notation)
  (goto-char (point-min))
  (while (re-search-forward (nth 0 notation) nil t)
    (replace-match (nth 1 notation)))
  (goto-char (point-min))
  (while (re-search-forward (nth 2 notation) nil t)
    (replace-match (nth 3 notation))))

(defun org-export-hatena (beg end)
  "org-mode のファイルをはてな記法にエクスポートする．"
  (interactive "r")
  (let ((diary (buffer-substring beg end))
	(quote org-export-hatena-notation-quote)
	(super-pre org-export-hatena-notation-super-pre)
	(src org-export-hatena-notation-src))
    (with-temp-buffer
      (pop-to-buffer (current-buffer))
      (insert diary)
      (org-export-hatena-decrease-level org-export-hatena-decrease-level-num)
      (org-export-hatena-begin-to-end quote)
      (org-export-hatena-begin-to-end super-pre)
      (org-export-hatena-begin-to-end src)
      (org-export-hatena-replace-one-line)
      (setq diary (buffer-substring (point-min) (point-max))))
    (simple-hatena simple-hatena-default-id)
    (simple-hatena-mode)
    (goto-char (point-min))
    (newline)
    (insert diary)))

(provide 'org-export-hatena)
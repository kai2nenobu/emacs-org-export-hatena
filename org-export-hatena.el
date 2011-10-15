;;; org-export-hatena.el

(require 'simple-hatena-mode)

(defvar org-export-hatena-notation-section "^\\*\\* \\([^:\t\n\r\f]*\\)\\(:?\\([^:\t\n\r\f]+:\\)*\\)$")
(defvar org-export-hatena-notation-subsection "^\\*\\*\\* \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-subsubsection "^\\*\\*\\*\\* \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-http-link "\\[\\[\\(https?:[^]\n\r]*\\)\\]\\(\\[\\([^]\n\r]*\\)\\]\\)?\\]")
(defvar org-export-hatena-notation-quote '("[ ]*#\\+BEGIN_QUOTE" ">>" "[ ]*#\\+END_QUOTE" "<<"))
(defvar org-export-hatena-notation-super-pre '("[ ]*#\\+BEGIN_EXAMPLE" ">||" "[ ]*#\\+END_EXAMPLE" "||<"))
(defvar org-export-hatena-notation-src '("[ ]*#\\+BEGIN_SRC[ ]*\\([^ \t\n\r]+\\)" ">|\\1|" "[ ]*#\\+END_SRC" "||<"))

(defun org-export-hatena-section ()
  (let ((section org-export-hatena-notation-section)
	(subsection org-export-hatena-notation-subsection)
	(subsubsection org-export-hatena-notation-subsubsection)
        (http-link org-export-hatena-notation-http-link))
    ;; replace section
    (goto-char (point-min))
    (while (re-search-forward section nil t)
      (replace-match "*t*\\2 \\1"))
    ; delete trailing white spaces
    (skip-chars-backward " \t")
    (delete-region (point) (progn (end-of-line) (point)))
    ; replace tag format from :HOGE: to [HOGE]
    (let ((bound (point)))               ; point of end-of-line
      (beginning-of-line)
      ; assume section doesn't include ":"
      (when (re-search-forward ":" bound t)
        (replace-match "["))
      (while (re-search-forward ":" bound t)
        (replace-match "]["))
      (delete-backward-char 1)
      (beginning-of-line)
      ; delete [Blog] tag
      (if (re-search-forward "\\[Blog\\]" bound t)
          (replace-match "")))
    ;; replace subsection
    (goto-char (point-min))
    (while (re-search-forward subsection nil t)
      (replace-match "** \\1"))
    ;; replace subsubsection
    (goto-char (point-min))
    (while (re-search-forward subsubsection nil t)
      (replace-match "*** \\1"))
    ;; replace http link
    (goto-char (point-min))
    (while (re-search-forward http-link nil t)
      (if (null (match-string 3))
          (replace-match "[\\1]")
        (replace-match "[\\1:title=\\3]")))
    ))

(defun org-export-hatena-begin-to-end (notation)
  (goto-char (point-min))
  (while (re-search-forward (nth 0 notation) nil t)
    (replace-match (nth 1 notation)))
  (goto-char (point-min))
  (while (re-search-forward (nth 2 notation) nil t)
    (replace-match (nth 3 notation))))

(defun org-export-hatena (beg end)
  (interactive "r")
  (let ((diary (buffer-substring beg end))
	(quote org-export-hatena-notation-quote)
	(s-pre org-export-hatena-notation-super-pre)
	(src org-export-hatena-notation-src))
    (with-temp-buffer
      (pop-to-buffer (current-buffer))
      (insert diary)
      (org-export-hatena-begin-to-end quote)
      (org-export-hatena-begin-to-end s-pre)
      (org-export-hatena-begin-to-end src)
      (org-export-hatena-section)
      (setq diary (buffer-substring (point-min) (point-max))))
    (simple-hatena simple-hatena-default-id)
    (simple-hatena-mode)
    (goto-char (point-min))
    (newline)
    (insert diary)))

(provide 'org-export-hatena)
;;; org-export-hatena.el

;; Author: Tsunenobu KAI
;; Keywords: Org-mode Hatena
;; URL: https://github.com/kbkbkbkb1/emacs-org-export-hatena

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'org)
(require 'simple-hatena-mode)

(defvar org-export-hatena-decrease-level-num 1 "*Org-mode の見出しのレベルを減らす数．

すべての見出しの * の数をこの変数の数値だけ減らす．")

(defvar org-export-hatena-notation-1st-level
  "^\\* \\([^:\t\n\r\f]*\\)\\(:?\\([^:\t\n\r\f]+:\\)*\\)$")
(defvar org-export-hatena-notation-2nd-level
  "^\\*\\* \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-3rd-level
  "^\\*\\*\\* \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-http-link
  "\\[\\[\\(https?:[^]\n\r]*\\)\\]\\(\\[\\([^]\n\r]*\\)\\]\\)?\\]")
(defvar org-export-hatena-notation-quote
  '("^\\s-*#\\+BEGIN_QUOTE" ">>" "^\\s-*#\\+END_QUOTE" "<<"))
(defvar org-export-hatena-notation-super-pre
  '("^\\s-*#\\+BEGIN_EXAMPLE" ">||" "^\\s-*#\\+END_EXAMPLE" "||<"))
(defvar org-export-hatena-notation-src
  '("^\\s-*#\\+BEGIN_SRC[ ]*\\([^ \t\n\r]+\\)" ">|\\1|" "^\\s-*#\\+END_SRC" "||<"))
(defvar org-export-hatena-notation-property-start
  "^\\s-*:PROPERTIES:.*$")
(defvar org-export-hatena-notation-property-end
  "^\\s-*:END:[ \t]*\n")
(defvar org-export-hatena-point-register nil)
(defvar org-export-hatena-1st-level-headline nil)
(defvar org-export-hatena-title nil)
(defvar org-export-hatena-post-date nil)
(defvar org-export-hatena-hatena-date nil)

(defun org-export-hatena-delete-posted-diary ()
  "以前に投稿されている日記を削除する．

1レベルの見出しが変更されていると，以前に投稿されている部分が特定できないので
削除することができない．どうしよう．"
  (when (re-search-forward org-export-hatena-1st-level-headline nil t)
    (org-mark-subtree)
    (delete-region (region-beginning) (region-end))))

(defun org-export-hatena-open-date-file ()
  "`org-export-hatena-hatena-date' に対応する日にちのファイルを開く．"
  (if (string-match "[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]"
                    org-export-hatena-hatena-date)
      (simple-hatena-internal-safe-find-file
       (concat simple-hatena-root "/"
               simple-hatena-default-id
               "/diary/"
               (match-string 0 org-export-hatena-hatena-date)
               ".txt"))
    (error "Invalid date")))

(defun org-export-hatena-read-1st-level-headline ()
  "1レベルの見出しを読み取る．"
  (goto-char (point-min))
  (if (re-search-forward org-export-hatena-notation-1st-level nil t)
      (setq org-export-hatena-1st-level-headline (match-string-no-properties 1))
    (error "1レベルのヘッドラインが存在しません"))
  ;; 余分な空白文字を削除
  (string-match "[ \t]*$" org-export-hatena-1st-level-headline)
  (setq org-export-hatena-1st-level-headline
        (replace-match "" nil nil org-export-hatena-1st-level-headline)))

(defun org-export-hatena-read-property ()
  "Org-mode 日記のプロパティを読み取る．"
  (goto-char (point-min))
  (let ((beg (when (re-search-forward
                    org-export-hatena-notation-property-start nil t)
               (match-beginning 0)))
        (end (when (re-search-forward
                    org-export-hatena-notation-property-end nil t)
               (match-end 0))))
    (if (and beg end)
        ;; プロパティ領域があるとき
        (progn
          (goto-char beg)
          (setq org-export-hatena-title (org-entry-get nil "HatenaTitle" t))
          (setq org-export-hatena-hatena-date (org-entry-get nil "HatenaDate" t))
          (setq org-export-hatena-post-date (org-entry-get nil "PostDate"))
          (delete-region beg end))
      ;; プロパティ領域がないとき
      (setq org-export-hatena-title nil)
      (setq org-export-hatena-hatena-date nil)
      (setq org-export-hatena-post-date nil))
    (unless org-export-hatena-hatena-date
      (setq org-export-hatena-hatena-date (format-time-string "[%Y-%m-%d %a]")))
    ))

(defun org-export-hatena-replace-title ()
  ""
  (goto-char (point-min))
  (delete-region (point) (progn (forward-line 1) (point)))
  (insert (concat (if org-export-hatena-title
                      org-export-hatena-title "") "\n")))

(defun org-export-hatena-decrease-level (num)
  "すべてのヘッドラインの * の数をこの変数の数値だけ減らす．"
  (goto-char (point-min))
  (while (re-search-forward "^\\*.*$" nil t)
    (delete-region (match-beginning 0) (+ (match-beginning 0) num)))
  )

(defun org-export-hatena-replace-one-line ()
  "1行の記法を置換する．"
  (org-export-hatena-replace-1st-level)
  ;(org-export-hatena-replace-2nd-level)
  ;(org-export-hatena-replace-3rd-level)
  (org-export-hatena-replace-http-link))

(defun org-export-hatena-replace-http-link ()
  "http のリンクを置換する"
  (goto-char (point-min))
  (while (re-search-forward org-export-hatena-notation-http-link nil t)
    (if (null (match-string 3))
        (replace-match "[\\1]")
      (replace-match "[\\1:title=\\3]"))))

(defun org-export-hatena-replace-1st-level ()
  "1レベルのヘッドラインをはてな記法に置換する．"
  (goto-char (point-min))
  (if (re-search-forward org-export-hatena-notation-1st-level nil t)
      (replace-match "*t*\\2 \\1")
    (error "1レベルのヘッドラインが存在しません"))
  ; delete trailing white spaces
  (skip-chars-backward " \t")
  (delete-region (point) (point-at-eol))
  ; replace tag format from :HOGE: to [HOGE]
  (let ((bound (point)))   ; point of end-of-line
    (beginning-of-line)
    ; assume headline doesn't include ":" except tags
    (when (re-search-forward ":" bound t)
      (replace-match "["))
    (while (re-search-forward ":" bound t)
      (replace-match "]["))
    (delete-char -1)
    ; delete [Blog] tag
    (beginning-of-line)
    (when (re-search-forward "\\[Blog\\]" bound t)
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

(defun org-export-hatena-rewrite-property ()
  "Org-mode 日記のプロパティを書き換える．

simple-hatena-after-submit-hook に引っかけて HatenaDate と PostDate を
書き換える．"
  (when (get-register 'org-export-hatena-point-register)
    (jump-to-register 'org-export-hatena-point-register)
    (org-set-property "HatenaDate" org-export-hatena-hatena-date)
    (org-set-property "PostDate" (format-time-string "[%Y-%m-%d %a %R]"))
    (set-register 'org-export-hatena-point-register nil)))

(defun org-export-hatena (beg end)
  "Org-mode のファイルをはてな記法にエクスポートする．"
  (interactive "r")
  ;; Org-mode 日記の位置を覚えておく
  (goto-char beg)
  (point-to-register 'org-export-hatena-point-register)
  (let ((diary (buffer-substring beg end)))
    ;; Org-mode 記事をはてな記法に変換
    (with-temp-buffer
      (pop-to-buffer (current-buffer))
      (insert diary)
      (org-export-hatena-decrease-level org-export-hatena-decrease-level-num)
      (org-export-hatena-read-property)
      (org-export-hatena-read-1st-level-headline)
      (org-export-hatena-begin-to-end org-export-hatena-notation-quote)
      (org-export-hatena-begin-to-end org-export-hatena-notation-super-pre)
      (org-export-hatena-begin-to-end org-export-hatena-notation-src)
      (org-export-hatena-replace-one-line)
      (setq diary (buffer-substring (point-min) (point-max))))
    ;; open simple-hatena-mode buffer according to HatenaDate
    (org-export-hatena-open-date-file)
    (simple-hatena-mode)
    (org-export-hatena-replace-title)
    (when org-export-hatena-post-date
      (org-export-hatena-delete-posted-diary))
    (insert diary)))

(add-hook 'simple-hatena-after-submit-hook 'org-export-hatena-rewrite-property)

(provide 'org-export-hatena)
;;; org-export-hatena.el ends here
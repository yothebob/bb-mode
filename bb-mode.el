;;; bb-mode.el --- This is a place where I have fun writing lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Brandon Brodrick

;; Author: Brandon Brodrick <bbrodrick@parthenonsoftware.com>
;; Keywords: tools, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;; to add to emacs config
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/bb-mode"))
;;(require 'bb-mode)

;;; Code:
(require 'bongo)
(require 'vterm)

(defun bb-goto-paren ()
  "Using 'show-paren-mode', Find matching parenthese and jump to."
  (interactive)
    (if (equal (show-paren--default) nil)
	(progn
	  (forward-char)
	  (if (equal (show-paren--default) nil)
	      (progn (backward-char 2)))
	  (goto-char (nth 2 (show-paren--default))))
      (goto-char (nth 2 (show-paren--default)))))
(global-set-key (kbd "C-c j p") 'bb-goto-paren)

(defun bb-dired-do-eshell-command ()
  "Run an Eshell COMMAND on the marked files."
  (interactive)
  (let ((files (dired-get-marked-files t)) (command ""))
    (setq command (read-string "command?: "))
    (eshell-command
     (format "%s %s" command (mapconcat #'identity files " ")))))
;; M-x dired-do-eshell-command RET grep -nH --color your-search--pattern RET

(defun bb-do-eshell-command ()
  "Run an Eshell command on the marked files."
  (interactive)
  (let (command)
    (set-buffer (current-buffer))
    (setq command (read-string "Eshell Command:" nil nil nil))
  (eshell-command
   (format "%s" command))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key [(control shift up)]  'move-line-up)

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control shift down)]  'move-line-down)

(defun duplicate-line()
  "Duplicate current cursor line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(defun duplicate-start-of-line-or-region (&optional args)
  "Duplicate line or region (if ARGS not nil) depending on mark active."
  (interactive "p")
  (if (equal args nil)
      (if mark-active
	  (duplicate-region)
	(duplicate-line))
    (let (x)
      (progn
	(setq x 0)
	(while (< x args)
      (if mark-active
	  (duplicate-region)
	(duplicate-line))
      (setq x (+ x 1)))))))
(global-set-key [(control shift d)]  'duplicate-start-of-line-or-region)

(defun duplicate-region ()
  "Duplicate a region of marked text."
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-'") 'comment-or-uncomment-region-or-line)

(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))
(global-set-key (kbd "C-c s") 'xah-search-current-word)

(defun bb-better-kill-line (&optional arg)
  "Allways kill the whole line, now with universal operator ARG."
  (interactive "p")
  (if (not (equal arg nil))
      (let (x)
	(setq x 0)
	(while (< x arg)
	(progn
	  (move-beginning-of-line 1)
	  (kill-line 1)
	  (setq x (+ x 1)))))
    (progn
      (move-beginning-of-line 1)
     (kill-line 1))))
(global-set-key (kbd "C-k") 'bb-better-kill-line)

(defun bb-better-kill-word()
  "Allways kill the whole word."
  (interactive)
  (forward-word)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "M-d") 'bb-better-kill-word)

(defun bb-kill-string()
  "Kill anything in quotes."
  (interactive)
  (let (mpos)
    (skip-chars-backward "-_A-Za-z 0-9?/\\|.,;:}{+=!@#><$%&*)(^")
    (setq mpos (point))
    (skip-chars-forward "-_A-Za-z 0-9?/\\|.,;:}{+=!@#$><%&*)(^")
    (push-mark mpos)
    (setq mark-active t)
    (kill-region (region-beginning) (region-end))))
(global-set-key (kbd "C-c k s") 'bb-kill-string)

(defun bb-copy-word-on-cursor()
  "Copy word on cursor."
  (interactive)
  (let (mpos)
    (skip-chars-backward "-_A-Za-z.0-9")
    (setq mpos (point))
    (skip-chars-forward "-_A-Za-z.0-9")
    (push-mark mpos)
    (setq mark-active t)
    (copy-region-as-kill (region-beginning) (region-end))
    (setq mark-active nil)))
(global-set-key (kbd "C-c c") 'bb-copy-word-on-cursor)

(defun bb-kill-word-on-cursor()
  "Kill entire word on cursor AKA:better \\M-\\d."
  (interactive)
  (let (mpos)
    (skip-chars-backward "-_A-Za-z.0-9")
    (setq mpos (point))
    (skip-chars-forward "-_A-Za-z.0-9")
    (push-mark mpos)
    (setq mark-active t)
    (kill-region (region-beginning) (region-end))
    (setq mark-active nil)))
(global-set-key (kbd "C-c d") 'bb-kill-word-on-cursor)

(defun bb-flip-boolean (&optional manual-boolean)
  "Insert an inverse boolean statement based off MANUAL-BOOLEAN supplied."
  (let (bol)
    (setq bol (thing-at-point 'word))
    (skip-chars-forward "-_A-Za-z0-9")
    (cond
     ((null bol) (insert "false"))
     ((string= bol "no") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "yes"))))
     ((string= bol "No") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "Yes"))))
     ((string= bol "yes") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "no"))))
     ((string= bol "Yes") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "No"))))
     ((string= bol "true") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "false"))))
     ((string= bol "True") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "False"))))
     ((string= bol "false") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "true"))))
     ((string= bol "False") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "True")))))
    ))

(defun bb-flip-boolean-interactive (&optional arg)
  "Flip or generate boolean on cursor, accepts universal ARG to allow for flipping into a new boolean type (false to True))."
  (interactive "p")
  (if (not (equal arg nil))
      (let (x)
	(setq x 0)
	(while (< x arg)
	  (cond
	   ((= x 2) (bb-flip-boolean "False"))
	   ((= x 3) (bb-flip-boolean "True"))
	   (t (bb-flip-boolean)))
	  (setq x (+ x 1))))
    (bb-flip-boolean)))
(global-set-key (kbd "C-c f b") 'bb-flip-boolean-interactive)

(defun bb-goto-forward-empty-parenthese (&optional args)
  "Go to next (), accepts universal ARGS to go forward multiple."
  (interactive "p")
  (if (equal args nil)
      (progn (search-forward "()") (left-char 1))
    (let (x)
      (setq x 0)
      (while (< x args)
	(progn
	  (search-forward "()")
	  (left-char 1)
	  (setq x (+ x 1)))))))
(global-set-key (kbd "C-c 0") 'bb-goto-forward-empty-parenthese)

(defun bb-goto-backwards-empty-parenthese (&optional args)
  "Go to previous (), accepts universal ARGS to go forward multiple."
  (interactive "p")
  (if (equal args nil)
      (progn (search-backward "()") (right-char 1))
    (let (x)
      (setq x 0)
      (while (< x args)
	(progn
	  (search-backward "()")
	  (right-char 1)
	  (setq x (+ x 1)))))))
(global-set-key (kbd "C-c 9") 'bb-goto-backwards-empty-parenthese)

(defun bb-wrap-at-region (befr aftr)
  "Wrap region with whatever called out with BEFR and AFTR."
  (interactive)
  (let ( $p1 $p2)
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (goto-char $p1)
    (insert befr)
    (goto-char $p2)
    (if (> (length befr) 1)
	(dotimes (_ (- (length befr) 1)) (right-char)))
    (right-char)
    (insert aftr)))

(defun bb-wrap-at-region-with-tag (tag-name)
  "Wrap region with whatever called out with TAG-NAME but turn them into xml tags."
  (interactive)
  (let ( $p1 $p2 start-tag end-tag)
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (goto-char $p1)
    (setq start-tag (format "<%s>" tag-name))
    (setq end-tag (format "</%s>" tag-name))
    (insert start-tag)
    (goto-char $p2)
    (if (> (length start-tag) 1)
	(dotimes (_ (- (length start-tag) 1)) (right-char)))
    (right-char)
    (insert end-tag)))

(defun bb-stringify-word()
  "Add double quotes around word on cursor."
  (interactive)
  (bb-wrap-at-region "\"" "\""))
(global-set-key (kbd "C-c w w") 'bb-stringify-word)

(defun bb-message-pop ()
  "A function to copy or insert the last message that appeared in *Messages* (good for shell commands and etc)."
  (interactive)
  (let (message-test picked-option)
    (setq picked-option 'kill-new)
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (line-move -1)
    (setq message-test (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (funcall picked-option message-test)))
(global-set-key (kbd "C-c m p") 'bb-message-pop)

(defun bb-message-pop-filename ()
  "A function for getting the current buffer path + filename and popping it into kill ring."
  (interactive)
  (message (buffer-file-name))
  (kill-new (buffer-file-name)))
(global-set-key (kbd "C-c m f") 'bb-message-pop-filename)

(defun bb-wrap-word ()
  "Wrap word/region with char of choice."
  (interactive)
  (let (insq)
  (setq insq (read-string "Wrapping Char? \"(\":" nil nil "("))
  (if (string-match insq "(${{})][")
    (progn
      (cond ((string= insq "{") (bb-wrap-at-region "{" "}")) ;; todo, make it so if any parenthese is in the list give wrap region the parenthese reverse
	    ((string= insq "${") (bb-wrap-at-region "${" "}"))
	    ((string= insq "(") (bb-wrap-at-region "(" ")"))
	    ((string= insq "[") (bb-wrap-at-region "[" "]"))
	    ((string= insq "}") (bb-wrap-at-region "{" "}"))
	    ((string= insq ")") (bb-wrap-at-region "(" ")"))
	    ((string= insq "]") (bb-wrap-at-region "[" "]"))
	    ))
    (if (cl-search "<<" insq) (bb-wrap-at-region-with-tag (nth 1 (split-string insq "<<")))(progn (bb-wrap-at-region insq insq))))))
(global-set-key (kbd "C-c w s") 'bb-wrap-word)

(defun bb-smart-highlight-in-parenthese ()
"Mark text in parenthese, find the matching parenthese from the first found left side one
\(by going backwards from cursor position).  TODO count dups and skip over."
  (interactive)
  (let (p1 p1-char p2)
    (skip-chars-backward "-_A-Za-z 0-9?/\\|'\"`.,;:}+=!@#><$%&*)^]")
    (setq p1-char (string (char-before)))
    (char-before 1)
    (setq p1 (point))
    (cond
     ((string= "(" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|'\\\".`,;:{~+=!@#>]<$(%&*}^[") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t)))
     ((string= "{" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|'\\\".`,;:{~+=!@#>]<$(%&*)^[") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t)))
     ((string= "[" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|'\\\".`,;:{~+=!@#>)(<$%&*}^[") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t))))))
(global-set-key (kbd "C-c h p") 'bb-smart-highlight-in-parenthese)

(defun bb-smart-highlight-string ()
  "Mark text in quotations.  TODO count dups and skip over."
  (interactive)
  (let (p1 p1-char p2)
    (skip-chars-backward "-_A-Za-z 0-9?/\\|.,;:}{+=!@#><$%&*)(^][")
    (setq p1-char (string (char-before)))
    (char-before 1)
    (setq p1 (point))
    (cond
    ((string= "\"" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)~\\'`(^][") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t)))
    ((string= "'" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)~\\\"`(^][") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t)))
    ((string= "`" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)'~\\\"(^][") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t))))))
(global-set-key (kbd "C-c h s") 'bb-smart-highlight-string)

(defun bb-remove-quotations ()
  "Remove wrapping quotations."
  (interactive)
  (skip-chars-backward "-_A-Za-z 0-9?/|.,~;:}{+=!@#><$%&\\*)(^][")
  (backward-char 1)
  (delete-char 1)
  (skip-chars-forward "-_A-Za-z 0-9?/|.,;:}{~+=!@#><$%&*)(\\^][")
  (delete-char 1))
(global-set-key (kbd "C-c r q") 'bb-remove-quotations)

(defun bb-remove-parenthese ()
  "Remove wrapping parenthese."
  (interactive)
  (skip-chars-backward "-_A-Za-z 0-9?/|`'\\\".,;:~+=!@#><$%&*^")
  (backward-char 1)
  (delete-char 1)
  (skip-chars-forward "-_A-Za-z 0-9?/|`'\\\".,;:~+=!@#><({[$%&*^")
  (delete-char 1))
(global-set-key (kbd "C-c r p") 'bb-remove-parenthese)

(defun bb-replace-quotations (x)
  "This function will be for 'finding'/'replacing' a surrounding quotation with either (\",',`)
   I think I want this to work without user input (like flip boolean)
   order: \" > ' > ` :WRAP" 
  (interactive "P")
  (let (to-replace start-point)
    (skip-chars-backward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)(^][")
    (setq to-replace (char-before))
    (setq start-point (- (point) 1))
    (progn
      (cond
       ((= to-replace 34) (progn
			    (goto-char start-point)
			    (delete-char 1)
			    (insert "'")
			    (skip-chars-forward "-_A-Za-z 0-9?/|\\.'`,;:}{~+=!@#><$%&*)(^][")
			    (delete-char 1)
			    (insert "'"))) ;; "
       ((= to-replace 39) (progn
 			    (goto-char start-point)
			    (delete-char 1)
			    (insert "`")
			    (skip-chars-forward "-_A-Za-z 0-9?/|.`\\\",;:}{~+=!@#><$%&*)(^][")
			    (delete-char 1)
			    (insert "`"))) ;; '
       ((= to-replace 96) (progn
			    (goto-char start-point)
			    (delete-char 1)
			    (insert "\"")
			    (skip-chars-forward "-_A-Za-z 0-9?/|.'\\\",;:}{~+=!@#><$%&*)(^][")
			    (delete-char 1)
			    (insert "\""))))) ;; `
    (backward-char 1)))
(global-set-key (kbd "C-c f q") 'bb-replace-quotations)

(defun bb-replace-parenthese (x)
  "This function will be for 'finding'/'replacing' a surrounding parenthese with either ((),[],{})
   I think I want this to work without user input (like flip boolean)
   order: () > [] > {} :WRAP" 
  (interactive "P")
  (let (to-replace start-point paren-points)
    (skip-chars-backward "-_A-Za-z 0-9?/|\"'`.,;:+=!@#><$%&*^")
    (setq to-replace (char-before))
    (setq start-point (- (point) 1))
    (progn
      (forward-char 1)
      (setq paren-points (show-paren--default))
      (cond
       ((= to-replace 40) (progn
			    (goto-char start-point)
			    (delete-char 1)
			    (insert "[")
			    ;; (goto-char (nth 2 paren-points))
			    (skip-chars-forward "-_A-Za-z 0-9?/|\\\"'`.,;:~+=!@#><$%&*^][}{")
			    (delete-char 1)
			    (insert "]"))) ;; ()
       ((= to-replace 91) (progn
 			    (goto-char start-point)
			    (delete-char 1)
			    (insert "{")
			    ;; (goto-char (nth 2 paren-points))
			    (skip-chars-forward "-_A-Za-z 0-9?/|\\\"'`.,;:~+=!@#><$%&*^)(}{")
			    (delete-char 1)
			    (insert "}"))) ;; []
       ((= to-replace 123) (progn
			    (goto-char start-point)
			    (delete-char 1)
			    (insert "(")
			    ;; (goto-char (nth 2 paren-points))
			    (skip-chars-forward "-_A-Za-z 0-9?/|\\\"'`.,;:~+=!@#><$%&*^)(][")
			    (delete-char 1)
			    (insert ")"))))) ;; {}
    (backward-char 1)))
(global-set-key (kbd "C-c f p") 'bb-replace-parenthese)

(setq bb-ansi-last-saved-buffer "")

(defun bb-switch-create-ansi ()
  "This is a function for switching between ansi-term buffers, if none exist create one."
  (interactive)
  (if (not (cl-search "ansi-term" (buffer-name)))
      (setq bb-ansi-last-saved-buffer (buffer-name)))
  
  (let (max-ansi-term current-ansi-term)
    (if (cl-search "ansi-term" (buffer-name))
	(cond
	 ((cl-search "<5>" (buffer-name)) (setq current-ansi-term 5))
	 ((cl-search "<4>" (buffer-name)) (setq current-ansi-term 4))
	 ((cl-search "<3>" (buffer-name)) (setq current-ansi-term 3))
	 ((cl-search "<2>" (buffer-name)) (setq current-ansi-term 2))
	 ((equal "*ansi-term*" (buffer-name)) (setq current-ansi-term 1))
	 )
      (setq current-ansi-term 0))
    
    (setq max-ansi-term 0)
    (cond
     ((get-buffer "*ansi-term*<5>") (progn
				      (setq max-ansi-term 5)
				      (if (< current-ansi-term max-ansi-term)
					  (if (equal current-ansi-term 0)
					      (if (get-buffer "*ansi-term*") (switch-to-buffer "*ansi-term*") (ansi-term "/usr/bin/fish"))
					    (switch-to-buffer (format "*ansi-term*<%s>" (+ current-ansi-term 1))))
					(if (not (string= bb-ansi-last-saved-buffer "")) (switch-to-buffer bb-ansi-last-saved-buffer) (switch-to-buffer "*ansi-term*"))
					)
				      ))
     ((get-buffer "*ansi-term*<4>") (progn
				      (setq max-ansi-term 4)
				      (if (< current-ansi-term max-ansi-term)
					  (if (equal current-ansi-term 0)
					      (if (get-buffer "*ansi-term*") (switch-to-buffer "*ansi-term*") (ansi-term "/usr/bin/fish"))
					    (switch-to-buffer (format "*ansi-term*<%s>" (+ current-ansi-term 1))))
					(if (not (string= bb-ansi-last-saved-buffer "")) (switch-to-buffer bb-ansi-last-saved-buffer) (switch-to-buffer "*ansi-term*")))
				      ))
     ((get-buffer "*ansi-term*<3>") (progn
				      (setq max-ansi-term 3)
				      (if (< current-ansi-term max-ansi-term)
					  (if (equal current-ansi-term 0)
					      (if (get-buffer "*ansi-term*") (switch-to-buffer "*ansi-term*") (ansi-term "/usr/bin/fish"))
					    (switch-to-buffer (format "*ansi-term*<%s>" (+ current-ansi-term 1))))
					(if (not (string= bb-ansi-last-saved-buffer "")) (switch-to-buffer bb-ansi-last-saved-buffer) (switch-to-buffer "*ansi-term*")))
				      ))
     ((get-buffer "*ansi-term*<2>") (progn
				      (setq max-ansi-term 2)
				      (if (< current-ansi-term max-ansi-term) 
					  (if (equal current-ansi-term 0)
					      (if (get-buffer "*ansi-term*") (switch-to-buffer "*ansi-term*") (ansi-term "/usr/bin/fish"))
					    (switch-to-buffer "*ansi-term*<2>"))
					(if (not (string= bb-ansi-last-saved-buffer "")) (switch-to-buffer bb-ansi-last-saved-buffer) (switch-to-buffer "*ansi-term*")))
				      ))
     ((get-buffer "*ansi-term*") (progn
				   (setq max-ansi-term 1)
				   (if
				       (and (string= (buffer-name) "*ansi-term*") (not (string= bb-ansi-last-saved-buffer "")))
				       (switch-to-buffer bb-ansi-last-saved-buffer) (switch-to-buffer "*ansi-term*"))))
     ((equal max-ansi-term 0) (ansi-term "/usr/bin/fish")))))
(global-set-key (kbd "C-c a s") 'bb-switch-create-ansi) 


(setq *current-vterm-buff* 0)

(defun bb-switch-vterm ()
  "Get a list of vterm buffers, cycle though them."
  (interactive)
  (let (vterm-buffers)
    (setq vterm-buffers (sort (buffer-query "select where vterm in mode") #'string>))
    (if (>= *current-vterm-buff* (length vterm-buffers))
	(progn
	  (switch-to-buffer (nth 0 vterm-buffers))
	  (setq *current-vterm-buff* 1))
      (progn
	(switch-to-buffer (nth *current-vterm-buff* vterm-buffers))
	(setq *current-vterm-buff* (+ *current-vterm-buff* 1))))))
(global-set-key (kbd "C-c v s") 'bb-switch-vterm)



(defun bb-increment-number-at-point ()
  "Increment number at point https://www.emacswiki.org/emacs/IncrementNumber."
  (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun bb-increment-number-at-point-universal (&optional arg)
  "A universal ARG compliant wrapper for bb-increment-number-at-point."
  (interactive "p")
  (if (not (equal arg nil))
      (let (x)
	(progn
	  (setq x 0)
	  (while (< x arg)
	    (bb-increment-number-at-point)
	    (setq x (+ x 1)))))
    (bb-increment-number-at-point)))
(global-set-key (kbd "C-c n i") 'bb-increment-number-at-point-universal)

(defun bb-decrement-number-at-point ()
  "Increment number at point https://www.emacswiki.org/emacs/IncrementNumber."
  (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun bb-decrement-number-at-point-universal (&optional arg)
  "A universal ARG compliant wrapper for bb-increment-number-at-point."
  (interactive "p")
  (if (not (equal arg nil))
      (let (x)
	(progn
	  (setq x 0)
	  (while (< x arg)
	    (bb-decrement-number-at-point)
	    (setq x (+ x 1)))))
    (bb-decrement-number-at-point)))
(global-set-key (kbd "C-c n d") 'bb-decrement-number-at-point-universal)

(defun bb-create-print-statement ()
  "Add a print statement inserting variable on point, gives you custom print per mode type."
  (interactive)
  (let (buffer-type-mode p1 p2 (print-word ""))
    (setq buffer-type-mode (buffer-local-value 'major-mode (current-buffer)))
    (if (use-region-p)
	(progn
	  (setq p1 (region-beginning))
	  (setq p2 (region-end))
	  (setq print-word (buffer-substring-no-properties p1 p2)))
      (save-excursion
        (skip-chars-backward "-$_A-Za-z0-9")
        (setq p1 (point))
        (right-char)
        (skip-chars-forward "-$_A-Za-z0-9")
        (setq p2 (point))
	(setq print-word (buffer-substring-no-properties p1 p2))))
    (setq mark-active nil)
    (cond
     ((string= buffer-type-mode "emacs-lisp-mode") (progn (back-to-indentation)(next-line 1)(open-line 1)(insert (format "(message \"%s\" %s)" "%s" print-word)) (backward-char 1)))
     ((string= buffer-type-mode "lisp-interaction-mode") (progn (back-to-indentation)(next-line 1)(open-line 1)(insert (format "(message \"%s\" %s)" "%s" print-word)) (backward-char 1)))
     ((string= buffer-type-mode "python-mode") (progn (back-to-indentation)(next-line 1)(open-line 1)(insert (format "print(\"%s:\",%s)" print-word print-word)) (backward-char 1)))
     ((string= buffer-type-mode "web-mode") (progn (back-to-indentation)(next-line 1)(open-line 1)(insert (format "console.log(\"%s:\",%s);" print-word print-word)) (backward-char 2)))
     ((string= buffer-type-mode "javascript-mode") (progn (back-to-indentation)(next-line 1)(open-line 1)(insert (format "console.log(\"%s:\",%s);" print-word print-word)) (backward-char 2)))
     ((string= buffer-type-mode "php-mode") (progn (back-to-indentation)(next-line 1)(open-line 1)(insert (format "file_put_contents(\"/tmp/test.txt\", print_r(%s,true), FILE_APPEND);" print-word)) (backward-char 21))))))
(global-set-key (kbd "C-c p t") 'bb-create-print-statement)

;; search for .. (dired mode)
(global-set-key (kbd "C-.")
  (lambda ()
    (interactive)
    (search-backward "..")))

;; launch Term
(global-set-key (kbd "s-<return>")
(lambda ()
  (interactive)
  (vterm)))

(defun bb-make-named-vterm ()
  "A function for making a custom named Vterm instance."
  (interactive)
  (vterm (read-string "Terminal Name:")))
(global-set-key (kbd "C-c v t") 'bb-make-named-vterm)

;; launch ansi term
(global-set-key (kbd "s-S-<return>")
(lambda ()
  (interactive)
  (ansi-term "/usr/bin/fish")))

(setq playing 'false)

(defun bb-global-play-Music()
  "Globally Play/Pause music."
  (interactive)
  (cond ((eq playing 'true) (shell-command "playerctl pause") (setq playing 'false) (kill-buffer "*Shell Command Output*"))
	((eq playing 'false) (shell-command "playerctl play") (setq playing 'true) (kill-buffer "*Shell Command Output*"))))

(defun bb-use-sht-music()
  "Update music player to use simple home theater."
  (progn
  (vterm "Music")
  (global-set-key (kbd "C-x p") (lambda () (interactive) (switch-to-buffer "Music")))
  (switch-to-buffer "Music")))

(defun bb-use-bongo-music()
  "Update music player to use bongo."
  (progn
    (bongo)
    (bongo-insert-directory-tree "~/Music/")
    (global-set-key (kbd "C-x p") 'bongo-pause/resume)
    (global-set-key (kbd "C-c b l") 'bongo-library)
    (global-set-key (kbd "C-x n s") 'bongo-next)))

(defun bb-update-music-player (&optional pre-picked)
  "Interactive function to switch music player types, do programmatically with PRE-PICKED."
  (interactive)
  (let ((choices '("b" "g" "s")) picked)
    (if pre-picked
	(setq picked pre-picked) 
      (setq picked (completing-read "Starting Music Player? (b:bongo, g:global, s:sht):" choices)))
    (cond ((string= picked "b") (bb-use-bongo-music))
	  ((string= picked "g") (global-set-key (kbd "C-x p") 'bb-global-play-Music))
	  ((string= picked "s") (bb-use-sht-music)))))

(defun bb-use-eaf (&optional pre-picked)
  "A function for turning off/on emacs application framework (browser, terminal etc).
  If off you eww is used, otherwise that keybinding is replaced with eaf-browser."
  (interactive)
  (let ((choices '("y" "n")) picked)
    (if pre-picked
	(setq picked pre-picked)
    (setq picked (completing-read "enable EAF? (y/n):" choices)))
    (cond
     ((string= picked "y")
      (progn
	(use-package eaf
	  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
	  :custom
	  (eaf-browser-continue-where-left-off t)
	  (eaf-browser-enable-adblocker t)
	  (browse-url-browser-function 'eaf-open-browser)
	  :config
	  (defalias 'browse-web #'eaf-open-browser))
	(require 'eaf-vue-demo)
	(require 'eaf-terminal)
	(require 'eaf)
	(require 'eaf-browser)
	(global-set-key (kbd "C-c e w ") 'eaf-open-browser)
	(global-set-key (kbd "M-s-<return>") 'eaf-open-terminal)))
     ((string= picked "n") (global-set-key (kbd "C-c e w ") 'eww))
     )))

(defun bb-make-todo ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (todo-msg)
      (setq todo-msg (read-string "Todo Message?: " nil nil nil))
      (end-of-line)
      ;; (newline 1)
      (insert (format "TODO: %s" todo-msg))
      (comment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "C-c t o") 'bb-make-todo)

(defun bb-random-string (&optional arg)
  "Add new random string to 'kill-ring', optionally set str length with ARG."
  (interactive)
  (let (res (charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890!@#$%^&|?"))
    (if (equal arg nil)
	(setq arg 10))
  (dotimes (_ arg)
    (setq res (concat res (string (nth (random (length (string-to-list charset)))(string-to-list charset))))))
  (kill-new res)))

(defun bb-transparent ()
  "Make emacs window transparent."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85)))

(defun bb-current-git-branch ()
  "Get current git branch in current working directory."
  (interactive)
  (kill-new (replace-regexp-in-string "\n" "" (shell-command-to-string "git branch --show-current"))))
(global-set-key (kbd "C-c g b") 'bb-current-git-branch)


(provide 'bb-mode)
;;; bb-mode.el ends here

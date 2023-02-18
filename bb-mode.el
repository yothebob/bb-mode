;;; Package: --- Summary
"this is a place where things happen."
;;; Commentary:
"these are things"

;; to add to emacs config
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/bb-mode"))
;;(require 'bb-mode)

;;; Require
(require 'bongo)
(require 'vterm)

;;; Code:

(defun bb-github-pat-to-killring ()
  "Tiny Macro for adding my github pat to the killring."
  (interactive)
  (kill-new "PUT TOKEN HERE")
  (message "Pat added to kill ring."))
(global-set-key (kbd "C-c b p") 'bb-github-pat-to-killring)

(defun bb-move-region-down ()
  "Move a marked text region down (not used)."
  (interactive)
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)end))))
  (forward-line)
  (kill-line)
  (region-beginning)
  (forward-line -1)
  (yank))

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
  "Duplicate line or region depending on mark active."
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
  "Allways kill the whole line. Now with universal operator!"
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
    (skip-chars-backward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)(^")
    (setq mpos (point))
    (skip-chars-forward "-_A-Za-z 0-9?/|.,;:}{+=!@#$><%&*)(^")
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
  (let (bol)
    (setq bol (thing-at-point 'word))
    (skip-chars-forward "-_A-Za-z0-9")
    (cond
     ((null bol) (insert "false"))
     ((string= bol "true") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "false"))))
     ((string= bol "True") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "False"))))
     ((string= bol "false") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "true"))))
     ((string= bol "False") (progn (bb-kill-word-on-cursor) (if (not (equal manual-boolean nil)) (insert manual-boolean) (insert "True")))))
    ))

(defun bb-flip-boolean-interactive (&optional arg)
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

(defun bb-parenthify-line() ;; this function BARELY gets used.. I may just delete it.
  "Add parenthese around word on cursor."
  (interactive)
  (skip-chars-backward "-_A-Za-z0-9")
  (insert "(")
  (skip-chars-forward "-_A-Za-z0-9")
  (insert ")"))
(global-set-key (kbd "C-c w (") 'bb-parenthify-line)

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
    (right-char)
    (insert aftr)))

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
    (end-of-buffer)
    (line-move -2)
    (setq message-test (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (funcall picked-option message-test)))
(global-set-key (kbd "C-c m p") 'bb-message-pop)

(defun bb-wrap-word ()
  "Wrap word/region with char of choice."
  (interactive)
  (let (insq)
  (setq insq (read-string "Wrapping Char? (\"):" nil nil "\""))
  (if (string-match insq "({})][")
    (progn
      (cond ((string= insq "{") (bb-wrap-at-region "{" "}"))
	    ((string= insq "(") (bb-wrap-at-region "(" ")"))
	    ((string= insq "[") (bb-wrap-at-region "[" "]"))
	    ((string= insq "}") (bb-wrap-at-region "{" "}"))
	    ((string= insq ")") (bb-wrap-at-region "(" ")"))
	    ((string= insq "]") (bb-wrap-at-region "[" "]"))
	    ))
    (progn (bb-wrap-at-region insq insq)))))
(global-set-key (kbd "C-c w s") 'bb-wrap-word)

;; (defun bb-highlight-in-parenthese ()
;;   "Mark text in parenthese."
;;   (interactive)
;;   (let (p1 p2)
;;     (skip-chars-backward "-_A-Za-z 0-9?/|'\".,;:}+=!@#><$%&*)^]")
;;     (setq p1 (point))
;;     (skip-chars-forward "-_A-Za-z 0-9?/|'\".,;:{+=!@#><$%&*(^[")
;;     (setq p2 (point))
;;     (goto-char p1)
;;     (push-mark p2)
;;     (setq mark-active t)))


(defun bb-smart-highlight-in-parenthese ()
  "Mark text in parenthese, find the matching parenthese from the first found left side one (by going backwards from cursor position)."
  (interactive)
  (let (p1 p1-char p2)
    (skip-chars-backward "-_A-Za-z 0-9?/|'\".,;:}+=!@#><$%&*)^]")
    (setq p1-char (string (char-before)))
    (char-before 1)
    (setq p1 (point))
    (cond
     ((string= "(" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|'\".,;:{+=!@#>]<$(%&*}^[") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t)))
     ((string= "{" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|'\".,;:{+=!@#>]<$(%&*)^[") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t)))
     ((string= "[" p1-char) (progn (skip-chars-forward "-_A-Za-z 0-9?/|'\".,;:{+=!@#>)(<$%&*}^[") (setq p2 (point)) (goto-char p1) (push-mark p2) (setq mark-active t))))))
(global-set-key (kbd "C-c h p") 'bb-smart-highlight-in-parenthese)


(defun bb-highlight-string ()
  "Mark text in quotations."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)(^][")
    (setq p1 (point))
    (skip-chars-forward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)(^][")
    (setq p2 (point))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))
(global-set-key (kbd "C-c h s") 'bb-highlight-string)

(defun bb-remove-quotations ()
  "Remove wrapping quotations."
  (interactive)
  (skip-chars-backward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)(^][")
  (backward-char 1)
  (delete-char 1)
  (skip-chars-forward "-_A-Za-z 0-9?/|.,;:}{+=!@#><$%&*)(^][")
  (delete-char 1))
(global-set-key (kbd "C-c r q") 'bb-remove-quotations)

(defun bb-remove-parenthese ()
  "Remove wrapping parenthese."
  (interactive)
  (skip-chars-backward "-_A-Za-z 0-9?/|`'\".,;:+=!@#><$%&*^")
  (backward-char 1)
  (delete-char 1)
  (skip-chars-forward "-_A-Za-z 0-9?/|`'\".,;:+=!@#><$%&*^")
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
			    (skip-chars-forward "-_A-Za-z 0-9?/|.'`,;:}{+=!@#><$%&*)(^][")
			    (delete-char 1)
			    (insert "'"))) ;; "
       ((= to-replace 39) (progn
 			    (goto-char start-point)
			    (delete-char 1)
			    (insert "`")
			    (skip-chars-forward "-_A-Za-z 0-9?/|.`\",;:}{+=!@#><$%&*)(^][")
			    (delete-char 1)
			    (insert "`"))) ;; '
       ((= to-replace 96) (progn
			    (goto-char start-point)
			    (delete-char 1)
			    (insert "\"")
			    (skip-chars-forward "-_A-Za-z 0-9?/|.'\",;:}{+=!@#><$%&*)(^][")
			    (delete-char 1)
			    (insert "\""))))) ;; `
    (backward-char 1)))
(global-set-key (kbd "C-c f q") 'bb-replace-quotations)


(defun bb-switch-create-ansi ()
  "This is a function for switching between ansi-term buffers, if none exist create one."
  (interactive)
  (let (max-ansi-term current-ansi-term)

    ;; check if current buffer is an ansi-term, if so set 'current-ansi-term' and move to the next one
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
					(switch-to-buffer "*ansi-term*"))
				      ))
     ((get-buffer "*ansi-term*<4>") (progn
				      (setq max-ansi-term 4)
				      (if (< current-ansi-term max-ansi-term)
					  (if (equal current-ansi-term 0)
					      (if (get-buffer "*ansi-term*") (switch-to-buffer "*ansi-term*") (ansi-term "/usr/bin/fish"))
					    (switch-to-buffer (format "*ansi-term*<%s>" (+ current-ansi-term 1))))
					(switch-to-buffer "*ansi-term*"))
				      ))
     ((get-buffer "*ansi-term*<3>") (progn
				      (setq max-ansi-term 3)
				      (if (< current-ansi-term max-ansi-term)
					  (if (equal current-ansi-term 0)
					      (if (get-buffer "*ansi-term*") (switch-to-buffer "*ansi-term*") (ansi-term "/usr/bin/fish"))
					    (switch-to-buffer (format "*ansi-term*<%s>" (+ current-ansi-term 1))))
					(switch-to-buffer "*ansi-term*"))
				      ))
     ((get-buffer "*ansi-term*<2>") (progn
				      (setq max-ansi-term 2)
				      (if (< current-ansi-term max-ansi-term) 
					  (if (equal current-ansi-term 0)
					      (if (get-buffer "*ansi-term*") (switch-to-buffer "*ansi-term*") (ansi-term "/usr/bin/fish"))
					    (switch-to-buffer "*ansi-term*<2>"))
					(switch-to-buffer "*ansi-term*"))
				      ))
     ((get-buffer "*ansi-term*") (progn (setq max-ansi-term 1) (switch-to-buffer "*ansi-term*")))
     ((equal max-ansi-term 0) (ansi-term "/usr/bin/fish")))))
(global-set-key (kbd "C-`") 'bb-switch-create-ansi)


(defun bb-increment-number-at-point ()
  "Increment number at point https://www.emacswiki.org/emacs/IncrementNumber."
  (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun bb-increment-number-at-point-universal (&optional arg)
  "A universal compliant wrapper for bb-increment-number-at-point."
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
  "A universal compliant wrapper for bb-increment-number-at-point."
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

(global-set-key (kbd "C-c p")
  (lambda ()
    (interactive)
    (re-search-backward (format "\\b%s\\b" (thing-at-point 'word)))))

;; search for .. (dired mode)
(global-set-key (kbd "C-.")
  (lambda ()
    (interactive)
    (search-backward "..")))

launch Term
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


(defun bb-global-play-Music()
  "Globally Play/Pause music."
  (interactive)
  (cond ((eq playing 'true) (shell-command "playerctl pause") (setq playing 'false))
	((eq playing 'false) (shell-command "playerctl play") (setq playing 'true))))

(defun bb-use-sht-music()
  (interactive)
  (progn
  (ansi-term "/bin/bash" "Music")
  (global-set-key (kbd "C-x p") '(switch-to-buffer "Music"))
  (switch-to-buffer "Music")))

(defun bb-use-bongo-music()
  (interactive)
  (progn
    (bongo)
    (bongo-insert-directory-tree "~/Music/")
    (global-set-key (kbd "C-x p") 'bongo-pause/resume)
    (global-set-key (kbd "C-c b l") 'bongo-library)
    (global-set-key (kbd "C-x n s") 'bongo-next)))

(defun bb-update-music-player (&optional pre-picked)
  (interactive)
  (let ((choices '("b" "g" "s")) picked)
    (if pre-picked
	(setq picked pre-picked) 
      (setq picked (completing-read "Starting Music Player? (b:bongo, g:global, s:sht):" choices)))
    (cond ((string= picked "b") (bb-use-bongo-music))
	  ((string= picked "g") (global-set-key (kbd "C-x p") 'bb-global-play-Music))
	  ((string= picked "s") (bb-use-sht-music)))))

(provide 'bb-mode)
;;; bb-mode.el ends here

(require 'comint)

;; q el
(defvar-local q-buffer nil
  "Current `q` buffer")

(defvar-local q-sync-timer nil
  "Sync timer used to reconstruct the prompt if onsolicited messages come out")

(defvar q-default-command "q"
  "The q command to run by default when creating a q-session")

(defvar-local q-command nil
  "Current `q` command. Default is `q-default-command` not this value.")

(defvar-local q--window-height nil
  "Last recorded window height")

(defvar-local q--window-width nil
  "Last recorded window width")

(defcustom q-mode-window-sync-p t
  "Keep the q window in sync with the emacs window (using the \\c command)"
  :type '(boolean)
  :group 'q)

(defun q--session-script (command)
  ;; this "looks this way" to support tramp.
  (concat
   "export QHOME=\"${QHOME:-$HOME/q}\";export QLIC=\"${QLIC:-$QHOME}\";export PATH=\"$PATH:$QHOME/l64:$QHOME/l32\";[ -f \"$HOME/.bash_profile\" ]&&source \"$HOME/.bash_profile\";"
   command))

(defun q--session-comint (procname command)
  (let ((buffer nil))
    (save-window-excursion
      (setq buffer (make-comint-in-buffer procname procname "/bin/bash" nil "-c" (q--session-script command))))
    buffer))   

(defvar q-session-history nil)

(defun q-check-sync (buffer)
  (with-current-buffer buffer
    (setq q-sync-timer nil)
    (ignore-errors 
      (if (q-needs-sync-p)
	  (q-sync)))))

(defvar-local q--datachannel-region nil
  "Context saved between two calls to the output filter `q--comm-output-hook'.")

(defun q--comm-output-hook (&optional string)
  (if q-sync-timer (cancel-timer q-sync-timer))
  (setq q-sync-timer (run-at-time "1 sec" nil 'q-check-sync (current-buffer)))
  (let ((begin (or comint-last-output-start (point-min-marker)))
	(end (process-mark (get-buffer-process (current-buffer)))))
    (let ((start-marker (or q--datachannel-region (copy-marker begin)))
	  (end-marker (copy-marker end)))
      (save-excursion
	(goto-char start-marker)
	(while (re-search-forward "\e]1337;File=[^:]*:[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/]*=*[\000-\037]" end-marker t)
	  (let ((text (buffer-substring-no-properties (match-beginning 0) (point))))
	    (replace-match "")
	    (message ".")
	    (setq text (substring text 0 (- (length text) 1)))
	    (let ((image (base64-decode-string (nth 1 (split-string text ":")))))
	      (insert-image (create-image image nil t)))))
	(if (re-search-forward "[\000-\037]" end-marker t)
	    (progn
	      (backward-char)
	      (setq q--datachannel-region (point)))
	  (setq q--datachannel-region nil))))))

(defun q--comm-input-hook (string)
  (let ((h (window-body-height))
	(w (window-body-width)))
    (if (or (not q-mode-window-sync-p)
	    (string= (substring string 0 1) "\\")
	    (and (eq q--window-height h) (eq q--window-width w)))
	string
      (comint-simple-send (get-buffer-process (current-buffer))
			  (format "\\c %d %d" h w))
      (setq q--window-height h
	    q--window-width w)
      string)))

(defun q--init-comint-mode ()
  (add-hook 'comint-dynamic-complete-functions 'q--comm-complete) 
  (add-hook 'comint-input-filter-functions 'q--comm-input-hook)
  (make-variable-buffer-local 'q--datachannel-region)
  (add-hook 'comint-output-filter-functions 'q--comm-output-hook))

(defun q-needs-sync-p ()
  (save-excursion
    (end-of-buffer)
    (backward-char 1)
    (eq 'fence (get-text-property (point) 'read-only))))

(defun q-session (command &optional session)
  (interactive (list (read-string (format "q workspace command: " q-default-command) q-default-command 'q-session-history q-default-command)))
  (if (null session)
      (setq session "")
    (setq session (format "(%s)" session)))
  (let ((orig-command command))
    (if (and command q-command (not (string= q-command command)))
	(setq q-buffer nil))
    (if command
	(setq q-command command)
      (setq command (or q-command q-default-command))) ; always prefer buffer version
    (if q-buffer
	q-buffer
      (let ((procname (format "q%s %s: exec %s" session default-directory command)))
	(if (comint-check-proc procname)
	    (get-buffer procname)
	  (let ((new-buffer (q--session-comint procname command)))
	    (with-current-buffer new-buffer (q-comint-mode))
	    (when orig-command
	      (split-window-sensibly)
	      (switch-to-buffer-other-window new-buffer))
	    new-buffer))))))

(defun q--hex-encode (before text after)
  (let ((res (list (concat before "10h$0x"))))
    (dotimes (i (length text) (apply #'concat (reverse (cons after res))))
      (push (format "%02x" (string-to-char (substring text i (+ i 1)))) res))))

(defun q--encode (text)    
  (if (string-match "[\000-\017\177-\377]" text)
      (q--hex-encode "value[" text "]")
    text))

(defun q-sync ()
  (interactive)
  (with-current-buffer (q-session nil)
    (comint-simple-send (get-buffer-process (current-buffer)) "")
    (goto-char (point-max))
    (insert-before-markers "\n")))

(defun q-send-string (text)
  (with-current-buffer (q-session nil)
    (goto-char (point-max))
    (insert-before-markers (concat text "\n"))
    (comint-simple-send (get-buffer-process (current-buffer)) (q--encode text))))

(defun q-send-region (start end)
  "Send the region between START and END to the inferior q[con] process."
  (interactive "r")
  (save-excursion
    (save-restriction 
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((sending t))
	(while (not (eobp))
	  (let ((statement (q-current-statement)))
	    (if (and sending (string= statement "\\"))
		(setq sending nil)
	      (if (and (not sending) (string= statement "/"))
		  (setq sending t)
		(if sending
		    (q-send-string statement)))))
	  (q-next-statement)))
      (widen))))

(defun q-send-line ()
  "Send the current line to the inferior q[con] process."
  (interactive)
  (q-send-string (buffer-substring (point-at-bol) (point-at-eol))))

(defun q-send-buffer ()
  "Load current buffer into the inferior q[con] process."
  (interactive)
  (q-send-region (point-min) (point-max)))

(defun q-beginning-of-statement ()
  (beginning-of-line)
  (while (and
	  (not (bobp))
	  (not (eobp))
	  (let ((c (following-char)))
	    (or (= c 9) (= c 10) (= c 13) (= c 32))))
    (forward-line -1)))
  
(defun q-next-statement ()
  (let ((end (re-search-forward "\n[^ \t]" nil t 1)))
    (let ((p (if end (- end 1) (point-max))))
      (goto-char p)
      p)))

(defun q-end-of-statement ()
  (q-next-statement)
  (while (and (not (bobp))
	      (let ((c (preceding-char)))
		(or (= c 9) (= c 10) (= c 13) (= c 32))))
    (backward-char)))

(defun q-current-statement ()
  (save-excursion
    (q-beginning-of-statement)
    (let ((start (point)))
      (q-end-of-statement)
      (buffer-substring-no-properties start (point)))))

(defun q-send-statement ()
  "Send the current function/definition to the inferior q[con] process."
  (interactive)
  (q-send-string (q-current-statement)))


(defun q--init-mode ()
  t)

(defvar q-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'q-send-statement)
    (define-key map "\C-\M-x"  'q-send-statement)
    (define-key map "\C-c\C-k" 'q-send-buffer)
    (define-key map "\C-c\M-k" 'q-send-buffer)
    (define-key map "\C-c\C-l" 'q-send-buffer)
    (define-key map "\C-c\C-r" 'q-send-region)
    (define-key map "\C-c\C-j" 'q-send-line)
    (define-key map "\C-x\C-e" 'q-send-statement) ;;!

    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `q`")

(define-derived-mode q-mode fundamental-mode "q"
  "Major mode for q code (kx.com)

\\<q-mode-map>"
  nil "q")
(add-hook 'q-mode-hook 'q--init-mode)


(defvar q-comint-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    (define-key map "\C-g" 'q-sync)
    map)
  "Basic mode map for `q`")

(defvar q-prompt-regexp "^[a-z ](?:\\.[^\\)]*)?[ ]?\\)+"
  "Prompt pattern for `q`")

(define-derived-mode q-comint-mode comint-mode "q-comint"
  "Major mode for q interactive sessions (kx.com)

\\<q-comint-mode-map>"
  nil "q-comint"
  (setq comint-prompt-regexp q-prompt-regexp)
  (setq comint-prompt-read-only t)
  (setq q-buffer (current-buffer))
  (set (make-local-variable 'paragraph-start) q-prompt-regexp))
(add-hook 'q-comint-mode-hook 'q--init-comint-mode)
  
(defun q (&optional dir)
  (interactive)
  (if (eq major-mode 'fundamental-mode) (q-mode))
  (let ((default-directory (or dir default-directory)))
    (command-execute 'q-session)))




(defun org-babel-execute:q (body params)
  (setq params (org-babel-process-params params))
  (let (temporary
	(sid (assq :session params))
	(cmd (assq :command params))
	(rt  (assq :result-type params))
	(dir (assq :dir params)))
    (let ((default-directory (if dir (cdr dir) default-directory))
	  (result-type (if (consp rt) (cdr rt) 'value)))
      (save-window-excursion
	(let ((session (q-session (if cmd (cdr cmd) "q")
				  (if (and (consp sid) (not (string= "none" (cdr sid))))
				      (format "%s" (cdr sid))
				    (setq temporary (format "%s" (md5 body)))
				    temporary))))
	  (sit-for 0.1)
          (while (not (comint-check-proc session))
            (sit-for 0.1))
	  (with-current-buffer session
	    (let (start end text (proc (get-buffer-process session)))
	      (setq start (copy-marker (process-mark proc)))
	      (ignore-errors
		(goto-char start)
		(backward-char))
	      (setq start (point-marker))
	      (dolist (x params)
		(if (eq :var (car x))
		    (setq body (concat (format "%s:%s;" (cadr x) (cddr x)) body))))
	      (if (eq result-type 'output)
		  (setq body (concat "-1\"\\033]1337;BeginQ\\007\";" body))
		(setq body (concat "{-1\"\\033]1337;BeginQ\\007\";show x}[{" body "}[]];")))
	      (setq body (concat body ";-1\"\\033]1337;EndQ\\007\";"))
	      (comint-simple-send proc (q--encode body))
	      (while (not (re-search-forward "\033]1337;EndQ\007" nil t))
		(accept-process-output proc)
		(goto-char start))
	      (replace-match "")
	      (setq end (copy-marker (point-marker)))
	      (re-search-backward "\033]1337;BeginQ\007")
	      (replace-match "")
	      (let ((inhibit-read-only t))
		(setq text (delete-and-extract-region (point) end)))
	      (setq text (substring text 1))
	      (delete-region (point) end)
	      (when temporary
		(set-process-query-on-exit-flag proc nil)
		(kill-process proc)
		(kill-buffer session))
	      (message "results %s" text)
	      text)))))))

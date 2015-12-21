(defvar kq-default-language "k"
  "Default language for new KQ sessions")

(defvar kq-connection nil)
(defvar kq-language nil)

;;
(defun kq-connections ()
  (let ((conn nil))
    (dolist (x (buffer-list))
      (let ((proc (get-buffer-process x)))
	(if (and proc (process-get proc 'kq-language))
	    (setq conn (cons proc conn)))))
    (nreverse conn)))

(defvar kq-mode-syntax-table nil)
(let ((table (make-syntax-table)))
  (dolist (x (string-to-list "!@#$%^&*:,.<>-_+=\\|';/?~"))
    (modify-syntax-entry x "." table))
  (modify-syntax-entry ?\` "'" table)
  (modify-syntax-entry ?\( "()  " table)
  (modify-syntax-entry ?\) ")(  " table)
  (modify-syntax-entry ?\[ "(]  " table)
  (modify-syntax-entry ?\] ")[  " table)
  (modify-syntax-entry ?\{ "(}  " table)
  (modify-syntax-entry ?\} "){  " table)
  (setq kq-mode-syntax-table table))

(defvar kq-mode-map nil)
(let ((map (make-sparse-keymap)))
  (define-key map "\C-c\C-c" 'kq-send-region)
  (define-key map "\C-x\C-e" 'kq-send-region)
  (setq kq-mode-map map))

(define-derived-mode kq-mode nil "KQ"
  "Major mode for writing K/Q code

\\{kq-mode-map}"
  (set (make-local-variable 'kq-connection) (car (kq-connections))) ;last touch
  (set (make-local-variable 'kq-language) nil)
  (set-syntax-table kq-mode-syntax-table))


(defvar kq-interactive-mode-map nil)
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "<return>") 'kq-interactive-enter)
  (define-key map "\C-a" 'kq-interactive-start)
  (setq kq-interactive-mode-map map))

(define-derived-mode kq-interactive-mode nil "KQ"
  "Major mode for interacting with a KDB server

\\{kq-interactive-mode-map}"
  (set-syntax-table kq-mode-syntax-table))

(defun kq-ipc-raw-integer (integer)
  "Encode an integer in little-endian"
  (unibyte-string
   (logand (lsh integer 0) 255)
   (logand (lsh integer -8) 255)
   (logand (lsh integer -16) 255)
   (logand (lsh integer -24) 255)))

(defun kq-ipc-string (string)
  (setq string (format "%s" string))
  (concat "\n\0"
	  (kq-ipc-raw-integer (string-bytes string))
	  (string-to-unibyte string)))

(defun kq-ipc-symbol (symbol)
  (setq symbol (format "%s" symbol))
  (concat "\xf5" (string-to-unibyte symbol) "\0"))

(defun kq-ipc-integer (integer)
  (concat "\xf9" (kq-ipc-raw-integer integer) "\0\0\0\0")) ; 32-bit only

(defun kq-ipc-list (count encoded)
  (concat "\0\0"  (kq-ipc-raw-integer count) encoded))

(defun kq-ipc-message (message-type encoded)
  (concat
   "\1"
   (unibyte-string message-type)
   "\0\0"
   (kq-ipc-raw-integer (+ 8 (string-bytes encoded)))
   encoded))

;;
(defun kq-send (code)
  (cond
   ((null kq-connection)
    (let ((x (kq-connections)))
      (cond
       ((null x) (call-interactively 'kq-connect))
       ((null (cdr x)) (setq kq-connection (car x)) (kq-send code))
       (t (error "todo")))))
   (t
    (let* ((lang (or kq-language
		     (let ((ext (file-name-extension (buffer-name))))
		       (if (or (string= ext "q") (string= ext "k")) ext nil))
		     (process-get kq-connection 'kq-language)))
	   (message (kq-ipc-list 3 (concat
				    (kq-ipc-string (kq-language-interp lang))
				    (kq-ipc-string (format "%s)%s" lang code))
				    (kq-ipc-integer 0)))))
      (process-send-string kq-connection (kq-ipc-message 0 message))))))

(defun kq-current-statement ()
  (save-excursion
    (beginning-of-line)
    (while (and
	    (not (bobp))
	    (not (eobp))
	    (let ((c (following-char)))
	      (or (= c 9) (= c 10) (= c 13) (= c 32))))
      (forward-line -1))
    (let ((start (point)) c q esc (ws t) (brace 0) (bracket 0) (paren 0) result)
      (while (and (not result) (not (eobp)))
	(setq c (following-char))
	(cond
	 (esc (setq esc nil))
	 (q (cond
	     ((= c 92) (setq esc t)) ;backslash
	     ((= c ?\") (setq q nil))))
	 ((and ws (= c ?\/) (end-of-line)))
	 ((or (= c 32) (= c 9))  (setq ws t))
	 ((or (= c 10) (= c 13))
	  (if (and (= 0 bracket brace paren) (not q))
	      (setq result (buffer-substring-no-properties start (point)))))
	 (t
	  (setq ws nil)
	  (cond
	   ((= c ?\") (setq q t))
	   ((= c ?\[) (setq bracket (1+ bracket)))
	   ((= c ?\]) (setq bracket (1- bracket)))
	   ((= c ?\() (setq paren (1+ paren)))
	   ((= c ?\)) (setq paren (1- paren)))
	   ((= c ?\{) (setq brace (1+ brace)))
	   ((= c ?\}) (setq brace (1- brace))))))
	(forward-char 1))
      (when (not result)
	(setq result (buffer-substring-no-properties start (point))))
      result)))
	 
(defun kq-send-region (mark point)
  (interactive "r")
  (kq-send
   (if (use-region-p) (buffer-substring-no-properties mark point)
     (save-excursion (goto-char point) (kq-current-statement)))))


;;
(defun kq-connect (port &optional hostname username password)
  (interactive "NPort: ")
  (if (null hostname) (setq hostname "127.0.0.1"))
  (let* ((name (format "kq`:%s:%d" hostname port))
         (buffer (get-buffer-create name))
	 (proc (make-network-process
		:name     name
		:host     hostname
		:service  port
		:nowait   t
		:buffer   buffer
		:coding   '(no-conversion . no-conversion)
		:filter   'kq-process-filter
		:sentinel 'kq-process-sentinel)))
    (with-current-buffer buffer
      (kq-interactive-mode)
      (setq buffer-read-only nil))
    (process-put proc 'buffer buffer)
    (process-put proc 'phase 'login)
    (process-put proc 'points (make-hash-table))
    (process-put proc 'neednl (make-hash-table))
    (process-put proc 'npoint 0)
    (process-put proc 'inq "")
    (process-put proc 'outq nil)
    (process-put proc 'kq-language kq-default-language)
    (switch-to-buffer buffer)
    (if (or username password)
        (process-put proc 'login
                     (format "%s:%s"
                             (or username "")
                             (or password ""))))
    proc))

(defun kq-read-int (string n)
  (logior (elt string (+ n 0))
	  (lsh (elt string (+ n 1)) 8)
	  (lsh (elt string (+ n 2)) 16)
	  (lsh (elt string (+ n 3)) 24)))

(defun kq-update-output-spot (proc slot buffer position)
  (puthash slot (vector buffer position) (process-get proc 'points)))
(defun kq-output-spot (proc slot)
  (gethash slot (process-get proc 'points)))
(defun kq-output-spot-buffer (spot)
  (elt spot 0))
(defun kq-output-spot-position (spot)
  (elt spot 1))

(defun kq-insert-readonly (text)
  (let ((here (point))
	(inhibit-read-only t))
    (insert text)
    (add-text-properties here (point) '(read-only t rear-nonsticky (read-only)))))

(defun kq-process-slot (proc slot fun)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (let ((spot (kq-output-spot proc slot))
	    (inhibit-read-only t))
	(with-current-buffer (kq-output-spot-buffer spot)
	  (goto-char (kq-output-spot-position spot))
	  (funcall fun)
	  (kq-update-output-spot proc slot (current-buffer) (point)))))))

(defun kq-process-image (proc slot text)
  (kq-process-slot proc slot
		   (lambda ()
		     (let ((start (point)))
		       (insert "\n")
		       (insert-image (create-image text nil t))
		       (add-text-properties start (point)
					    '(read-only t rear-nonsticky (read-only)))))))


(defun kq-process-string (proc slot text)
  (cond
   ((= 0 slot)
    (message text))
   ((string= "" text)
    t)
   (t
    (let ((neednl (string= (substring text -1) "\n"))
	  (table (process-get proc 'neednl)))
      (if neednl (setq text (substring text 0 -1)))
      (kq-process-slot proc slot
		       (lambda ()
			 (if (gethash slot table)
			     (setq text (concat "\n" text)))
			 (kq-insert-readonly text)
			 (puthash slot neednl table)))))))

(defun kq-process-input (proc message)
  (let ((type (elt message 8)))
    (cond
     ;((= type 246) (kq-process-string proc 0 (char-to-string (elt message 9))))
     ((= type 10)  (kq-process-string proc 0 (substring message 14))) ;; framed
     ((and (= type 0) (= (elt message 10) 2) (= (elt message 23) 10)) ;; (n; text)
      (kq-process-string proc (kq-read-int message 15) (substring message 29)))
     ((and (= type 0) (= (elt message 10) 2) (= (elt message 23) 4)) ;; (n; 0ximg)
      (kq-process-image proc (kq-read-int message 15) (substring message 29)))
     (t (error "unsupported kdb parse")))))	  

(defun kq-process-filter (proc input)
  (setq input (concat (process-get proc 'inq) input))
  (process-put proc 'inq "")
  (let ((n nil)
	(inhibit-read-only t))
    (cond
     ((eq 'login (process-get proc 'phase))
      (process-put proc 'phase 'waiting)
      (cond
       ((and (<= 1 (length input)) (= (elt input 0) 0))
	(kq-insert-prompt proc)
	(kq-process-filter proc (substring input 1)))
       (t
	(let ((q  (process-get proc 'outq))
	      (nq (kq-connect (process-contact proc :service)
			      (process-contact proc :remote)
			      (read-from-minibuffer "kdb username: ")
			      (read-passwd "password: "))))
	  (if q (process-put nq 'outq q))))))
     ((or (> 8 (length input))
	  (> (setq n (kq-read-int (substring input 4 8) 0)) (length input))) ;;setq n
      (process-put proc 'phase 'more)
      (process-put proc 'inq input))
     (t
      (kq-process-input proc (substring input 0 n))
      (kq-process-filter proc (substring input n))))))

(defun kq-process-sentinel (proc event)
  (cond
   ((string= event "open\n")
    (let ((auth (process-get proc 'login)))
      (process-put proc 'login nil)
      (process-send-string proc (concat (string-to-unibyte (or auth "anonymous")) "\0"))))
   (t
    (princ (replace-regexp-in-string "\n$" "" (format "kq %s" event))))))

(defun kq-insert-prompt (proc)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((p (point))
	    (n (process-get proc 'npoint)))
	(setq n (+ n 1))
	(process-put proc 'npoint n)
	(insert (kq-language-prompt proc))
	(add-text-properties p (point)
			     (list
			      'read-only t
			      'rear-nonsticky '(read-only)
			      'kq-spot n
			      'kq-language (process-get proc 'kq-language)))))))

(defun kq-interactive-clear ()
  (interactive)
  (kill-region (kq-interactive-start) (point-max)))

(defun kq-interactive-start ()
  (interactive)
  (let ((slot (get-pos-property (point) 'kq-spot))
	(here (point)))
    (while (and
	    (< 1 (point))
	    (eql slot (get-pos-property (point) 'kq-spot)))
      (setq here (point))
      (backward-char 1))
    (goto-char here)
    (forward-char 1)
    (point)))

(defun kq-interactive-enter ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	(language (get-pos-property (point) 'kq-language))
	(start    (save-excursion (kq-interactive-start)))
	(end      (save-excursion (end-of-line) (point)))
	(slot     (get-pos-property (point) 'kq-spot)))
    (let ((text   (buffer-substring-no-properties start end))) ; length of prompt
      (cond
       ((kq-output-spot proc slot) ;copy
	(goto-char (point-max))
	(kq-interactive-clear)
	(insert-and-inherit text))
       ((string= text "")
	(kq-insert-readonly "\n") ;ignore, but allow newlines because they look nice.
	(kq-insert-prompt proc))
       (t
	(let ((interp (kq-ipc-string (kq-language-interp proc)))
	      (inhibit-read-only t)
	      (code   (kq-ipc-string (format "%s)%s" language text))))
	  (goto-char end)
	  (kq-update-output-spot proc slot (current-buffer) (point))
	  (puthash slot t (process-get proc 'neednl))
	  (insert "\n")
	  (add-text-properties start (point)
			       '(read-only t rear-nonsticky (read-only)))
	  (if (string= "\\" text)
	      (process-put proc 'kq-language (if (string= "k" (process-get proc 'kq-language)) "q" "k"))
	    (process-send-string proc (kq-ipc-message 0 (kq-ipc-list 3 (concat interp code (kq-ipc-integer slot))))))
	  (kq-insert-prompt proc)))))))	

(defun kq-language-format (proc)
  (if (processp proc) (setq proc (process-get proc 'kq-language)))
  (if (string= "k" proc) "-3!" ".Q.s@"))
(defun kq-language-interp (proc)
  (concat
   "k){a:.q.show;.z.slot::y;.q.show::{y@(z;.Q.s x);}[;-.z.w;y];.:\"\\\\c "
   (format "%d %d" (window-body-height) (window-body-width))
   "\";@[{x:.:x;y@(z;$[101h=@x;\"\";"
   (kq-language-format proc)
   "x])}[;-.z.w;y];x;{y@(z;\"'\",x,\"\\n\")}[;-.z.w;y]];.q.show::a;}"))
(defun kq-language-prompt (proc)
  (let ((lang (process-get proc 'kq-language)))
    (if (string= "k" lang) "  " (format "%s)" lang))))

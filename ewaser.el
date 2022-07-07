(defun ewaser--get-class-name (str)
  "Return the css classname from STR"
  (and (s-starts-with-p "." str)
	   (cadr (split-string str "\\."))))

(defun ewaser--get-id-name (str)
  "Return the css id from STR"
  (and (s-starts-with-p "#" str)
	   (cadr (split-string str "\\#"))))

(defun ewaser--get-node-by-selector (dom selector)
  "Giving DOM and a css SELECTOR, return the nodes"
  (if-let ((class-name (ewaser--get-class-name selector)))
	  (dom-by-class dom class-name)
	(if-let ((id-name (ewaser--get-id-name selector)))
		(dom-by-id dom id-name))))

(defun ewaser-clean (selectors)
  "Take a list of css selector in SELECTORS (separated by space)
and remove the associated nodes from the DOM in the current eww
buffer."
  (interactive "MDomselector: ")
  (unless (eq major-mode 'eww-mode)
	(user-error "Not in a EWW buffer"))
  (let* ((data eww-data)
		 (dom (ewaser--get-dom))
		 (base (plist-get data :url))
		 (selectors (split-string selectors)))
	(dolist (s selectors)
	  (dolist (node (ewaser--get-node-by-selector dom s))
		(dom-remove-node dom node)))
	(eww-display-html nil nil
					  (list 'base (list (cons 'href base))
							dom)
					  nil (current-buffer))
	(dolist (elem '(:source :url :title :next :previous :up))
	  (plist-put eww-data elem (plist-get data elem)))
	(eww--after-page-change)))

(defun ewaser--get-dom ()
  "Retrieve the DOM object from the current eww buffer. Extracted
from eww code."
  (let ((data eww-data))
	(with-temp-buffer
	  (insert (plist-get data :source))
	  (condition-case nil
		  (decode-coding-region (point-min) (point-max) 'utf-8)
		(coding-system-error nil))
	  (eww--preprocess-html (point-min) (point-max))
	  (libxml-parse-html-region (point-min) (point-max)))))



(defun ewaser--hook ()
  "Hook passed to eww-after-render-hook"
  (let* ((url (plist-get eww-data :url))
		 (rules (ewaser--get-rules url ewaser-rules-alist)))
	(when rules
	  (ewaser-clean (string-join rules " ")))))


(defun ewaser--get-rules (url alist)
  (catch 'match
	(dolist (entry alist)
	  (let ((key (car entry)))
		(when (string-match-p key url)
		  (throw 'match (cdr entry)))))))

(provide 'ewaser)




(defun org+-element-container (el &optional type)
  "Return the element of TYPE where element EL is contained.
TYPE defaults to 'headline.
Returns nil if El has no container with type TYPE."
  (unless type (setq type 'headline))
  (while (and el
              (null (eq (org-element-type el) type)))
    (setq el (org-element-property :parent el)))
  el)

(defsubst org+-element-set-property (el prop val)
  "Set property PROP of element EL to VAL."
  (setf (nth 1 el) (plist-put (nth 1 el) prop val)))

(defsubst org+-element-set-parent (el parent)
  "Set parent of element EL to PARENT."
  (org+-element-set-property el :parent parent))

(defun org+-element-add-contents (el item &optional append afterp)
  "Add ITEM to org element EL.
Thereby the :parent property of ITEM is set to EL.
You should create ITEM by `org-element-copy',
`org-element-create', or `org-element-extract'.
Those functions either create a new element or unlink
the element from the parse tree.
That way there is no parent when this function is called.

AFTERP is nil or a predicate with an org element
as its only argument.
ITEM is inserted after the first element of EL
for which AFTERP returns non-nil.
The new ITEM is appended to the existing content
if AFTERP is undecisive and APPEND is non-nil
and prepend otherwise."
  (org+-element-set-parent item el)
  (let* ((contents (org-element-contents el))
         (ptr contents)
         found)
    (when afterp
      (while ptr
        (if (funcall afterp (car ptr))
            (progn
              (setcdr ptr (cons item (cdr ptr)))
              (org-element-set-contents el contents) ;; pro forma
              (setq found t
                    ptr nil))
          (setq ptr (cdr ptr)))))
    (unless found
      (org-element-set-contents
       el
       (if append
           (setcdr (last contents) (cons item nil))
         (cons item contents))))))

(defcustom org+-copy-id_log-clock-allow-duplicates nil
  "Allow duplicated clock entries in logbooks when copying
clock entries from id_log to id."
  :group 'org
  :type 'boolean)

(defsubst org+-element-clocks=-p (clock1 clock2)
  "Test whether clock elements CLOCK1 and CLOCK2 are equivalent."
  (string-equal (org-element-interpret-data clock1)
                (org-element-interpret-data clock2)))

(require 'subr-x)

(defmacro with-current-file (filename &rest body)
  "Temporarily visit FILENAME to execute BODY.
If a buffer is already visiting FILENAME re-use that buffer.
Otherwise create a new buffer for visiting FILENAME
and kill that buffer if it is unmodified after executing BODY."
  (declare (indent 1) (debug (form body)))
  (let ((file-buffer (make-symbol "file-buffer"))
    (file-name (make-symbol "file-name"))
    (old-file-buffer (make-symbol "old-file-buffer")))
    `(let* ((,file-name ,filename)
        (,old-file-buffer (find-buffer-visiting ,file-name))
        (,file-buffer (or ,old-file-buffer
                                (find-file-noselect ,file-name))))
       (with-current-buffer ,file-buffer
         (unwind-protect
         (progn
           ,@body)
       (unless (or ,old-file-buffer
               (buffer-modified-p))
         (kill-buffer)))))))

(defun multivalued-alist-insert (alist key val &optional dup key-plist val-keylist)
  "Extend the multivalued alist by the mapping from KEY to VAL.
Allow duplicated values for one KEY if DUP is non-nil.
KEY-PLIST is a keyword-value plist passed to `cl-assert' for testing KEY.
VAL-PLIST is a keyword-vlaue plist passed to `cl-member' for testing membership of VAL."
  (let ((slot (apply #'cl-assoc key alist key-plist)))
    (if slot
        (when (or dup
                  (null (apply #'cl-member val (cdr slot) val-keylist)))
          (setcdr slot (cons val (cdr slot))))
      (setq alist (cons (list key val) alist))))
  alist)
;; test:
;; (setq l '((1 a) (2 b c) (3 d e f)))
;; (multivalued-alist-insert l 2 'b t) ;; duplicated
;; (multivalued-alist-insert l 2 'b) ;; not duplicated
;; (multivalued-alist-insert l 4 'g) ;; new key

(defun org+-copy-id_log-clock-collect (&optional clock-map hap)
  "Return an alist mapping targets to clock entries.
The clock entries are collected from headers with appropriate ID_LOG properties.
The new clocks are inserted into the alist CLOCK-MAP.
If HAP is non-nil only search current top level header for ID_LOG entries."
  (save-excursion
    (save-restriction
      (when hap
        (org-up-heading-safe)
        (let* ((el (org-element-at-point))
               (b (progn
                    (cl-assert (eq (org-element-type el) 'headline)
                               nil
                               "No headline found.")
                    (org-element-property :begin el)))
               (e (org-element-property :end el)))
          (narrow-to-region b e)))
      (let ((tree (org-element-parse-buffer)))
        (org-element-map
            tree
            'clock
          (lambda (clock)
            (when-let ((headline (org+-element-container clock))
                       (id (org-element-property :ID_LOG headline)))
              (setq clock-map (multivalued-alist-insert
                               clock-map id clock nil
                               '(:test string-equal)
                               '(:test org+-element-clocks=-p))))))
        clock-map))))

(defun org+-add-clock-to-log-in-headline (headline clock)
  "Extend org element HEADLINE by CLOCK.
Also creates a logbook if it does not exist yet in HEADLINE.
Return non-nil if HEADLINE has changed."
  (let ((logbook (car
                  (org-element-map
                      headline
                      'drawer
                    (lambda (logbook)
                      (let ((drawer-name (org-element-property :drawer-name logbook)))
                        (when (and (stringp drawer-name)
                                   (string-equal drawer-name "LOGBOOK"))
                          logbook)))
                    nil nil 'no-recursion))))
    (if logbook ;; logbook already existing -- just add CLOCK to it.
        (when (or org+-copy-id_log-clock-allow-duplicates
                  (null (org-element-map logbook
                            'clock
                          (lambda (log-clock)
                            (when (org+-element-clocks=-p log-clock clock)
                              log-clock)))))
          (org+-element-add-contents logbook (org-element-copy clock))
          t)
      ;; logbook missing -- create one with CLOCK as entry
      (let ((section (org-element-map headline 'section #'identity nil t t)))
        (unless section
          (setq section (org-element-create 'section))
          (org+-element-add-contents headline section))
        (setq logbook (org-element-create 'drawer (list :parent clock
                                                        :drawer-name "LOGBOOK")))
        (org+-element-add-contents logbook clock)
        (org+-element-add-contents section logbook nil
                                   (lambda (el)
                                     (eq (org-element-type el) 'property-drawer))))
      t)))

(defun org+-copy-id_log-clock (&optional clock-map)
  "Copy all clock entries from ID_LOG headlines to ID headlines.
See option `org+-copy-id_log-clock-allow-duplicates'.
Return non-nil if the buffer has been rewritten."
  (interactive)
  (unless clock-map (setq clock-map (org+-copy-id_log-clock-collect)))
  (let ((tree (org-element-parse-buffer))
        rewrite)
    ;; modify tree and print it out
    (org-element-map
        tree
        'headline
      (lambda (headline)
        (when-let ((id (org-element-property :ID headline))
                   (clocks (cdr (assoc-string id clock-map))))
          (cl-loop for clock in clocks do
                   (setq rewrite (or
                                  (org+-add-clock-to-log-in-headline headline clock)
                                  rewrite))
                   ))))
    (when rewrite
      (delete-region (point-min) (point-max))
      (insert (org-element-interpret-data tree))
      t)))

(defun org+-agenda-copy-id_log-clock (&optional hap)
  "Copy all clock entries from ID_LOG headlines to ID headlines in agenda files.
If HAP is non-nil only search the header at point for ID_LOG.
Interactively HAP is the prefix argument."
  (interactive "P")
  (let ((agenda-files (org-agenda-files t))
        clock-map)
    (if hap
        (setq clock-map (org+-copy-id_log-clock-collect clock-map t))
      (dolist (file agenda-files)
        (with-current-file file
          (setq clock-map (org+-copy-id_log-clock-collect clock-map)))))
    (dolist (file agenda-files)
      (with-current-file file
          (org+-copy-id_log-clock clock-map)))))
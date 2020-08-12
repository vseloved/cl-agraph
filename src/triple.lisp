;;; CL-AGRAPH triple definition and parsing/serialization
;;; (c) Vsevolod Dyomkin. see LICENSE file for permissions

(in-package :agraph)
(named-readtables:in-readtable rutils-readtable)


;;; URIs & blank nodes

(defvar *prefixes*
  (let ((prefixes (make-hash-table :test 'equal)))
    (loop :for (prefix uri) :in '(("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")  
                                  ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")  
                                  ("owl" "http://www.w3.org/2002/07/owl#")  
                                  ("xsd" "http://www.w3.org/2001/XMLSchema#")  
                                  ("xs" "http://www.w3.org/2001/XMLSchema#")  
                                  ("fn" "http://www.w3.org/2005/xpath-functions#")  
                                  ("err" "http://www.w3.org/2005/xqt-errors#"))
          :do (set# prefix prefixes uri))
    prefixes)
  "Default uri prefixes.")

(defun register-prefix (prefix uri)
  "Register the mapping of PREFIX to URL for use in the function URI."
  (set# prefix *prefixes* uri))

(defun clear-prefix (prefix)
  "Unregister the mapping of PREFIX used in the function URI."
  (rem# prefix *prefixes*))

(defstruct (uri (:print-object (lambda (uri out)
                                 (format out "<~A>" (uri-name uri)))))
  name)

(defvar *uris*
  (make-hash-table :test 'equal)
  "The cache of processed uris.")

(defun uri (name)
  "Try to expand NAME using registered uri prefixes.
   Then create a URI object from it and return it.
   Also, cache the object in *URIS*."
  (dotable (prefix uri *prefixes*)
    (when (starts-with (strcat prefix ":") name)
      (:= name (strcat uri (slice name (1+ (length prefix)))))
      (return)))
  (getset# name *uris* (make-uri :name name)))

(defstruct (blank-node (:print-object (lambda (bn out)
                                        (format out "_:~A" (blank-node-name bn)))))
  (name (symbol-name (gensym "bn"))))

(progn
  (defvar *blank-nodes-repo* #h(equalp nil #h(equal))
          "Blank-node caches by repo.")
  (defvar *blank-nodes* (? *blank-nodes-repo* nil)
          "The current blank-node cache."))

(defun blank-node (name)
  "Get the blank-node for NAME from the cache or add it there."
  (getset# name *blank-nodes* (make-blank-node :name name)))


;;; triples

(defstruct (triple (:conc-name nil)
                   (:print-object print-triple))
  s p o g
  obj-lang obj-type)

(defun print-triple (tr &optional out)
  (format out "~A ~A ~A~@[~A~]~@[ ~A~] ."
          (s tr)
          (p tr)
          (typecase (o tr)
            (string (fmt "~S" (o tr)))
            (otherwise (o tr)))
          (cond-it ((obj-type tr) (fmt "^^~A" it))
                   ((obj-lang tr) (fmt "@~A" it)))
          (g tr)))

(defun ensure-subj (x)
  (etypecase x
    ((or uri blank-node) x)
    (string (uri x))))

(defun ensure-pred (x)
  (etypecase x
    (uri x)
    (blank-node (if *strict-mode*
                    (error 'type-error :datum x :expected-type '(or uri string))
                    x))
    (string (uri x))))

(defun ensure-obj (x &key lang type)
  (declare (ignore lang type))
  (etypecase x
    ((or uri blank-node string number boolean) x)))

(defun ensure-graph (x)
  (etypecase x
    ((or uri blank-node) x)
    (string (uri x))))

(defun <> (s p o &key g lang type)
  ""
  (make-triple
   :s (when s (ensure-subj s))
   :p (when p (ensure-pred p))
   :o (when o (ensure-obj o :lang lang :type type))
   :g (when g (ensure-graph g))
   :obj-lang lang
   :obj-type type))


;;; utils

(defun uri-or-blank-node (x)
  (if (stringp x)
      (uri x)
      (blank-node (second x))))

(defun triple-from-list (list)
  (with (((s p o &optional g) list)
         (lang nil)
         (type nil))
    (make-triple
     :s (uri-or-blank-node s)
     :p (uri-or-blank-node p)
     :o (case (atomize o)
          (:uri (uri (second o)))
          (:bn (blank-node (second o)))
          (:literal-string (prog1 (second o)
                             (when-it (getf o :lang)
                               (:= lang it))
                             (when-it (getf o :uriref)
                               (:= type (uri it)))))
          (t (uri o)))
     :g (when g (uri-or-blank-node g))
     :obj-lang lang
     :obj-type type)))


;;; nquads

(defparameter *strict-mode* nil
  "Parse according to the spec. Non-strict mode allows:
  - blank-nodes in predicates")

(in-package :cl-ntriples)

(defun parse-nq (stream)
  (list* ;; subject ::= uriref | nodeID
        (progn (consume-whitespace stream)
               (case (peek-char t stream)
                 (#\< (parse-uriref stream))
                 (#\_ (list :bn (parse-node-id stream)))
                 (otherwise (error "wrong char `~a' while parsing subject~%"
                                   (peek-char t stream)))))
        ;; predicate ::= uriref | nodeID
        (progn (consume-whitespace stream)
               (case (peek-char t stream)
                 (#\< (parse-uriref stream))
                 (#\_ (if agraph::*strict-mode*
                          (error "wrong char `_' while parsing subject~%")
                          (list :bn (parse-node-id stream))))
                 (otherwise (error "wrong char `~a' while parsing subject~%"
                                   (peek-char t stream)))))               
        ;; object ::= uriref | nodeID | literal
        (progn (consume-whitespace stream)
               (case (peek-char t stream)
                 (#\< (parse-uriref stream))
                 (#\_ (list :bn (parse-node-id stream)))
                 (#\" (parse-literal stream))
                 (otherwise (error "wrong character `~a' in while parsing object~%"
                                   (peek-char t stream)))))
        ;; graph ::= uriref | nodeID
        (progn (consume-whitespace stream)
               (flet ((consume-dot-after (graph)
                        (consume-whitespace stream)
                        (case (peek-char t stream)
                          (#\. (read-char stream)
                               (consume-whitespace stream))
                          (otherwise (error "wrong char `~a' while parsing dot~%"
                                            (peek-char t stream))))
                        (list graph)))
                 (case (peek-char t stream)
                   (#\< (consume-dot-after (parse-uriref stream)))
                   (#\_ (consume-dot-after (list :bn (parse-node-id stream))))
                   (#\. (read-char stream)
                        nil)
                   (otherwise (error "wrong char `~a' while parsing optional graph~%"
                                     (peek-char t stream))))))))

(defun agraph::parse-nq-line (stream)
  ;; consume white space if there is any at the start of a line
  (loop
    :for c = (peek-char t stream nil)
    :unless c :do (return-from agraph::parse-nq-line nil)
      :while (ntriple-ws-p c)
    :do (read-char stream))
  ;; check if this line is a comment or a triple
  (if (char= #\# (peek-char t stream))
      (progn				; consume comment line
        (loop :for c = (read-char stream nil)
              :unless c :do (return-from agraph::parse-nq-line 'comment)
              :until (ntriple-crlf-p c))
        (when (and (peek-char t stream nil)
                   (ntriple-crlf-p (peek-char t stream)))
          (read-char stream nil))
        'comment)
      (parse-nq stream)))

(defun agraph::parse-nq-doc (stream)
  (loop :while (peek-char t stream nil)
        :for line := (agraph::parse-nq-line stream) :while line
        :unless (eql line 'comment) :collect line))

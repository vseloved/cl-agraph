;;; CL-AGRAPH high-level api
;;; (c) Vsevolod Dyomkin. see LICENSE file for permissions

(in-package :agraph)
(named-readtables:in-readtable rutils-readtable)


;;; triple API

(defun get<> (&key s p o g ids limit offset)
  "Get triples specified by S/P/O/G or a list of IDS in the current repo.

   Supports the standard LIMIT and OFFSET parameters."
  (if ids
      (ag-req "/statements/id" (mapcan ^(list "id" (princ-to-string %)) ids))
      (ag-req "/statements" (list "subj" s "pred" p "obj" o "context" g
                                  "limit" limit "offset" offset))))

(defun add<> (&rest triples)
  "Add TRIPLES to the current repo."
  (values (parse-integer
           (ag-req "/statements" (list "continueOnError" t)
                   :method :post
                   :content-type "text/x-nquads"
                   :content (strjoin #\Newline (mapcar 'print-triple
                                                       triples))))))

(defun rem<> (&key s p o g lang type)
  "Remove the triples specified by S/P/O/G/lang/type from the current repo."
  (values (parse-integer
           (ag-req "/statements" (list "subj" s
                                       "pred" p
                                       "obj" (cond (lang (fmt "~A@~A" o langte))
                                                   (type (fmt "~A^^~A" o type))
                                                   (t o))
                                       "context" g)
                   :method :delete))))

(defun map<> (fn &rest args &key s p o g limit offset key (into 'list))
  "Collect the results of calling FN on the consecutive triples
   in the current repo.
   The triple pattern may be specified with the usual S/P/O/G.
   The INTO argument specifies the structure to collect the triples:
   list or array.

   The KEY argument allows executing BODY with TR bound to the result of
   calling KEY on the raw triple. The most commom use case for this
   is iterating over triple S/P/O/G.

   Supports the standard LIMIT and OFFSET parameters."
  (declare (ignorable s p o g limit offset))
  (let ((rez (ecase into
                ((nil list) ())
                (vector (vec)))))
    (remf args :into)
    (remf args :key)
    (loop :for tr :in (let ((*stream-mode* t))
                        (apply 'get<> args))
          :do (let ((cur (call fn (if key (call key tr) tr))))
                (case into
                  ((nil) #| do nothing |#)
                  (list (push cur rez))
                  (vector (pushx cur rez)))))
    rez))

(defmacro do<> ((tr &rest args &key s p o g limit offset key) &body body)
  "Iterate triples and execute BODY, assiging each one to the variable TR.
   The triple pattern may be specified with the usual S/P/O/G.

   The KEY argument allows executing BODY with TR bound to the result of
   calling KEY on the raw triple. The most commom use case for this
   is iterating over triple S/P/O/G.

   Supports the standard LIMIT and OFFSET parameters."
  (declare (ignorable s p o g limit offset key))
  `(block nil
     (map<> (lambda (,tr) ,@body)
            :into nil ,@args)))

(defun count<> (&key g)
  "Count the number of triples in the current repo.
   Possibly, limit to only the graph G."
  (values (parse-integer (ag-req "/size" (list "context" g)))))


;;; sparql

(defun sparql (query &rest args &key limit offset want-triples)
  "Run sparql QUERY on the current repo.
   Returns the results either as a list of hass-tables
   having the keys corrsponding to the sparql variables.
   Or, if WANT-TRIPLES, expects that the results will contain the
   S/P/O/G fields that will be used to populate and return triples
   instead of hash-tables.

   The QUERY may be a string or a symbolic sparql tree.

   Supports the standard LIMIT and OFFSET parameters."
  (declare (ignorable limit offset))
  (remf args :want-triples)
  (with ((*stream-mode* t)
         (in (ag-req (fmt "~@[?~A~]"
                          (strjoin "&" (mapcar ^(fmt "~(~A~)=~A" (first %)
                                                     (case (second %)
                                                       ((t) "true")
                                                       ((nil) "false")
                                                       (otherwise (second %))))
                                               (append (group 2 args)
                                                       (when (symbolp query)
                                                         (list "query" query))))))
                     ()
                     :content (substitute #\Space #\Newline
                                          (etypecase query
                                            (symbol "")
                                            (string query)
                                            (list (generate-sparql query nil))))
                     :method :post
                     :content-type "application/sparql-query"
                     :accept "application/sparql-results+json-rows")))
    (loop :while (peek-char t in nil)
          :for cur := (mapkv (lambda (k v)
                               (let ((val (? v "value")))
                                 (ecase (mkeyw (? v "type"))
                                   (:literal (cond-it
                                               ((? v "datatype")
                                                (list val :type it))
                                               ((? v "xml:lang")
                                                (list val :lang it))
                                               (t
                                                val)))
                                   (:bnode (blank-node val))
                                   (:uri (uri val)))))
                             (yason:parse in))
          :collect (if want-triples
                       (let ((o (? cur "O")))
                         (apply 'triple
                                (? cur "S")
                                (? cur "P")
                                (atomize o)
                                (? cur "G")
                                (when (listp o)
                                  (rest o))))
                       cur))))


;;; additional utils

(defun storedp (tr)
  "Check if triple TR exists in the current repo."
  (assert (triple-p tr))
  (when (get<> :s (s tr) :p (p tr) :o (o tr) :limit 1)
    t))

(defun duplicates (&key mode)
  "Get all duplicate triples in the current repo."
  (ag-req "/statements/duplicates" (list "mode" mode)))

(defun uniques (&key column)
  "Get all unique triples in the current repo."
  (ag-req "/statements/unique" (list "column" column)))

(defun graphs ()
  "Get all the graphs in the current repo."
  (ag-req "/contexts" nil))


;;; repo API

(defun create-repo (repo)
  "Create REPO."
  (with-ag (:repo repo :sessionp nil
            :scheme (ag-scheme *ag*) :host (ag-host *ag*) :port (ag-port *ag*)
            :user (ag-user *ag*) :pass (ag-pass *ag*) :catalog (ag-catalog *ag*))
    (when (= 204 (2nd (ag-req "" () :method :put)))
      t)))

(defun delete-repo (repo)
  "Delete REPO."
  (with-ag (:repo repo :sessionp nil
            :scheme (ag-scheme *ag*) :host (ag-host *ag*) :port (ag-port *ag*)
            :user (ag-user *ag*) :pass (ag-pass *ag*) :catalog (ag-catalog *ag*))
    (when (string= "true" (ag-req "" () :method :delete))
      t)))


;;; transaction API

(defun begin-trans ()
  "Begin a transaction."
  (assert (ag-session *ag*))
  (ag-req "/begin" nil :method :post))

(defun commit ()
  "Commit current transaction."
  (assert (ag-session *ag*))
  (ag-req "/commit" nil :method :post))

(defun rollback ()
  "Rollback current transaction."
  (assert (ag-session *ag*))
  (ag-req "/rollback" nil :method :post))

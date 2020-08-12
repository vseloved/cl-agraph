;;; CL-AGRAPH core
;;; (c) Vsevolod Dyomkin. see LICENSE file for permissions

(in-package :agraph)
(named-readtables:in-readtable rutils-readtable)


(defstruct (ag-config (:conc-name ag-))
  (scheme :http)
  (host "localhost")
  (port 10035)
  catalog
  repo
  (user "test")
  (pass "xyzzy")
  session)

(defvar *ag* (make-ag-config)
  "Current agraph config.")

(defparameter *stream-mode* nil
  "Ask for a stream of results from an agraph request.")

(define-condition ag-error (error)
  ((code :initarg :code :accessor ag-code)
   (msg :initarg :msg :accessor ag-msg))
  (:report (lambda (e out)
             (format out "AG-ERROR ~A: ~A"
                     (ag-code e) (ag-msg e)))))

(defun open-ag (&key scheme host port catalog repo user pass sessionp timeout)
  "Configure *AG* with SCHEME/HOST/POST/CATALOG/REPO/USER/PASS.
   If SESSIONP, establish a session (optionally specify its lifetime as TIMEOUT)
   and returns it. Otherwise, returns NIL."
  (:= *ag* (apply 'make-ag-config
                  (append (when scheme (list :scheme scheme))
                          (when host (list :host host))
                          (when port (list :port port))
                          (when catalog (list :catalog catalog))
                          (when repo (list :repo repo))
                          (when user (list :user user))
                          (when pass (list :pass pass)))))
  (:= *blank-nodes* (getset# *ag* *blank-nodes-repo* #h(equal)))
  (when sessionp
    (:= (ag-session *ag*) (ag-req "/session"
                                  (when timeout (list "lifetime" timeout))
                                  :method :post))))

(defun close-ag ()
  "Terminate the current agraph session (if there's any). Clear *AG*."
  (when (and *ag* (ag-session *ag*))
    (ag-req "/session/close" nil :method :post))
  (:= *ag* nil
      *blank-nodes* (? *blank-nodes-repo* nil))
  t)

(defmacro with-ag ((&key scheme host port catalog repo user pass
                      (sessionp t) timeout)
                   &body body)
  "OPEN-AG and, if SESSIONP (default), establish a session
   (its lifetime may be specified with TIMEOUT).
   Execute BODY.
   CLOSE-AG.
   If the session is terminated, try to reestablish it once."
  (once-only (scheme host port catalog repo user pass sessionp timeout)
    (let ((rez (gensym)))
      `(let (*ag*)
         (block nil
           (unwind-protect
                (progn
                  (open-ag :scheme ,scheme 
                           :host ,host
                           :port ,port
                           :catalog ,catalog
                           :repo ,repo
                           :user ,user
                           :pass ,pass
                           :sessionp ,sessionp
                           :timeout ,timeout)
                  (handler-case (let ((,rez (progn ,@body)))
                                  (when ,sessionp (commit))
                                  ,rez)
                    (usocket:connection-refused-error ()
                      (warn "The current agraph sesssion is closed. Restarting it.")
                      (open-ag :scheme ,scheme 
                               :host ,host
                               :port ,port
                               :catalog ,catalog
                               :repo ,repo
                               :user ,user
                               :pass ,pass
                               :timeout ,timeout
                               :sessionp t)
                      (let ((,rez (progn ,@body)))
                        (when ,sessionp (commit))
                        ,rez))))
             (close-ag)))))))

(defun ag-params (args)
  "Transform ARGS to pairs suitable for passing as agraph HTTP query parameters."
  (remove nil
          (apply 'mapcar ^(when %% (cons % (prin1-to-string %%)))
                 (loop :for (k v) :on args :by #'cddr :while k
                       :collect k :into ks
                       :collect v :into vs
                       :finally (return (list ks vs))))))

(defun ag-req (query params
               &key (ag *ag*) (method :get) content accept content-type)
  "Run the Aagraph QUERY with PARAMS processed by ag-params
   against the current session or the current repository.

   METHOD, ACCEPT, CONTENT, and CONTENT-TYPE may be chnaged.

   In case of 2xx HTTP return code, returns the result
   as triples (if nquads content-type is returned)
   or raw string result and the return code as a second value.

   In case 4xx or 5xx return codes, signals AG-ERROR for them."
  (with ((rez code headers
              (drakma:http-request
               (if-it (ag-session ag)
                      (fmt "~A~@[~A~]" it query)
                      (fmt "~A://~A:~A/~@[catalogs/~A/~]repositories/~A~@[~A~]"
                           (ag-scheme ag)
                           (ag-host ag)
                           (ag-port ag)
                           (ag-catalog ag)
                           (ag-repo ag)
                           query))
               :method method
               :content content
               :content-type content-type
               :parameters (ag-params params)
               :basic-authorization (list (ag-user ag) (ag-pass ag))
               :accept (or accept "text/x-nquads, */*")
               :want-stream *stream-mode*)))
    (ecase (floor code 100)
      ((4 5) (error 'ag-error :code code
                              :msg (if (streamp rez)
                                       (asdf::slurp-stream-string rez)
                                       rez)))
      (2 (if (starts-with "text/x-nquads" (assoc1 :Content-Type headers))
             (mapcar 'triple-from-list
                     (if *stream-mode* (parse-nq-doc rez)
                         (with-input-from-string (in rez)
                           (parse-nq-doc in))))
             (values rez
                     code))))))

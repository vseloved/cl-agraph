;;; CL-AGRAPH package definition
;;; (c) Vsevolod Dyomkin. see LICENSE file for permissions

(in-package :cl-user)


(defpackage #:agraph
  (:use :common-lisp #:rtl
        #+dev #:should-test)
  (:local-nicknames (#:re #:ppcre))
  (:export #:ag-error

           #:*ag*
           #:ag-config
           #:make-ag-config
           #:ag-scheme
           #:ag-host
           #:ag-port
           #:ag-catalog
           #:ag-repo
           #:ag-session

           #:open-ag
           #:close-ag
           #:with-ag
           
           #:ag-req
           #:ag-params

           #:triple
           #:s
           #:p
           #:o
           #:g
           #:obj-lang
           #:obj-type

           #:*prefixes*
           #:register-prefix
           #:clear-prefix

           #:blank-node
           #:uri
           
           #:create-repo
           #:delete-repo
           
           #:<>
           #:get<>
           #:add<>
           #:rem<>
           #:map<>
           #:do<>
           #:count<>
           #:storedp

           #:sparql

           #:begin-trans
           #:commit
           #:rollback
           #:with-trans
           
           #:graphs
           #:uniques
           #:duplicates))

;;; CL-AGRAPH system definition
;;; (c) Vsevolod Dyomkin.  See LICENSE file for permissions

(in-package :asdf-user)

(defsystem #:cl-agraph
  :version (:read-file-line "version.txt" :at 0)
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "A minimal portable Lisp client for AllegroGraph"
  :depends-on (#:rutils #:cl-ppcre #:drakma #:cl-ntriples #:yason
                        #+dev #:should-test)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "triple")
                             (:file "sparql")
                             (:file "agraph")
                             (:file "api")))
               #+dev (:file "test/test-sparql")))


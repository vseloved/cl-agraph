;;; CL-AGRAPH core test
;;; (c) Vsevolod Dyomkin. see LICENSE file for permissions

(in-package :agraph)

(deftest triple ()
  (should be string= "http://foo.com/"
          (register-prefix "foo" "http://foo.com/"))
  (let ((tr (<> (make-blank-node) "rdf:type" (uri "foo:quux"))))
    (should be eql 'blank-node (type-of (s tr)))
    (should be eql 'uri (type-of (p tr)))
    (should be eql 'uri (type-of (o tr)))
    (should be string= "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            (uri-name (p tr)))
    (should be string= "http://foo.com/quux"
            (uri-name (o tr)))))

(deftest <> ()
  (register-prefix "foo" "http://foo.com/")
  (let ((repo (gensym)))
    (with-ag (:repo "test" :port 12345 :sessionp nil)
      (should be true (create-repo repo)))
    (with-ag (:repo repo :port 12345)
      (should be = 2
              (let ((subj (make-blank-node)))
                (add<> (<> subj "rdf:type" (uri "foo:bar"))
                       (<> subj "foo:baz" "quux" :lang "en"))))
      (let ((trs (get<>)))
        (should be = 2 (length trs))
        (should be eql (first trs) (second trs)))
      (should be = 0 (rem<> :g (uri "foo:bar")))
      (should be = 1 (rem<> :o (uri "foo:bar")))
      (should be = 1 (length (get<>)))
      (should be = 1 (count<>)))
    (with-ag (:repo "test" :port 12345 :sessionp nil)
      (should be true (delete-repo repo)))))
        
(deftest transactions ()
  (register-prefix "foo" "http://foo.com/")
  (let ((repo (gensym)))
    (with-ag (:repo "test" :port 12345 :sessionp nil)
      (should be true (create-repo repo)))
    (with-ag (:repo repo :port 12345)
      (let ((subj (make-blank-node)))
        (add<> (<> subj "rdf:type" (uri "foo:bar"))
               (<> subj "foo:baz" "quux" :lang "en")))
      (rollback)
      (should be zerop (count<>))
      (let ((subj (make-blank-node)))
        (add<> (<> subj "rdf:type" (uri "foo:bar"))
               (<> subj "foo:baz" "quux" :lang "en"))))
    (with-ag (:repo repo :port 12345)
      (should be = 2 (count<>)))
    (with-ag (:repo "test" :port 12345 :sessionp nil)
      (should be true (delete-repo repo)))))

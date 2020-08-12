;;; CL-AGRAPH symbolic SPARQL translator
;;; (c) Vsevolod Dyomkin. see LICENSE file for permissions

(in-package :agraph)
(named-readtables:in-readtable rutils-readtable)


(defun generate-sparql (tree out)
  "Transform symbolic sparql TREE into a sparql string and print it ot OUT."
  (let ((return-string (null out)))
    (when return-string (:= out (make-string-output-stream)))
    (dolist (node tree)
      (etypecase node
        (symbol (format out "~A " node))
        (list (process-expr node out :top-level t)))
      (terpri out))
    (when return-string
      (get-output-stream-string out))))

(defun process-expr (tree out &key top-level)
  (let ((return-string (null out)))
    (when return-string (:= out (make-string-output-stream)))
  (typecase (first tree)
    (keyword (when (and top-level (eql :graph (first tree))) (format out "{~%"))
             (process-custom (first tree) (rest tree) out :top-level top-level)
             (when (and top-level (eql :graph (first tree))) (format out " }")))
    (symbol (when top-level (format out "{~%"))
            (process-triple-pattern tree out)
            (when top-level (format out " }")))
    (list (format out "{ ~{~A ~}}"
                  (mapcar ^(process-expr % nil)
                          tree))))
    (when return-string
      (get-output-stream-string out))))

(defun process-triple-pattern (tree out)
  (with (((s p &optional o) tree)
         (return-string (null out)))
    (when return-string (:= out (make-string-output-stream)))
    (format out "~A " s)
    (labels ((print-o (o)
               (etypecase o
                 (null (error "Object shouldn't be empty in: ~A" tree))
                 (symbol (format nil "~A " o))
                 (atom (format nil "~S " o))
                 (list (if (keywordp (first o))
                           (process-expr o nil)
                           (format nil "~{~A~^,~%~}" o)))))
             (format-p-o (out p-o)
               (format out "~A ~A"
                       (lt p-o) (print-o (rt p-o)))))
      (if (listp p)
          (format out "~{~A ~^;~%~}"
                  (mapcar ^(format-p-o nil %)
                          (group 2 p)))
          (format-p-o out (pair p o))))
    (format out ".~%")
    (when return-string
      (get-output-stream-string out))))
  
(defgeneric process-custom (key tree out &key)
  (:method :around (key tree out &key)
    (if (null out)
        (with-output-to-string (out)
          (call-next-method key tree out))
        (call-next-method)))
  (:method (key tree out &key)
    (format out "~(~A~)(~{~A~^,~})" key
            (mapcar ^(etypecase %
                       (string (fmt "~S" %))
                       (atom (fmt "~A" %)))
                    tree)))
  (:method ((key (eql :prefix)) tree out &key)
    (assert (dyadic tree))
    (format out "PREFIX ~A: ~A" (lt tree) (rt tree)))
  (:method ((key (eql :union)) tree out &key)
    (format out "{ ~{~A~^~%UNION~%~} }"
            (mapcar ^(process-expr % nil :top-level t)
                    tree)))
  (:method ((key (eql :filter)) tree out &key)
    (assert (single tree))
    (format out "FILTER ( ~A )~%"
            (process-expr (first tree) nil :top-level nil)))
  (:method ((key (eql :optional)) tree out &key)
    (assert (single tree))
    (format out "OPTIONAL ~A"
            (process-expr (first tree) nil :top-level t)))
  (:method ((key (eql :|^|)) tree out &key)
    (assert (and (dyadic tree)
                 (stringp (first tree))
                 (symbolp (second tree))))
    (format out "~S^^~A" (first tree) (second tree)))
  (:method ((key (eql :@)) tree out &key)
    (assert (and (dyadic tree)
                 (every 'stringp tree)))
    (format out "~S@~A" (first tree) (second tree)))
  (:method ((key (eql :graph)) tree out &key)
    (assert (dyadic tree))
    (format out "GRAPH ~A " (first tree))
    (process-expr (second tree) out :top-level t))
  (:method ((key (eql :bind)) tree out &key)
    (assert (dyadic tree))
    (format out "BIND (~A AS ~A)"
            (process-expr (first tree) nil :top-level t)
            (second tree)))
  (:method ((key (eql :!)) tree out &key)
    (assert (single tree))
    (format out "!")
    (process-custom (first (first tree)) (rest (first tree)) out))
  (:method ((key (eql :\|\|)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :&&)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :>)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :>=)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :<)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :<=)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :=)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :!=)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :+)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :-)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :*)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level))
  (:method ((key (eql :/)) tree out &key top-level)
    (process-arithm key tree out :top-level top-level)))

(defun process-arithm (fn tree out &key top-level)
  (flet ((print-side (side)
           (etypecase side
             (symbol (fmt "~A" side))
             (atom (fmt "~S" side))
             (list (process-custom (first side) (rest side) nil)))))
    (unless top-level (format out "("))
    (format out (fmt "~~{~~A ~~^~A ~~}" fn)
            (mapcar #'print-side tree))
    (unless top-level (format out ")"))))

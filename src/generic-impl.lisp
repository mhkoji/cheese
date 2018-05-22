(defpackage :cheese.generic-impl
  (:use :cl))
(in-package :cheese.generic-impl)

(defun cheese.generic::cons (a b)
  (lambda (m)
    (cond ((eq m 'type) 'cons)
          ((eq m 'car) a)
          ((eq m 'cdr) b))))

(defun cheese.generic::type-of (c)
  (funcall c 'type))

(defun cheese.generic::car (c)
  ;; TODO: Ensure cons
  (funcall c 'car))

(defun cheese.generic::cdr (c)
  ;; TODO: Ensure cons
  (funcall c 'cdr))


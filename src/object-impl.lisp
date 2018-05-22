(defpackage :cheese.object-impl
  (:use :cl))
(in-package :cheese.object-impl)
;;; "(+ 1 2)" -> (cons '+ (cons 1 (cons 2 nil)))

(defun cheese.object::make-symbol (symbol-name package)
  (cheese.generic::cons 'symbol
                       cheese.generic::cons symbol-name package))

(defun cheese.object::symbol-name (symbol)
  ;; TODO: Ensure that the arg is of type symbol
  (cheese.generic::car (cheese.generic::cdr symbol)))

(defun cheese.object::symbol-package (symbol)
  ;; TODO: Ensure that the arg is of type symbol
  (cheese.generic::cdr (cheese.generic::cdr symbol)))


(defun cheese.object::make-package (package-name)
  (declare (ignore package-name))
  (let ((symbols nil))
    (let ((this nil))
      (setq this
            (lambda (m &rest args)
              (cond ((eq m 'intern)
                     (let ((symbol-name (car args)))
                       (or (find symbol-name symbols
                                 :key #'cheese.generic::symbol-name)
                           (let ((sym (cheese.generic::make-symbol
                                       symbol-name
                                       this)))
                             (push sym symbols)
                             sym)))))))
      this)))

(defun cheese.object::intern! (name package)
  (funcall package 'intern name))


(defun eval-arg-list (evaluator args env)
  nil)

(defun extend-environment (env params args)
  )

(defun cheese.object::apply (evaluator proc args)
  (let ((env (cheese.object::procedure-environment proc))
        (body (cheese.object::procedure-body proc))
        (params (cheese.object::procedure-parameters proc)))
    (funcall evaluator body (extend-environment env params args))))

(defun cheese.object::eval (expr env)
  (let ((type (cheese.generic::type-of expr)))
    (cond ((eq type 'cons)
           ;; application
           (let ((proc (cheese.object::eval
                        (cheese.object::car expr)
                        env))
                 (args (eval-arg-list
                        #'cheese.object::eval
                        (cheese.object::cdr expr)
                        env)))
             (cheese.object::apply #'cheese.object::eval proc args))))))

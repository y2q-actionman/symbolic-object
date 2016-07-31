(in-package :cl-user)

;; ':=' -- 通常関数定義
(defun hoge (x)
  (1+ x))


;; Unique[] -- symbol作ればいいはず。
(defun make-symbolic-object (&rest gensym-args)
  (apply #'gensym gensym-args))

;; '^:=' -- 関数を内側のobjectに持たせる -> symbol-plist に持たせる
(defun add-symbolic-object-method (name s-object &optional function)
  (setf (get s-object name) function))

(defun call-symbolic-object-method (name s-object &rest args)
  (apply (get s-object name) args))

(add-symbolic-object-method 'test-func 'test-obj
			    (lambda () (format t "Hello symbolic-method!~%")))
(call-symbolic-object-method 'test-func 'test-obj)

;;; ガワ Macro 追加

;; 通常関数のように call する。

#|
;; 通常method のみversion
(defun add-global-function-for-symbolic-object-method (defun-name &optional (method-name defun-name))
  (setf (fdefinition defun-name)
	(lambda (obj &rest args)
	  (apply #'call-symbolic-object-method method-name obj args))))
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun add-global-function-for-symbolic-object-method (defun-name &optional (method-name defun-name))
  (cond ((and (listp defun-name)	; setf function
	      (eq (first defun-name) 'setf))
	 (setf (fdefinition defun-name)
	       (lambda (val obj)
		 (call-symbolic-object-method method-name obj val))))
	(t
	 (setf (fdefinition defun-name)
	       (lambda (obj &rest args)
		 (apply #'call-symbolic-object-method method-name obj args))))))
)

(add-global-function-for-symbolic-object-method 'test-func)
(test-func 'test-obj)

;; defun のように定義する。
(defmacro define-symbolic-object-method (name s-object lambda-list &body body)
  `(progn
     (add-global-function-for-symbolic-object-method ',name)
     (add-symbolic-object-method ',name ,s-object
				 (lambda (,@lambda-list) ,@body))))

(define-symbolic-object-method test-hello 'test-obj ()
  (format t "Hello symbolic-method with define-symbolic-object-method!~%"))

(test-hello 'test-obj)

(define-symbolic-object-method test-hello-to 'test-obj (name)
  (format t "Hello ~A!~%" name))

(test-hello-to 'test-obj "hogehoge")


;; 逆順で call (おまけ)
(defun add-global-function-for-symbolic-object (s-object)
  (setf (fdefinition s-object)
	(lambda (symbolic-method-name &rest args)
	  (apply #'call-symbolic-object-method symbolic-method-name s-object args))))

(add-global-function-for-symbolic-object 'test-obj)
(test-obj 'test-hello)
  

;;; class
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun merge-spec-list (spec-list new-spec-list)
    (flet ((spec-name (spec)
	     (symbol-name (first spec))))
      (let* ((sorted-new-specs (stable-sort (copy-list new-spec-list)
					    #'string< :key #'spec-name))
	     (merged (merge 'list (copy-list spec-list)
			    sorted-new-specs #'string< :key #'spec-name)))
	(delete-duplicates merged :test 'eq :key #'spec-name))))

  (defvar *check-global-function-existence* t)
  )

(defmacro define-symbolic-object-class (name super-class-names fields methods)
  (let* ((this-class-field-specs
	  ;; makes some global defs
	  (loop for field-def in fields
	     as field-name = (first field-def)
	     as reader-name = (intern (format nil "~S-reader" field-name))
	     as writer-name = (intern (format nil "~S-writer" field-name))
	     ;; global defs
	     when (and *check-global-function-existence*
		       (not (fboundp field-name)))
	     do (add-global-function-for-symbolic-object-method field-name reader-name)
	     when (and *check-global-function-existence*
		       (not (fboundp `(setf ,field-name))))
	     do (add-global-function-for-symbolic-object-method `(setf ,field-name) writer-name)
	     ;; spec 回収
	     collect `(,field-name ,reader-name ,writer-name ,@(rest field-def))))
	 (all-field-specs
	  ;; merge field specs
	  (loop for super-class-name in super-class-names
	     as super-class-field-specs = (get super-class-name :symbolic-class-field-specs)
	     as all-specs = super-class-field-specs
	     then (merge-spec-list all-specs super-class-field-specs)
	     finally
	       (return (merge-spec-list all-specs this-class-field-specs))))
	 (field-names  (mapcar #'car all-field-specs))
	 (method-specs nil))
    (setf (get name :symbolic-class-field-specs) all-field-specs)
    ;; generate method-making functions
    `(flet (,@(loop for (method-name method-lambda-list . method-body) in methods
		 as method-maker-name = (gensym (format nil "method-maker-~A" method-name))
		 ;; global defs
		 when (and *check-global-function-existence*
			   (not (fboundp method-name)))
		 do (add-global-function-for-symbolic-object-method method-name)
		 ;; spec 回収
		 do (push `(list ',method-name #',method-maker-name) method-specs)
		 ;; flet form
		 collect `(,method-maker-name (,@field-names)
					      (declare (type symbol ,@field-names))
					      (lambda (,@method-lambda-list)
						(let (,@(loop for f in field-names
							   collect `(,f (symbol-value ,f))))
						  (declare (ignorable ,@field-names))
						  ,@method-body)))
		 into flet-defs
		 ;;
		 finally
		   (setf method-specs (nreverse method-specs))
		   (return flet-defs)))
       ;; merge and set method specs
       (loop with this-class-method-specs = nil
	  for class-name in ',super-class-names
	  do (setf this-class-method-specs (merge-spec-list this-class-method-specs
							    (get class-name :symbolic-class-method-specs)))
	  finally
	    (setf (get ',name :symbolic-class-method-specs)
		  (merge-spec-list this-class-method-specs (list ,@method-specs)))))))
;; TODO: use parse-lambda in alexandria
;; TODO: add structure for 'field-spec' and 'method-spec'

(defun apply-symbolic-object-class (s-object symbolic-class-name)
  (let* ((field-specs (get symbolic-class-name :symbolic-class-field-specs))
	 (method-specs (get symbolic-class-name :symbolic-class-method-specs))
	 (namespace-package-name (format nil "~S" s-object))
	 (namespace (or (find-package namespace-package-name)
			(make-package namespace-package-name :use nil)))
	 (field-value-symbols nil))
    ;; * field 生成
    (loop for (field-name reader-name writer-name . field-inits) in field-specs
       as sym = (intern field-name namespace)
       do (push sym field-value-symbols)
       ;; initialize
       if field-inits
       do (setf (symbol-value sym) (first field-inits))
       else
       do (makunbound sym)
       ;; reader/writer
       do (add-symbolic-object-method reader-name s-object
				      (let ((field-symbol sym))
					(lambda () (symbol-value field-symbol))))
	 (add-symbolic-object-method writer-name s-object
				     (let ((field-symbol sym))
				       (lambda (val) (setf (symbol-value field-symbol) val))))
       finally
         (setf field-value-symbols (nreverse field-value-symbols)))
    ;; * method 生成
    (loop for (method-name method-maker-func) in method-specs
       do (add-symbolic-object-method method-name s-object
				      (apply method-maker-func field-value-symbols)))
    s-object))

(define-symbolic-object-class test-class ()
  ((field-a 1)
   (field-b nil)
   (field-c))				; unbound
  ((method-hello ()
		 (format t "Hello, class world!~%"))
   (method-describe-test-class ()
			       (format t "~&field-a = ~A, field-b = ~A, field-c = ~A~%"
				       field-a field-b field-c))))

(apply-symbolic-object-class 'test-obj 'test-class)

;; fields
(format t "~& a ~A, b ~A, c ~A~%"
	(field-a 'test-obj) (field-b 'test-obj)
	(multiple-value-list (ignore-errors (field-c 'test-obj))))
(setf (field-a 'test-obj) "a")
(setf (field-b 'test-obj) "b")
(setf (field-c 'test-obj) "c")
(method-describe-test-class 'test-obj)

;; method call
(call-symbolic-object-method 'method-hello 'test-obj)
(method-hello 'test-obj)
(call-symbolic-object-method 'method-describe-test-class 'test-obj)
(method-describe-test-class 'test-obj)


;; inheritance
(define-symbolic-object-class test-class-2 (test-class)
  ((field-b 10)				; override
   (field-c 100))			; override
  ((method-all-sum (&rest args)
		   (apply #'+ field-a field-b field-c args))))

(apply-symbolic-object-class 'test-obj-2 'test-class-2)

(call-symbolic-object-method 'method-hello 'test-obj-2)
(method-hello 'test-obj-2)
(call-symbolic-object-method 'method-describe-test-class 'test-obj-2)
(method-describe-test-class 'test-obj-2)

(call-symbolic-object-method 'method-all-sum 'test-obj-2)
(method-all-sum 'test-obj-2)
(call-symbolic-object-method 'method-all-sum 'test-obj-2 1000)
(method-all-sum 'test-obj-2 1000)

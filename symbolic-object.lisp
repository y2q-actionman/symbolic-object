(in-package :cl-user)

;; ':=' -- 通常関数定義
(defun hoge (x)
  (1+ x))


;; Unique[] -- symbol作ればいいはず。
(defun make-symbolic-object (&rest gensym-args)
  (apply #'gensym gensym-args))

;; '^:=' -- 関数を内側のobjectに持たせる -> symbol-plist に持たせる
(defun add-symbolic-object-method (s-object name &optional function)
  (cond ((null function)
	 (setf (get s-object name) (get name :symbolic-method))) ; interface for define-symbolic-object-method 
	(t
	 (setf (get s-object name) function))))

(defun call-symbolic-method (s-object name &rest args)
  (apply (get s-object name) args))

(add-symbolic-object-method 'test-obj 'test-func
			    (lambda () (format nil "Hello symbolic-method!")))

(call-symbolic-method 'test-obj 'test-func)

;; ガワ Macro 追加
(defmacro define-symbolic-object-method (name lambda-list &body body)
  `(setf (get ',name :symbolic-method)
	 (lambda (,@lambda-list) ,@body)))

(define-symbolic-object-method test-hello ()
  (format nil "Hello symbolic call to with define-symbolic-object-method!"))

(add-symbolic-object-method 'test-obj 'test-hello)

(call-symbolic-method 'test-obj 'test-hello)


;; おまけ：通常関数のように call する。
(defun add-global-function-to-symbolic-object-method (defun-name &optional (method-name defun-name))
  (cond ((and (listp defun-name)	; setf function
	      (eq (first defun-name) 'setf))
	 (setf (fdefinition defun-name)
	       (lambda (val obj)
		 (call-symbolic-method obj method-name val))))
	(t
	 (setf (fdefinition defun-name)
	       (lambda (obj &rest args)
		 (apply #'call-symbolic-method obj method-name args))))))

(add-global-function-to-symbolic-object-method 'test-hello)

(test-hello 'test-obj)
  

;; class
(defun find-symbolic-object-namespace (s-object)
  (let ((namespace-package-name (format nil "~S" s-object)))
    (or (find-package namespace-package-name)
	(make-package namespace-package-name :use nil))))

(defmacro define-symbolic-object-class (name super-class-names fields methods)
  (let ((field-specs nil))
    ;; makes some global defs
    (loop for field-def in fields
       as field-name = (first field-def)
       as reader-name = (intern (format nil "~S-reader" field-name))
       as writer-name = (intern (format nil "~S-writer" field-name))
       ;; global defs
       unless (fboundp field-name)
       do (add-global-function-to-symbolic-object-method field-name reader-name)
       unless (fboundp `(setf ,field-name))
       do (add-global-function-to-symbolic-object-method `(setf ,field-name) writer-name)
       ;; reader/writer を作る関数を作る
       do (let ((reader-name reader-name)
		(writer-name writer-name))
	    (flet ((make-reader/writer (s-object sym)
		     (add-symbolic-object-method s-object reader-name
						 (lambda () (symbol-value sym)))
		     (add-symbolic-object-method s-object writer-name
						 (lambda (val) (setf (symbol-value sym) val)))))
	      (push `(,field-name ,#'make-reader/writer ,@(rest field-def)) field-specs)))
       ;; 
       finally (setf field-specs (nreverse field-specs)))
    `(loop for class-name in ',super-class-names
	append (get class-name :symbolic-class-field-specs) into this-class-field-specs
	append (get class-name :symbolic-class-methods) into this-class-methods
	finally
	  (setf (get ',name :symbolic-class-field-specs) (append this-class-field-specs ',field-specs)
		(get ',name :symbolic-class-methods) (append this-class-methods ',methods)))))

(defun apply-symbolic-object-class (s-object symbolic-class-name)
  (let* ((field-specs (get symbolic-class-name :symbolic-class-field-specs))
	 (methods (get symbolic-class-name :symbolic-class-methods))
	 (namespace-package-name (format nil "~S" s-object))
	 (namespace (or (find-package namespace-package-name)
			(make-package namespace-package-name :use nil))))
    ;; * field 生成
    (loop for (field-name reader/writer-maker . field-inits) in field-specs
       as sym = (intern field-name namespace)
       ;; initialize
       when field-inits
       do (setf (symbol-value sym) (first field-inits))
       ;; reader/writer
       do (funcall reader/writer-maker s-object sym))
    ;; ;; * method 生成
    ;; (loop for (method-name method-lambda-list . method-body) in methods ; TODO: use parse-lambda in alexandria
    ;;    do (add-symbolic-object-method s-object method-name
    ;; 				      `(lambda (,@method-lambda-list) ,@method-body))
    ;; 	 (add-global-function-to-symbolic-object-method method-name)))
    s-object))

(define-symbolic-object-class test-class ()
  ((field-a 1)
   (field-b nil)
   (field-c))				; unbound
  ((class-method-hello () "Hello, class world!")
   (class-method-describe () (format t "~&field-a = ~A, field-b = ~A, field-c = ~A~%"
				     field-a field-b field-c))))

(define-symbolic-object-class test-class-2 (test-class)
  ((field-b 10)				; override
   (field-c 100))			; override
  ((class-method-all-sum (&rest args)
			 (apply #'+ field-a field-b field-c args))))

(apply-symbolic-object-class 'test-obj 'test-class)

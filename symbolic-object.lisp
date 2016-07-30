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
(defmacro define-symbolic-object-class (name super-class-names fields methods)
  `(loop for class-name in ',super-class-names
      append (get class-name :symbolic-class-fields) into this-class-fields
      append (get class-name :symbolic-class-methods) into this-class-methods
      finally
	(setf (get ',name :symbolic-class-fields) (append this-class-fields ',fields)
	      (get ',name :symbolic-class-methods) (append this-class-methods ',methods))))

(defun add-symbolic-object-fields (s-object fields)
  (let* ((namespace-package-name (format nil "~S" s-object))
	 (namespace (or (find-package namespace-package-name)
			(make-package namespace-package-name :use nil)))
	 (field-alist nil))
    ;; * field 生成
    ;; package を新しい名前空間にする。
    (loop for field-def in fields
       as field-name = (first field-def)
       as sym = (intern field-name namespace)
       ;; initialize
       when (>= (length field-def) 2)	; TODO: use length= in alexandria
       do (setf (symbol-value sym) (second field-def))
       ;; reader/writer
       do (let ((reader-name (intern (format nil "~S-reader" field-name))))
	    (add-symbolic-object-method s-object reader-name
					(let ((sym sym))
					  (lambda () (symbol-value sym))))
	    (add-global-function-to-symbolic-object-method field-name reader-name))
       do (let ((writer-name (intern (format nil "~S-writer" field-name))))
	    (add-symbolic-object-method s-object writer-name
					(let ((sym sym))
					  (lambda (val) (setf (symbol-value sym) val))))
	    (add-global-function-to-symbolic-object-method `(setf ,field-name) writer-name))
       ;; make renaming entry
       do (setf field-alist (acons field-name sym field-alist))))
  s-object)

(defmacro apply-symbolic-object-class (s-object symbolic-class-name)
  (let* ((fields (get symbolic-class-name :symbolic-class-fields))
	 (methods (get symbolic-class-name :symbolic-class-methods)))
    `(progn
       (add-symbolic-object-fields ,s-object ',fields)
    ;; ;; * method 生成
    ;; (loop for (method-name method-lambda-list . method-body) in methods ; TODO: use parse-lambda in alexandria
    ;;    do (add-symbolic-object-method s-object method-name
    ;; 				      `(lambda (,@method-lambda-list) ,@method-body))
    ;; 	 (add-global-function-to-symbolic-object-method method-name)))
       ,s-object)))

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

(apply-symbolic-object-class 'test-obj test-class)

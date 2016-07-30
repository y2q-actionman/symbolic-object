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
(defun add-spec-into-spec-list (new-spec spec-list)
  (loop with ret = nil
     for spec in spec-list
     unless (eq (first spec) (first new-spec))
     do (push spec ret)
     finally
       (push new-spec ret)
       (return ret)))

(defun merge-spec-list (spec-list new-spec-list)
  (loop for s in new-spec-list
     do (setf spec-list (add-spec-into-spec-list s spec-list)))
  spec-list)

(defmacro define-symbolic-object-class (name super-class-names fields methods)
  (let ((field-specs nil)
	(method-specs nil)
	(field-names nil))
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
       ;; spec 回収
       do (push `(,field-name ,reader-name ,writer-name ,@(rest field-def)) field-specs)
       ;; 
       finally (setf field-specs (nreverse field-specs)))
    ;; merge field specs
    (loop with this-class-field-specs = nil
       for class-name in super-class-names
       do (setf this-class-field-specs (merge-spec-list this-class-field-specs
							(get class-name :symbolic-class-field-specs)))
       finally
	 (setf this-class-field-specs
	       (merge-spec-list this-class-field-specs field-specs))
	 (setf (get name :symbolic-class-field-specs)
	       (merge-spec-list this-class-field-specs field-specs))
	 (setf field-names (mapcar #'car this-class-field-specs)))
    ;; generate method-making functions
    `(flet (,@(loop for (method-name method-lambda-list . method-body) in methods
		 as method-maker-name = (gensym (format nil "method-maker-~A" method-name))
		 ;; global defs
		 unless (fboundp method-name)
		 do (add-global-function-to-symbolic-object-method method-name)
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
       ;; merge method specs
       (loop with this-class-method-specs = nil
	  for class-name in ',super-class-names
	  do (setf this-class-method-specs (merge-spec-list this-class-method-specs
							    (get class-name :symbolic-class-method-specs)))
	  finally
	    (setf (get ',name :symbolic-class-method-specs)
		  (merge-spec-list this-class-method-specs (list ,@method-specs)))))))
;; TODO: use parse-lambda in alexandria

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
       ;; initialize
       when field-inits
       do (setf (symbol-value sym) (first field-inits))
       ;; reader/writer
       do (add-symbolic-object-method s-object reader-name
				      (let ((field-symbol sym))
					(lambda () (symbol-value field-symbol))))
	 (add-symbolic-object-method s-object writer-name
				     (let ((field-symbol sym))
					  (lambda (val) (setf (symbol-value field-symbol) val))))
       ;;
       collect sym into field-syms
       finally
	 (setf field-value-symbols field-syms))
    ;; * method 生成
    (loop for (method-name method-maker-func) in method-specs
       do (add-symbolic-object-method s-object method-name
				      (apply method-maker-func field-value-symbols)))
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
(apply-symbolic-object-class 'test-obj-2 'test-class-2)

(call-symbolic-method 'test-obj 'class-method-hello)
(call-symbolic-method 'test-obj 'class-method-describe)

(call-symbolic-method 'test-obj-2 'class-method-hello)
(call-symbolic-method 'test-obj-2 'class-method-describe)
(call-symbolic-method 'test-obj-2 'class-method-all-sum)
(call-symbolic-method 'test-obj-2 'class-method-all-sum 1000)

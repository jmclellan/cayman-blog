---
layout: post
title: Trace - Practical Examples
categories: [lisp, allegro, trace, debugging, examples]
tags: [debuging, allegro]
---
{% highlight lisp linenos %}
(defparameter \*attempted-lookups\* 0 "price lookups that where made")

(defparameter \*sucessful-lookups\* 0 "price look ups that returned a price")

(defparameter \*store-price-table\*
  (list
   (cons 'milk 7)
   (cons 'dozen-eggs 1.50)
   (cons 'toothpaste 10)
   (cons 'assorted-veggies 3))
  "assoc list that will act as our lookup table for prices")

(defclass storage ()
  ((current-storage :accessor current-storage)
   (storage-type :reader storage-type)))

(defclass grocery-cart (storage)
  ((storage-type :initform 'temporary)
   (current-cart-cost :reader cart-cost :writer adjust-cart-cost :initform 0)))

(defun price-lookup (product-symbol)
  "look up a symbol in the store-price-table and return the price"
  (let ((price-cons (assoc product-symbol *store-price-table* :test 'string=)))
    (values (cdr price-cons)
            (when price-cons t))))

(defmethod add-to-cart (product ((cart grocery-cart)))
  (multiple-value-bind (price exists-in-store-p)
      (price-lookup product)
    (push product (current-storage cart))
    (incf (adjust-cart-cost cart) price)))

(defmethod add-to-cart :before (product ((cart grocery-cart)))
  (incf *attempted-lookup*)
  (unless (nth-value 1 (price-lookup product))
    (error 'invalid-product)))

(defmethod add-to-cart :after (product ((cart grocery-cart)))
  (incf \*successful-lookups\*))

(defmethod add-to-cart :around (product (cart grocery-cart))
  (ignore-errors (call-next-method)))

(defun remove-from-cart (product cart-obj)
  (flet ((in-cart-p (product cart-storage)
           (find product cart-storage :test #'string=)))
    (if (in-cart-p product (strage cart-obj 
    (delete product
{% endhighlight %}


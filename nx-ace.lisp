;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-mode ace-mode (editor-mode)
  "Mode for usage with the Ace editor."
  ((style (cl-css:css
           '(("#editor"
              :position "absolute"
              :top "0"
              :right "0"
              :bottom "0"
              :left "0"))))
   (constructor
    (lambda (mode)
      (initialize-display mode)))))

(defmethod initialize-display ((ace ace-mode))
  (let* ((content (markup:markup
                   (:head (:style (style ace)))
                   (:body
                    (:div :id "editor" "Loading...")
                    (:script :src "https://pagecdn.io/lib/ace/1.4.12/ace.min.js"
                             :type "text/javascript"
                             :charset "utf-8" "")
                    (:script
                     (markup:raw (ps:ps (defparameter editor (ps:chain ace (edit "editor")))))))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer ace) insert-content)))

(defmethod set-option ((ace ace-mode) option value)
  (with-current-buffer (buffer ace)
    (pflet ((set-option (option value)
                        (ps:chain editor (set-option (ps:lisp option) (ps:lisp value)))))
      (set-option option value))))

(defmethod set-content ((ace ace-mode) content)
  (with-current-buffer (buffer ace)
    (pflet ((set-content (content)
                         (ps:chain editor session (set-value (ps:lisp content)))))
      (set-content content))))

(defmethod get-content ((ace ace-mode))
  (with-current-buffer (buffer ace)
    (pflet ((get-content ()
                         (ps:chain editor (get-value))))
      (get-content))))

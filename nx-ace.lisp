;;;; nx-ace.lisp

(in-package #:nx-ace)

(in-package :nyxt)

(define-mode ace-mode (nyxt/application-mode:application-mode)
  "Mode for usage with the Ace editor"
  ((file :documentation "The file being edited.")
   (style (cl-css:css
           '(("#editor"
              :position "absolute"
              :top "0"
              :right "0"
              :bottom "0"
              :left "0"))))
   (constructor
    (lambda (mode)
      (if (current-keymaps-hook (buffer mode))
          (hooks:add-hook (current-keymaps-hook (buffer mode))
                          (make-handler-keymaps-buffer #'keep-override-map))
          (make-hook-keymaps-buffer
           :combination #'hooks:combine-composed-hook
           :handlers (list #'keep-override-map)))
      (initialize-display mode)))
   (destructor
    (lambda (mode)
      (hooks:remove-hook (current-keymaps-hook (buffer mode))
                         'keep-override-map)))))

(defun keep-override-map (keymaps buffer)
  (if (nyxt::active-prompt-buffers (current-window))
      (values keymaps buffer)
      (values (list (override-map buffer)) buffer)))

(defmethod initialize-display ((ace ace-mode))
  (let* ((content (markup:markup
                   (:head (:style (style ace)))
                   (:body
                    (:div :id "editor" "Loading...")
                    (:script :src "https://pagecdn.io/lib/ace/1.4.12/ace.min.js"
                             :type "text/javascript"
                             :charset "utf-8" "")
                    (:script
                     (markup:raw "var editor = ace.edit(\"editor\");
                                  editor.setTheme(\"ace/theme/textmate\");
                                  editor.session.setMode(\"ace/mode/lisp\");")))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer ace) insert-content)))

(defmethod set-content ((ace ace-mode) value)
  (pflet ((set-content (value)
                       (ps:chain editor session (set-value (ps:lisp value)))))
    (set-content value)))

(defmethod get-content ((ace ace-mode))
  (pflet ((get-content ()
                       (ps:chain editor (get-value))))
    (get-content)))

(defmethod open-file ((ace ace-mode) file)
  (setf (file ace) file)
  (set-content ace (uiop:read-file-string file)))

(defmethod write-file ((ace ace-mode) &key (if-exists :error))
  (alexandria:write-string-into-file (get-content ace) (file ace)
                                     :if-exists if-exists))

(defun current-ace ()
  (find-submode (current-buffer) 'ace-mode))

(define-command ace ()
  "Show Ace editor."
  (let* ((ace-buffer (make-buffer :title "*Ace*" :modes '(ace-mode base-mode))))
    (set-current-buffer ace-buffer)
    ace-buffer))

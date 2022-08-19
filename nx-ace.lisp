;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nx-ace)

(define-mode ace-mode (nyxt/editor-mode:editor-mode nyxt/passthrough-mode:passthrough-mode)
  "Mode for usage with the Ace editor."
  ((style
    #+nyxt-2
    (cl-css:css
     '(("#editor"
        :position "absolute"
        :top "0"
        :right "0"
        :bottom "0"
        :left "0")))
    #+nyxt-3
    (theme:themed-css (theme *browser*)
      ("#editor"
       :position "absolute"
       :top "0"
       :right "0"
       :bottom "0"
       :left "0")))
   (extensions
    nil
    :type list
    :documentation "A list of links to scripts enhancing Ace, like keybindings or themes.

Example of the value:
'(\"https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/keybinding-emacs.min.js\"
  \"https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/mode-c_cpp.min.js\"
  \"https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/theme-tomorrow_night_eighties.min.js\"))")
   (theme
    nil
    :type string
    :documentation "Name of the theme to enable by default when opening Ace.

Should be of the \"ace/theme/name\" format and should be loaded as part of `extensions', unless built into Ace.")
   (keybindings
    nil
    :type string
    :documentation "Name of the keybinding mode to enable in Ace.

Should be of the \"ace/keyboard/name\" format and should be loaded as part of `extensions', unless built into Ace.")
   (epilogue
    nil
    :type string
    :documentation "JavaScript code to run after setting the file contents.
Put your extension-specific configuration here.")
   #+nyxt-2
   (constructor
    (lambda (mode)
      (initialize-display mode)))))

#+nyxt-2
(defmethod initialize-display ((ace ace-mode))
  (let* ((content (markup:markup
                    (:head (:style (style ace)))
                    (:body
                     (:div :id "editor" "")
                     (:script :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/ace.min.js"
                              :type "text/javascript"
                              :charset "utf-8" "")
                     (:script (markup:raw (ps:ps (defparameter editor (ps:chain ace (edit "editor")))))))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer ace) insert-content)))

#+nyxt-3
(defmethod nyxt/editor-mode::markup ((ace ace-mode))
  (spinneret:with-html-string
    (:head
     (:style (style ace)))
    (:body
     (:script
      :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/ace.min.js"
      :integrity "sha512-U2JKYiHG3ixOjmdycNbi4Xur8q4Nv73CscCGEopBeiVyzDR5ErC6jmHNr0pOB8CUVWb0aQXLgL0wYXhoMU6iqw=="
      :crossorigin "anonymous"
      :type "text/javascript"
      :charset "utf-8"
      "")
     (dolist (ext (extensions ace))
       (:script
        :src (quri:render-uri (quri:uri ext))
        :crossorigin "anonymous"
        :type "text/javascript"
        :charset "utf-8"
        ""))
     (:div :id "editor" "")
     (:script
      (:raw
       (ps:ps
         (defparameter editor (ps:chain ace (edit "editor")))
         (when (ps:lisp (theme ace))
           (ps:chain editor (set-theme (ps:lisp (theme ace)))))
         (ps:chain editor (set-keyboard-handler
                           (ps:@ (require (ps:lisp (keybindings ace))) handler))))))
     (:script
      (:raw (epilogue ace))))))

(defmethod nyxt/editor-mode::set-content ((ace ace-mode) content)
  #+nyxt-3
  (peval :buffer (buffer ace)
    (ps:chain editor session (set-value (ps:lisp content))))
  #+nyxt-2
  (with-current-buffer (buffer ace)
    (pflet ((set-content (content)
                         (ps:chain editor session (set-value (ps:lisp content)))))
      (set-content content))))

(defmethod nyxt/editor-mode::get-content ((ace ace-mode))
  #+nyxt-3
  (peval :buffer (buffer ace) (ps:chain editor (get-value)))
  #+nyxt-2
  (with-current-buffer (buffer ace)
    (pflet ((get-content ()
                         (ps:chain editor (get-value))))
      (get-content))))

(defmethod set-option ((ace ace-mode) option value)
  #+nyxt-3
  (peval :buffer (buffer ace)
    (ps:chain editor (set-option (ps:lisp option) (ps:lisp value))))
  #+nyxt-2
  (with-current-buffer (buffer ace)
    (pflet ((set-option (option value)
                        (ps:chain editor (set-option (ps:lisp option) (ps:lisp value)))))
      (set-option option value))))

(defun options ()
  "Get the list of option names currently present in the editor."
  (alex:hash-table-keys (peval (ps:chain editor (get-options)))))

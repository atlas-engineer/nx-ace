;;;; nx-ace.asd

(asdf:defsystem #:nx-ace
  :description "Binding for the Ace editor within Nyxt"
  :author "Atlas Engineer LLC"
  :license  "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt
               :parenscript)
  :components ((:file "package")
               (:file "nx-ace")))

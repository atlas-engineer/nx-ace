;;;; nx-ace.asd

(asdf:defsystem #:nx-ace
  :description "Binding for the Ace editor within Nyxt"
  :author "Atlas Engineer LLC"
  :license  "BSD 2-clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "nx-ace")))

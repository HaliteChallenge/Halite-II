(cl:in-package #:asdf-user)

(defsystem :mybot
  :description "A simple Halite-II bot."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on (:halite)
  :build-operation :program-op
  :build-pathname "mybot"
  :entry-point "mybot:mybot"

  :serial t
  :components
  ((:file "mybot")))

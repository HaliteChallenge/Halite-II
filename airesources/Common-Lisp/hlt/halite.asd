(cl:in-package #:asdf-user)

(defsystem :halite
  :description "A Halite-II client."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :serial t
  :components
  ((:file "packages")
   (:file "constants")
   (:file "entities")
   (:file "commands")
   (:file "networking")))

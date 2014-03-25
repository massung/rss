(defpackage :rss-asd
  (:use :cl :asdf))

(in-package :rss-asd)

(defsystem :rss
  :name "rss"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "RSS feed parser and aggregator for LispWorks."
  :serial t
  :components ((:file "rss") (:file "aggregator"))
  :depends-on ("http" "xml" "rfc-date"))

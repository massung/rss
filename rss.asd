(defpackage :rss-asd
  (:use :cl :asdf))

(in-package :rss-asd)

(defsystem :rss
  :name "rss"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "RSS feed parser and aggregator for ClozureCL."
  :serial t
  :components ((:file "rss")
               (:file "channel")
               (:file "atom")
               (:file "aggregator"))
  :depends-on ("http" "url" "xml" "rfc-date"))

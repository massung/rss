;;;; RSS for LispWorks
;;;;
;;;; Copyright (c) 2012 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :rss
  (:use :cl :lw :date :xml :http)
  (:export
   #:rss-get
   #:rss-query
   #:rss-title
   #:rss-link
   #:rss-description
   #:rss-image
   #:rss-content
   #:rss-categories
   #:rss-date
   #:rss-guid
   #:rss-ttl
   #:rss-items))

(in-package :rss)

(defclass rss-node ()
  ((node  :initarg :xml-node :reader rss-xml-node))
  (:documentation "Base interface for RSS feeds and items."))

(defclass rss-feed (rss-node)
  ((items :initarg :items :reader rss-items))
  (:documentation "Interface to an RSS channel."))

(defclass rss-item (rss-node)
  ()
  (:documentation "Interface to an RSS channel."))

(defmethod print-object ((node rss-node) s)
  "Output an RSS feed object to a stream."
  (print-unreadable-object (node s :type t) (format s "~s" (rss-title node))))

(defun rss-get (url &key (redirect-limit 3))
  "Fetch a feed from a URL and parse it."
  (let ((resp (http-follow (http-get url) :limit redirect-limit)))
    (when (= (response-code resp) 200)
      (let ((doc (parse-xml (response-body resp) url)))
        (when doc
          (let ((channel (query-xml doc "/rss/channel" :first t)))
            (when channel
              (let ((items (query-xml channel "item")))
                (flet ((make-rss-item (item)
                         (make-instance 'rss-item :xml-node item)))
                  (make-instance 'rss-feed :xml-node channel :items (mapcar #'make-rss-item items)))))))))))

(defmethod rss-query ((node rss-node) element-name &key (if-found #'identity) if-not-found)
  "Lookup the value of an RSS element."
  (let ((element (query-xml (rss-xml-node node) element-name :first t)))
    (if (null element)
        if-not-found
      (funcall if-found (node-value element)))))

(defmethod rss-title ((node rss-node))
  "Return the <title> of an RSS item."
  (rss-query node "title"))

(defmethod rss-link ((node rss-node))
  "Return the <link> of an RSS item."
  (rss-query node "link"))

(defmethod rss-description ((node rss-node))
  "Return the <description> of an RSS item."
  (rss-query node "description"))

(defmethod rss-image ((node rss-node))
  "Return the <image> of an RSS item."
  (rss-query node "image"))

(defmethod rss-categories ((node rss-node))
  "Return the <category> list of an RSS item."
  (mapcar #'node-value (query-xml (rss-xml-node node) "category")))

(defmethod rss-content ((item rss-item))
  "Return the <content:encoded> of an RSS item."
  (rss-query item "encoded"))

(defmethod rss-date ((item rss-item))
  "Return the <pubDate> of an RSS item."
  (rss-query item "pubDate" :if-found  #'encode-universal-rfc822-time :if-not-found (get-universal-time)))

(defmethod rss-guid ((item rss-item))
  "Return the <guid> of an RSS item."
  (rss-query item "guid"))

(defmethod rss-date ((feed rss-feed))
  "Return the <lastBuildDate> of an RSS feed."
  (rss-query feed "lastBuildDate" :if-found  #'encode-universal-rfc822-time :if-not-found (get-universal-time)))

(defmethod rss-ttl ((feed rss-feed))
  "Return the <ttl> of an RSS feed."
  (rss-query feed "ttl" :if-found #'parse-integer))
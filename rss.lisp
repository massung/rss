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
  (:use :cl :lw :rfc-date :xml :http)
  (:export
   #:rss-get
   #:rss-parse
   #:rss-title
   #:rss-subtitle
   #:rss-link
   #:rss-content
   #:rss-author
   #:rss-date
   #:rss-ttl
   #:rss-guid
   #:rss-image
   #:rss-items))

(in-package :rss)

(defclass rss-feed ()
  ((title    :initarg :title    :reader rss-title)
   (subtitle :initarg :subtitle :reader rss-subtitle)
   (link     :initarg :link     :reader rss-link)
   (date     :initarg :date     :reader rss-date)
   (ttl      :initarg :ttl      :reader rss-ttl)
   (image    :initarg :image    :reader rss-image)
   (items    :initarg :items    :reader rss-items))
  (:documentation "RSS atom feed or channel."))

(defclass rss-item ()
  ((title    :initarg :title    :reader rss-title)
   (link     :initarg :link     :reader rss-link)
   (content  :initarg :content  :reader rss-content)
   (date     :initarg :date     :reader rss-date)
   (author   :initarg :author   :reader rss-author)
   (guid     :initarg :guid     :reader rss-guid)
   (image    :initarg :image    :reader rss-image))
  (:documentation "RSS atom entry or channel item."))

(defmethod print-object ((feed rss-feed) s)
  "Output an RSS feed object to a stream."
  (print-unreadable-object (feed s :type t)
    (format s "~s (~a items)" (rss-title feed) (length (rss-items feed)))))

(defmethod print-object ((item rss-item) s)
  "Output an RSS item object to a stream."
  (print-unreadable-object (item s :type t)
    (format s "~s" (rss-title item))))

(defun rss-get (url &key (redirect-limit 3))
  "Fetch a feed from a URL and parse it."
  (with-response (resp (http-follow (http-get url) :limit redirect-limit))
    (when-let (doc (parse-xml (response-body resp) url))
      (when-let (node (query-xml doc "/feed" :first t))
        (return-from rss-get (rss-parse-atom node)))
      (when-let (node (query-xml doc "/rss/channel" :first t))
        (return-from rss-get (rss-parse node))))))

(defun rss-query (node element-name &key (if-found #'node-value))
  "Lookup the an RSS element."
  (let ((element (query-xml node element-name :first t)))
    (when element
      (funcall if-found element))))

(defun rss-query-value (node element-name &key (if-found #'identity))
  "Lookup the inner-text of an RSS element."
  (rss-query node element-name :if-found #'(lambda (n) (funcall if-found (node-value n)))))

(defun rss-query-attribute (node element-name attribute &key (if-found #'identity))
  "Lookup the attribute of an RSS element."
  (flet ((query-attrib (n)
           (when-let (attrib (query-attribute n attribute))
             (funcall if-found (node-value attrib)))))
    (rss-query node element-name :if-found #'query-attrib)))

(defun rss-query-date (node locations encode-function)
  "Lookup a date using possible locations."
  (loop :for location :in locations
        :for date := (rss-query-value node location :if-found encode-function)
        :when date
        :return date
        :finally (return (get-universal-time))))

(defun rss-parse (node)
  "Returns an RSS feed from a channel."
  (make-instance 'rss-feed
                 :title    (rss-query-value node "title")
                 :subtitle (rss-query-value node "description")
                 :link     (rss-query-value node "link")
                 :image    (rss-query-value node "image/url")
                 :ttl      (rss-query-value node "ttl" :if-found #'parse-integer)
                 :date     (rss-query-date node '("lastBuildDate" "pubDate") #'encode-universal-rfc822-time)
                 :items    (mapcar #'rss-parse-item (query-xml node "item"))))

(defun rss-parse-atom (node)
  "Return an RSS feed from an atom feed."
  (make-instance 'rss-feed
                 :title    (rss-query-value node "title")
                 :subtitle (rss-query-value node "subtitle")
                 :link     (rss-query-attribute node "link" "href")
                 :ttl      (rss-query-value node "ttl" :if-found #'parse-integer)
                 :date     (rss-query-date node '("updated") #'encode-universal-rfc3339-time)
                 :image    (or (rss-query-value node "logo")
                               (rss-query-value node "icon"))
                 :items    (mapcar #'rss-parse-atom-entry (query-xml node "entry"))))

(defun rss-parse-item (node)
  "Returns an RSS item from an atom feed."
  (make-instance 'rss-item
                 :title   (rss-query-value node "title")
                 :author  (rss-query-value node "author")
                 :link    (rss-query-value node "link")
                 :guid    (rss-query-value node "guid")
                 :content (rss-query-value node "description")
                 :date    (rss-query-date node '("lastBuildDate" "pubDate") #'encode-universal-rfc822-time)
                 :image   (rss-query-value node "image/url")))

(defun rss-parse-atom-entry (node)
  "Returns an RSS item from an atom feed."
  (make-instance 'rss-item
                 :title   (rss-query-value node "title")
                 :author  (rss-query-value node "author")
                 :link    (rss-query-attribute node "link" "href")
                 :content (rss-query-value node "content")
                 :guid    (rss-query-value node "id")
                 :date    (rss-query-date node '("updated" "published") #'encode-universal-rfc3339-time)
                 :image   (or (rss-query-value node "logo")
                              (rss-query-value node "icon"))))
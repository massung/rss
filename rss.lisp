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
   #:rss-title
   #:rss-link
   #:rss-description
   #:rss-content
   #:rss-date
   #:rss-ttl
   #:rss-guid
   #:rss-image
   #:rss-items))

(in-package :rss)

(defclass rss-feed ()
  ((title :initarg :title       :reader rss-title)
   (link  :initarg :link        :reader rss-link)
   (desc  :initarg :description :reader rss-description)
   (date  :initarg :date        :reader rss-date)
   (ttl   :initarg :ttl         :reader rss-ttl)
   (image :initarg :image       :reader rss-image)
   (items :initarg :items       :reader rss-items))
  (:documentation "RSS atom feed or channel."))

(defclass rss-item ()
  ((title   :initarg :title   :reader rss-title)
   (link    :initarg :link    :reader rss-link)
   (content :initarg :content :reader rss-content)
   (date    :initarg :date    :reader rss-date)
   (author  :initarg :author  :reader rss-author)
   (guid    :initarg :guid    :reader rss-guid)
   (image   :initarg :image   :reader rss-image))
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
  (let ((resp (http-follow (http-get url) :limit redirect-limit)))
    (when (= (response-code resp) 200)
      (let ((doc (parse-xml (response-body resp) url)))
        (when doc
          (parse-rss doc))))))

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
           (let ((attrib (query-attribute n attribute)))
             (when attrib
               (funcall if-found (node-value attrib))))))
    (rss-query node element-name :if-found #'query-attrib)))

(defun parse-rss (doc)
  "Handle the various formats for RSS feeds."
  (when-let (feed (query-xml doc "/feed" :first t))
    (return-from parse-rss (parse-rss-feed feed)))
  (when-let (channel (query-xml doc "/rss/channel" :first t))
    (return-from parse-rss (parse-rss-channel channel))))

(defun parse-rss-feed (feed)
  "Return an RSS feed from an atom feed."
  (make-instance 'rss-feed
                 :title (rss-query-value feed "title" :if-found #'decode-html)
                 :link (rss-query-attribute feed "link" "href")
                 :description (rss-query-value feed "subtitle")
                 :image (or (rss-query-value feed "logo")
                            (rss-query-value feed "icon"))
                 :ttl (rss-query-value feed "ttl" :if-found #'parse-integer)
                 :date (or (rss-query-value feed "updated" :if-found #'encode-universal-rfc3339-time)
                           (get-universal-time))
                 :items (mapcar #'parse-feed-entry (query-xml feed "entry"))))

(defun parse-feed-entry (entry)
  "Returns an RSS item from an atom feed."
  (make-instance 'rss-item
                 :title (rss-query-value entry "title" :if-found #'decode-html)
                 :author (rss-query-value entry "author")
                 :link (rss-query-attribute entry "link" "href")
                 :content (rss-query-value entry "content" :if-found #'decode-html)
                 :guid (rss-query-value entry "id")
                 :image (or (rss-query-value entry "logo")
                            (rss-query-value entry "icon"))
                 :date (or (rss-query-value entry "updated" :if-found #'encode-universal-rfc3339-time)
                           (rss-query-value entry "published" :if-found #'encode-universal-rfc3339-time)
                           (get-universal-time))))

(defun parse-rss-channel (channel)
  "Returns an RSS feed from a channel."
  (make-instance 'rss-feed
                 :title (rss-query-value channel "title" :if-found #'decode-html)
                 :link (rss-query-value channel "link")
                 :description (rss-query-value channel "description")
                 :image (rss-query-value channel "image/url")
                 :ttl (rss-query-value channel "ttl" :if-found #'parse-integer)
                 :date (or (rss-query-value channel "lastBuildDate" :if-found #'encode-universal-rfc822-time)
                           (rss-query-value channel "pubDate" :if-found #'encode-universal-rfc822-time)
                           (get-universal-time))
                 :items (mapcar #'parse-channel-item (query-xml channel "item"))))

(defun parse-channel-item (item)
  "Returns an RSS item from an atom feed."
  (make-instance 'rss-item
                 :title (rss-query-value item "title" :if-found #'decode-html)
                 :author (rss-query-value item "author")
                 :link (rss-query-value item "link")
                 :content (rss-query-value item "description" :if-found #'decode-html)
                 :image (rss-query-value item "image/url")
                 :guid (rss-query-value item "guid")
                 :date (or (rss-query-value item "lastBuildDate" :if-found #'encode-universal-rfc822-time)
                           (rss-query-value item "pubDate" :if-found #'encode-universal-rfc822-time)
                           (get-universal-time))))

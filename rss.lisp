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
   #:rss-aggregator

   ;; rss-feed readers
   #:rss-feed-title
   #:rss-feed-subtitle
   #:rss-feed-link
   #:rss-feed-date
   #:rss-feed-ttl
   #:rss-feed-image
   #:rss-feed-icon
   #:rss-feed-items

   ;; rss-item readers
   #:rss-item-title
   #:rss-item-author
   #:rss-item-link
   #:rss-item-guid
   #:rss-item-summary
   #:rss-item-content
   #:rss-item-categories
   #:rss-item-date

   ;; rss-content readers
   #:rss-content-find
   #:rss-content-type
   #:rss-content-summary
   #:rss-content-link
   
   ;; rss-aggregator methods
   #:rss-aggregator-start
   #:rss-aggregator-stop
   #:rss-aggregator-headlines
   #:rss-aggregator-wait-for-headlines
   #:rss-aggregator-feeds
   #:rss-aggregator-feed-urls

   ;; headline aggregation readers
   #:rss-headline-feed
   #:rss-headline-item))

(in-package :rss)

(defclass rss-feed ()
  ((title      :initarg :title      :reader rss-feed-title)
   (subtitle   :initarg :subtitle   :reader rss-feed-subtitle)
   (link       :initarg :link       :reader rss-feed-link)
   (date       :initarg :date       :reader rss-feed-date)
   (ttl        :initarg :ttl        :reader rss-feed-ttl)
   (image      :initarg :image      :reader rss-feed-image)
   (icon       :initarg :icon       :reader rss-feed-icon)
   (items      :initarg :items      :reader rss-feed-items))
  (:documentation "RSS atom feed or channel."))

(defclass rss-item ()
  ((title      :initarg :title      :reader rss-item-title)
   (link       :initarg :link       :reader rss-item-link)
   (summary    :initarg :summary    :reader rss-item-summary)
   (content    :initarg :content    :reader rss-item-content)
   (categories :initarg :categories :reader rss-item-categories)
   (date       :initarg :date       :reader rss-item-date)
   (author     :initarg :author     :reader rss-item-author)
   (guid       :initarg :guid       :reader rss-item-guid))
  (:documentation "RSS atom entry or channel item."))

(defclass rss-content ()
  ((type       :initarg :type       :reader rss-content-type)
   (summary    :initarg :summary    :reader rss-content-summary)
   (link       :initarg :link       :reader rss-content-link))
  (:documentation "RSS item content data."))

(defmethod print-object ((feed rss-feed) s)
  "Output an RSS feed object to a stream."
  (print-unreadable-object (feed s :type t)
    (format s "~s (~a items)" (rss-feed-title feed) (length (rss-feed-items feed)))))

(defmethod print-object ((item rss-item) s)
  "Output an RSS item object to a stream."
  (print-unreadable-object (item s :type t)
    (format s "~s" (rss-item-title item))))

(defun rss-get (url &key (redirect-limit 3))
  "Fetch a feed from a URL and parse it."
  (with-url (url url)
    (with-response (resp (http-follow (http-get url) :redirect-limit redirect-limit))
      (multiple-value-bind (body format)
          (decode-response-body resp :utf-8)
        (when-let (doc (parse-xml body url format))
          (rss-parse doc (format-url url)))))))

(defun rss-parse (doc &optional source-url)
  "Parse an XML document as an RSS feed."
  (when-let (node (find-xml doc "/feed"))
    (return-from rss-parse (rss-parse-atom node source-url)))
  (when-let (node (find-xml doc "/rss/channel"))
    (return-from rss-parse (rss-parse-channel node source-url))))

(defun rss-content-find (item type)
  "Returns a list of rss-content objects matching a particular type in an rss-item."
  (flet ((match-content-p (content)
           (eql (search type (rss-content-type content) :test #'string=) 0)))
    (remove-if-not #'match-content-p (rss-item-content item))))

(defun rss-query (node &optional (if-found #'node-value))
  "Apply an XML node through a chain of functions until NIL or the final result."
  (and node (funcall if-found node)))

(defun rss-query-value (node &optional (if-found #'identity))
  "Apply a function to a node value if the node exists."
  (and node (funcall if-found (node-value node))))

(defun rss-query-date (node locations encode)
  "Attempts to get the date of an RSS feed/item. If none can be found then use the current time."
  (or (loop :for loc :in locations
            :for date := (rss-query-value (find-xml node loc) encode)
            :when date
            :return date)
      (get-universal-time)))

;;;; RSS Parser and Aggregator for ClozureCL
;;;;
;;;; Copyright (c) Jeffrey Massung
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
  (:use :cl :ccl :rfc-date :xml :url :http)
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
   #:rss-aggregator-wait
   #:rss-aggregator-feeds

   ;; headline aggregation readers
   #:rss-headline-feed
   #:rss-headline-item))

(in-package :rss)

;;; ----------------------------------------------------

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

;;; ----------------------------------------------------

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

;;; ----------------------------------------------------

(defclass rss-content ()
  ((type       :initarg :type       :reader rss-content-type)
   (summary    :initarg :summary    :reader rss-content-summary)
   (link       :initarg :link       :reader rss-content-link))
  (:documentation "RSS item content data."))

;;; ----------------------------------------------------

(defmethod print-object ((feed rss-feed) s)
  "Output an RSS feed object to a stream."
  (print-unreadable-object (feed s :type t)
    (with-slots (title items)
        feed
      (format s "~s (~a items)" title (length items)))))

;;; ----------------------------------------------------

(defmethod print-object ((item rss-item) s)
  "Output an RSS item object to a stream."
  (print-unreadable-object (item s :type t)
    (prin1 (rss-item-title item) s)))

;;; ----------------------------------------------------

(defun rss-get (url &key (redirect-limit 3))
  "Fetch a feed from a URL and parse it."
  (with-response (resp (http-get url :redirect-limit redirect-limit))
    (let ((doc (xml-parse (resp-body resp) url)))
      (rss-parse doc url))))

;;; ----------------------------------------------------

(defun rss-parse (doc &optional url)
  "Parse an XML document as an RSS feed."
  (let ((node (first (xml-query doc "/feed"))))
    (when node (return-from rss-parse (rss-parse-atom node url))))
  (let ((node (first (xml-query doc "/rss/channel"))))
    (when node (return-from rss-parse (rss-parse-channel node url)))))

;;; ----------------------------------------------------

(defun rss-content-find (item type &optional subtype)
  "Returns a list of rss-content objects matching a particular type."
  (flet ((match-content-p (c)
           (destructuring-bind (fst &optional snd)
               (content-mime-type c)
             (and (string-equal fst type)
                  (or (null subtype)
                      (string-equal snd subtype))))))
    (remove-if-not #'match-content-p (rss-item-content item))))

;;; ----------------------------------------------------

(defun rss-query (node path &optional (if-found #'xml-node-value))
  "Search for a child tag of an node."
  (let ((q (xml-query node path)))
    (when q
      (funcall if-found (first q)))))

;;; ----------------------------------------------------

(defun rss-query-value (node path &optional (if-found #'identity))
  "Apply a function to a node value if the node exists."
  (let ((value (rss-query node path)))
    (when value
      (funcall if-found value))))

;;; ----------------------------------------------------

(defun rss-query-date (node locations date-format)
  "Attempts to get the date of an RSS feed/item."
  (do ((loc (pop locations)
            (pop locations)))
      ((null loc)
       (get-universal-time))
    (let ((date (rss-query node loc)))
      (when date
        (return (encode-universal-rfc-time date date-format))))))

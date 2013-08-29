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
  (:use :cl :lw :re :xml :http)
  (:export
   #:read-rss

   ;; feed accessors
   #:feed-title
   #:feed-link
   #:feed-description
   #:feed-categories
   #:feed-image
   #:feed-ttl
   #:feed-items

   ;;item accessors
   #:item-title
   #:item-link
   #:item-description
   #:item-categories
   #:item-pub-date
   #:item-guid))

(in-package :rss)

(defclass rss-feed ()
  ((title       :initarg :title       :accessor feed-title)
   (link        :initarg :link        :accessor feed-link)
   (description :initarg :description :accessor feed-description)
   (categories  :initarg :categories  :accessor feed-categories)
   (image       :initarg :image       :accessor feed-image)
   (ttl         :initarg :ttl         :accessor feed-ttl)
   (items       :initarg :items       :accessor feed-items))
  (:documentation ""))

(defclass rss-item ()
  ((title       :initarg :title       :accessor item-title)
   (link        :initarg :link        :accessor item-link)
   (description :initarg :description :accessor item-description)
   (categories  :initarg :categories  :accessor item-categories)
   (pub-date    :initarg :pub-date    :accessor item-pub-date)
   (guid        :initarg :guid        :accessor item-guid))
  (:documentation ""))

(defmethod print-object ((feed rss-feed) s)
  "Output an RSS feed object to a stream."
  (print-unreadable-object (feed s :type t)
    (format s "~s" (feed-title feed))))

(defmethod print-object ((item rss-item) s)
  "Output an RSS item object to a stream."
  (print-unreadable-object (item s :type t)
    (format s "~s" (item-title item))))

(defconstant +date-re+ (compile-re "(%a+),%s*(%d+)%s+(%a+)%s+(%d+)%s+(%d+):(%d+):(%d+)%s+(.*)")
  "An RSS date/time format (RFC 822).")
(defconstant +months+ '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "All months abbreviated for RFC 822 date format.")
(defconstant +days+ '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "Days abbreviated for RFC 822 date format.")

(defun month-of-name (month)
  "Return the integer month for a given named month."
  (1+ (position month +months+ :test #'string=)))

(defun day-of-name (day)
  "Return the integer day of the week for a given named day."
  (position day +days+ :test #'string=))

(defun parse-date (date-string)
  "Encode a string representing a date into a universal timestamp."
  (with-re-match (match (match-re +date-re+ date-string) :no-match (get-universal-time))
    (let ((day (parse-integer $2))
          (month (month-of-name $3))
          (year (parse-integer $4))
          (hour (parse-integer $5))
          (min (parse-integer $6))
          (sec (parse-integer $7)))
    (encode-universal-time sec min hour day month year))))

(defun read-rss (url)
  "Fetch a feed from a URL and parse it."
  (let ((resp (http-follow (http-get url))))
    (when (= (response-code resp) 200)
      (with-slots (request)
          resp
        (let ((doc (parse-xml (response-body resp) (request-url request))))
          (when doc
            (let ((channel (query-xml doc "/rss/channel" :first t)))
              (when channel
                (let ((title (query-xml channel "title" :first t))
                      (link (query-xml channel "link" :first t))
                      (description (query-xml channel "description" :first t))
                      (image (query-xml channel "image/url" :first t))
                      (ttl (query-xml channel "ttl" :first t)))
                  (make-instance 'rss-feed
                                 :title (when title (node-value title))
                                 :link (when link (parse-url (node-value link)))
                                 :description (when description (node-value description))
                                 :image (when image (node-value image))
                                 :ttl (when ttl (parse-integer (node-value ttl)))
                                 :categories (parse-categories channel)
                                 :items (parse-items doc)))))))))))

(defun parse-items (doc)
  "Read all the RSS items from an XML document."
  (flet ((parse-item (item)
           (let ((title (query-xml item "title" :first t))
                 (link (query-xml item "link" :first t))
                 (description (query-xml item "description" :first t))
                 (pub-date (query-xml item "pubDate" :first t))
                 (guid (query-xml item "guid" :first t)))
             (make-instance 'rss-item
                            :title (when title (node-value title))
                            :link (when link (parse-url (node-value link)))
                            :description (when description (node-value description))
                            :pub-date (when pub-date (parse-date (node-value pub-date)))
                            :guid (when guid (node-value guid))
                            :categories (parse-categories item)))))
    (mapcar #'parse-item (query-xml doc "/rss/channel/item"))))

(defun parse-categories (node)
  "Find all the categories for a feed or item."
  (mapcar #'node-value (query-xml node "category")))
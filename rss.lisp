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
   #:rss-summary
   #:rss-link
   #:rss-content
   #:rss-content-type
   #:rss-content-link
   #:rss-content-find
   #:rss-author
   #:rss-date
   #:rss-ttl
   #:rss-guid
   #:rss-image
   #:rss-items))

(in-package :rss)

(defclass rss-feed ()
  ((title     :initarg :title     :reader rss-title)
   (subtitle  :initarg :subtitle  :reader rss-subtitle)
   (link      :initarg :link      :reader rss-link)
   (date      :initarg :date      :reader rss-date)
   (ttl       :initarg :ttl       :reader rss-ttl)
   (image     :initarg :image     :reader rss-image)
   (items     :initarg :items     :reader rss-items))
  (:documentation "RSS atom feed or channel."))

(defclass rss-item ()
  ((title     :initarg :title     :reader rss-title)
   (link      :initarg :link      :reader rss-link)
   (summary   :initarg :summary   :reader rss-summary)
   (content   :initarg :content   :reader rss-content)
   (date      :initarg :date      :reader rss-date)
   (author    :initarg :author    :reader rss-author)
   (guid      :initarg :guid      :reader rss-guid))
  (:documentation "RSS atom entry or channel item."))

(defclass rss-content ()
  ((type      :initarg :type      :reader rss-content-type)
   (link      :initarg :link      :reader rss-content-link))
  (:documentation "RSS item content data."))

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

      ;; check for an ATOM feed
      (when-let (atom (find-xml doc "/feed"))
        (return-from rss-get (rss-parse-atom atom)))

      ;; check for an RSS 2.0 channel
      (when-let (channel (find-xml doc "/rss/channel"))
        (return-from rss-get (rss-parse channel))))))

(defun rss-content-find (item type)
  "Returns a list of rss-content objects matching a particular type in an rss-item."
  (flet ((match-content-p (content)
           (eql (search type (rss-content-type content) :test #'string=) 0)))
    (remove-if-not #'match-content-p (rss-content item))))

(defun rss-query (node &optional if-found)
  "Apply an XML node through a chain of functions until NIL or the final result."
  (handler-case
      (when-let (value (and node (node-value node)))
        (if (null if-found)
            value
          (funcall if-found value)))
    (error (c) nil)))

(defun rss-query-date (node locations encode)
  "Attempts to get the date of an RSS feed/item. If none can be found then use the current time."
  (loop :for loc :in locations
        :for date := (rss-query (find-xml node loc) encode)
        :when date
        :return date
        :finally (return (get-universal-time))))

(defun rss-parse (channel)
  "Parse the items of an RSS 2.0 channel."
  (make-instance 'rss-feed
                 :title      (rss-query (find-xml channel "title"))
                 :subtitle   (rss-query (find-xml channel "description"))
                 :link       (rss-query (find-xml channel "link"))
                 :ttl        (rss-query (find-xml channel "ttl") #'parse-integer)
                 :image      (rss-query (find-xml channel "image/url"))

                 ;; use the build date or publish date
                 :date       (rss-query-date channel '("lastBuildDate" "pubDate") #'encode-universal-rfc822-time)

                 ;; parse all the items in the channel
                 :items      (mapcar #'rss-parse-item (query-xml channel "item"))))

(defun rss-parse-item (item)
  "Parse an individual items in an RSS 2.0 channel."
  (make-instance 'rss-item
                 :title      (rss-query (find-xml item "title"))
                 :author     (rss-query (find-xml item "author"))
                 :link       (rss-query (find-xml item "link"))
                 :summary    (rss-query (find-xml item "description"))
                 
                 ;; find multimedia content
                 :content    (mapcar #'rss-parse-enclosure (query-xml item "enclosure"))

                 ;; RSS 2.0 items only have a publish date
                 :date       (rss-query-date item '("pubDate") #'encode-universal-rfc822-time)

                 ;; for the unique identifier, use the link if no guid is present
                 :guid       (rss-query (or (find-xml item "guid")
                                            (find-xml item "link")))))

(defun rss-parse-enclosure (node)
  "Parse the attributes of an RSS item enclosure."
  (make-instance 'rss-content
                 :link (rss-query (find-attribute node "url"))
                 :type (rss-query (find-attribute node "type"))))

(defun rss-parse-atom (atom)
  "Parse the items of an RSS 2.0 channel."
  (make-instance 'rss-feed
                 :title      (rss-query (find-xml atom "title"))
                 :subtitle   (rss-query (find-xml atom "subtitle"))
                 :link       (rss-query (find-xml atom "link") #'rss-parse-atom-link)
                 :ttl        (rss-query (find-xml atom "ttl") #'parse-integer)

                 ;; can have a logo or an icon
                 :image      (or (rss-query (find-xml atom "logo"))
                                 (rss-query (find-xml atom "icon")))

                 ;; use the build date or publish date
                 :date       (rss-query-date atom '("updated" "published") #'encode-universal-rfc3339-time)

                 ;; parse all the entries in the feed
                 :items      (mapcar #'rss-parse-atom-entry (query-xml atom "entry"))))

(defun rss-parse-atom-entry (entry)
  "Returns an RSS item from an ATOM feed."
  (make-instance 'rss-item
                 :title      (rss-query (find-xml entry "title"))
                 :author     (rss-query (find-xml entry "author"))
                 :link       (rss-query (find-xml entry "link") #'rss-parse-atom-link)
                 :summary    (rss-query (find-xml entry "summary"))

                 ;; look for multimedia content
                 :content    (mapcan #'rss-parse-atom-content (rss-query (query-xml entry "content")))

                 ;; use the unique id if present, otherwise the entry link
                 :guid       (or (rss-query (find-xml entry "id"))
                                 (rss-query (find-xml entry "link") #'rss-parse-atom-link))

                 ;; use the last update or the original publish date of the entry
                 :date       (rss-query-date entry '("updated" "published") #'encode-universal-rfc3339-time)))

(defun rss-parse-atom-link (node)
  "Links in ATOM are in an HREF attribute."
  (when-let (href (find-attribute node "href"))
    (node-value href)))

(defun rss-parse-atom-content (node)
  "Parse the attributes of an ATOM content tag."
  (make-instance 'rss-content
                 :link (rss-query (find-attribute node "src"))
                 :type (rss-query (find-attribute node "type"))))

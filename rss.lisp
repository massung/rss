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
   #:rss-aggregator-clear
   #:rss-aggregator-reset
   #:rss-aggregator-headlines
   #:rss-aggregator-wait-for-headlines
   #:rss-aggregator-feeds

   ;; headline aggregation readers
   #:rss-headline-link
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
  (with-response (resp (http-follow (http-get url) :redirect-limit redirect-limit))
    (when-let (doc (parse-xml (response-body resp) url))
      (rss-parse doc))))

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

(defun rss-parse (doc)
  "Parse an XML document as an RSS feed or ATOM."
  (when-let (atom (find-xml doc "/feed"))
    (return-from rss-parse (rss-parse-atom atom)))

  ;; check for an RSS 2.0 channel
  (when-let (channel (find-xml doc "/rss/channel"))
    (return-from rss-parse (rss-parse-channel channel))))

(defun rss-parse-channel (channel)
  "Parse the items of an RSS 2.0 channel."
  (make-instance 'rss-feed
                 :title      (rss-query (find-xml channel "title"))
                 :subtitle   (rss-query (find-xml channel "description"))
                 :link       (rss-query (find-xml channel "link"))
                 :image      (rss-query (find-xml channel "image/url"))

                 ;; time to live (number of minutes between updates)
                 :ttl        (rss-query-value (find-xml channel "ttl") #'parse-integer)

                 ;; rss channels don't support icons, so try a favicon
                 :icon       (when-let (link (rss-query (find-xml channel "link")))
                               (with-url (url link :path "/favicon.ico")
                                 (format-url url)))

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

                 ;; category tags
                 :categories (mapcar #'node-value (query-xml item "category"))

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

                 ;; time to live (minutes between updates)
                 :ttl        (rss-query (find-xml atom "ttl") #'parse-integer)

                 ;; can have a logo and icon
                 :image      (rss-query (find-xml atom "logo"))
                 :icon       (rss-query (find-xml atom "icon"))

                 ;; use the build date or publish date
                 :date       (rss-query-date atom '("updated" "published") #'encode-universal-rfc3339-time)

                 ;; parse all the entries in the feed
                 :items      (mapcar #'rss-parse-atom-entry (query-xml atom "entry"))))

(defun rss-parse-atom-entry (entry)
  "Returns an RSS item from an ATOM feed."
  (make-instance 'rss-item
                 :title      (rss-query (find-xml entry "title"))
                 :author     (rss-query (find-xml entry "author/name"))
                 :summary    (rss-query (find-xml entry "summary"))
                 :link       (rss-query (find-xml entry "link") #'rss-parse-atom-link)

                 ;; look for multimedia content
                 :content    (mapcar #'rss-parse-atom-content (query-xml entry "content"))

                 ;; category tags
                 :categories (mapcar #'node-value (query-xml entry "category"))

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
                 :summary (node-value node)
                 :link (rss-query (find-attribute node "src"))
                 :type (rss-query (find-attribute node "type"))))

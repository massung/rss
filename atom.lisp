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

(in-package :rss)

;;; ----------------------------------------------------

(defun rss-parse-atom-link (node)
  "Parse the URL for a link."
  (first (xml-query node "link/@href/%text/'url:url-parse")))

;;; ----------------------------------------------------

(defun rss-parse-atom-date (node)
  "Parse a date/time in ATOM format."
  (rss-query-date node '("updated" "published") :atom))

;;; ----------------------------------------------------

(defun rss-parse-atom-items (node)
  "Parse all the items in an atom feed."
  (xml-query node "entry/'rss::rss-parse-atom-entry"))

;;; ----------------------------------------------------

(defun rss-parse-atom-content (node)
  "Parse the attributes of a content tag."
  (make-instance
   'rss-content
   :summary (xml-node-value node)

   ;; lookup a link to the content
   :link (rss-query node "@src/%text/'url:url-parse")

   ;; parse the mime type
   :type (rss-query node "type/%text/'http:content-type-parse")))

;;; ----------------------------------------------------

(defun rss-parse-atom-entry (node)
  "Parse a single atom item."
  (let ((link (rss-parse-atom-link node)))
    (make-instance
     'rss-item
     :title      (rss-query node "title/%text")
     :author     (rss-query node "author/name/%text")
     :summary    (rss-query node "summary/%text")

     ;; get the link to this item
     :link       link

     ;; look for multimedia content
     :content    (xml-query node "content/'rss::rss-parse-atom-content")

     ;; category tags
     :categories (xml-query node "category/%text")

     ;; use the unique id if present, otherwise the link
     :guid       (or (rss-query node "id/%text") link)

     ;; get the date the item was last updated
     :date       (rss-parse-atom-date node))))


;;; ----------------------------------------------------

(defun rss-parse-atom (node url)
  "Parse the items of an ATOM feed."
  (make-instance
   'rss-feed
   :title      (rss-query node "title/%text")
   :subtitle   (rss-query node "subtitle/%text")

   ;; set the link back to the site
   :link       (let ((link (rss-parse-atom-link node)))
                 (or link url))

   ;; time to live (minutes between updates)
   :ttl        (rss-query node "ttl/%text/'parse-integer")

   ;; can have a logo and icon
   :image      (rss-query node "logo/%text")
   :icon       (rss-query node "icon/%text")

   ;; use the build date or publish date
   :date       (rss-parse-atom-date node)

   ;; parse all the entries in the feed
   :items      (rss-parse-atom-items node)))

;;;; RSS Parser and Aggregator for SBCL
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

(defun rss-parse-channel-link (node)
  "Parse the URL for a link."
  (rss-query node "link/%text/'url:url-parse"))

;;; ----------------------------------------------------

(defun rss-parse-channel-date (node)
  "Parse a date/time in RSS format."
  (rss-query-date node '("lastBuildDate" "pubDate") :rss))

;;; ----------------------------------------------------

(defun rss-parse-enclosure (node)
  "Parse the attributes of an RSS item enclosure."
  (make-instance
   'rss-content

   ;; get the link to the content
   :link (first (xml-query node "@url/%text/'url:url-parse"))

   ;; parse the mime type of the content
   :type (let ((type (xml-tag-get node "type")))
           (when type
             (content-type-parse type)))))

;;; ----------------------------------------------------

(defun rss-parse-channel-item (node)
  "Parse an individual items in an RSS 2.0 channel."
  (let ((link (rss-parse-channel-link node)))
    (make-instance
     'rss-item

     ;; title, author, and description
     :title      (rss-query node "title/%text")
     :author     (rss-query node "author/%text")
     :summary    (rss-query node "description/%text")

     ;; set the link to the external site
     :link       link

     ;; find multimedia content
     :content    (let ((content (xml-query node "enclosure")))
                   (mapcar #'rss-parse-enclosure content))

     ;; category tags
     :categories (let ((cats (xml-query node "category")))
                   (mapcar #'xml-node-value cats))

     ;; RSS 2.0 items only have a publish date
     :date       (rss-parse-channel-date node)

     ;; use the link if no guid is present
     :guid       (or (rss-query node "guid/%text") link))))

;;; ----------------------------------------------------

(defun rss-parse-channel (node url)
  "Parse the items of an RSS 2.0 channel."
  (let ((link (or (rss-parse-channel-link node) url)))
    (make-instance
     'rss-feed

     ;; title and description
     :title      (rss-query node "title/%text")
     :subtitle   (rss-query node "description/%text")

     ;; get the url to the channel's image
     :image      (rss-query node "image/url/%text/'url:url-parse")

     ;; set the link to the homepage
     :link       link

     ;; time to live (number of minutes between updates)
     :ttl        (rss-query node "ttl/%text" #'parse-integer)

     ;; rss channels don't support icons, so try a favicon
     :icon       (url-parse link :path "/favicon.ico")

     ;; use the build date or publish date
     :date       (rss-parse-channel-date node)

     ;; parse all the items in the channel
     :items      (let ((items (xml-query node "item")))
                   (mapcar #'rss-parse-channel-item items)))))

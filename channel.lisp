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

(in-package :rss)

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

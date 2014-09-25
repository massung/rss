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
  (let ((link (rss-find-link channel)))
    (make-instance 'rss-feed
                   :title      (rss-query (find-xml channel "title"))
                   :subtitle   (rss-query (find-xml channel "description"))
                   :image      (rss-query (find-xml channel "image/url"))
                   
                   ;; set the link to the homepage
                   :link       link
                   
                   ;; time to live (number of minutes between updates)
                   :ttl        (rss-query-value (find-xml channel "ttl") #'parse-integer)
                   
                   ;; rss channels don't support icons, so try a favicon
                   :icon       (when link
                                 (format-url (copy-url link :path "/favicon.ico")))
                   
                   ;; use the build date or publish date
                   :date       (rss-query-date channel '("lastBuildDate" "pubDate") #'encode-universal-rfc822-time)
                   
                   ;; parse all the items in the channel
                   :items      (mapcar #'rss-parse-item (query-xml channel "item")))))

(defun rss-parse-item (item)
  "Parse an individual items in an RSS 2.0 channel."
  (let ((link (rss-find-link item)))
    (make-instance 'rss-item
                   :title      (rss-query (find-xml item "title"))
                   :author     (rss-query (find-xml item "author"))
                   :summary    (rss-query (find-xml item "description"))
                   
                   ;; set the link to the external site
                   :link       link
                   
                   ;; find multimedia content
                   :content    (mapcar #'rss-parse-enclosure (query-xml item "enclosure"))
                   
                   ;; category tags
                   :categories (mapcar #'node-value (query-xml item "category"))
                   
                   ;; RSS 2.0 items only have a publish date
                   :date       (rss-query-date item '("pubDate") #'encode-universal-rfc822-time)
                   
                   ;; for the unique identifier, use the link if no guid is present
                   :guid       (rss-query (or (find-xml item "guid") link)))))

(defun rss-find-link (node)
  "Scan all the link tags until it finds a valid URL."
  (loop :for tag :in (query-xml node "link")

        ;; attempt to parse the link found
        :do (when (handler-case
                      (parse-url (node-value tag))
                    (condition (c) nil))
              (return-from rss-find-link (node-value tag)))))

(defun rss-parse-enclosure (node)
  "Parse the attributes of an RSS item enclosure."
  (make-instance 'rss-content
                 :link (rss-query (find-attribute node "url"))
                 :type (rss-query (find-attribute node "type"))))

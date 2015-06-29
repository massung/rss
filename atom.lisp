;;;; RSS for LispWorks
;;;;
;;;; Copyright (c) 2015 by Jeffrey Massung
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

(defun rss-parse-atom (atom url)
  "Parse the items of an ATOM feed."
  (let ((link (or (rss-query (xml-query atom "link") #'rss-parse-atom-link) url)))
    (make-instance 'rss-feed
                   :title      (rss-query (xml-query atom "title"))
                   :subtitle   (rss-query (xml-query atom "subtitle"))

                   ;; set the link back to the site
                   :link       link

                   ;; time to live (minutes between updates)
                   :ttl        (rss-query (xml-query atom "ttl") #'parse-integer)
                   
                   ;; can have a logo and icon
                   :image      (rss-query (xml-query atom "logo"))
                   :icon       (rss-query (xml-query atom "icon"))
                   
                   ;; use the build date or publish date
                   :date       (rss-query-date atom '("updated" "published") #'encode-universal-rfc3339-time)
                   
                   ;; parse all the entries in the feed
                   :items      (mapcar #'rss-parse-atom-entry (xml-query atom "entry" :all t)))))

(defun rss-parse-atom-entry (entry)
  "Returns an RSS item from an ATOM feed."
  (make-instance 'rss-item
                 :title      (rss-query (xml-query entry "title"))
                 :author     (rss-query (xml-query entry "author/name"))
                 :summary    (rss-query (xml-query entry "summary"))
                 :link       (rss-query (xml-query entry "link") #'rss-parse-atom-link)

                 ;; look for multimedia content
                 :content    (mapcar #'rss-parse-atom-content (xml-query entry "content" :all t))

                 ;; category tags
                 :categories (mapcar #'node-value (xml-query entry "category" :all t))

                 ;; use the unique id if present, otherwise the entry link
                 :guid       (or (rss-query (xml-query entry "id"))
                                 (rss-query (xml-query entry "link") #'rss-parse-atom-link))

                 ;; use the last update or the original publish date of the entry
                 :date       (rss-query-date entry '("updated" "published") #'encode-universal-rfc3339-time)))


(defun rss-parse-atom-link (node)
  "Parse the URL for a link."
  (when-let (href (xml-query-attribute node "href"))
    (xml-node-value href)))

(defun rss-parse-atom-content (node)
  "Parse the attributes of an ATOM content tag."
  (make-instance 'rss-content
                 :summary (xml-node-value node)
                 :link (rss-query (xml-query-attribute node "src"))
                 :type (rss-query (xml-query-attribute node "type"))))
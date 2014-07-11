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

(defpackage :rss-utils
  (:use :cl :lw :sys :rss)
  (:export
   #:rss-open-link
   #:rss-parse-summary
   #:rss-image-url
   #:rss-age-string
   #:rss-source-info
   #:rss-favicon-url))

(in-package :rss-utils)

(defconstant +img-re+ (re:compile-re "<img[^>]+>")
  "Pattern for finding an img tag in a description string.")
(defconstant +src-re+ (re:compile-re "src%s*=%s*['\"]?([^'\" ]+)")
  "Pattern for finding the src attribute in an img tag.")
(defconstant +month-name+ #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The shorthand names for each month.")

(defun rss-open-link (item)
  "Open the RSS link in a browser."
  (open-url (rss-item-link item)))

(defun rss-parse-summary (item)
  "Strip HTML tags and decode the content."
  (flet ((strip-tags (html)
           (re:replace-re #/<[^>]+>?/ "" (html:decode-html html) :all t)))
    (strip-tags (or (when-let (html-content (rss-content-find item "html"))
                      (rss-content-summary (first html-content)))
                    (rss-item-summary item)))))

(defun rss-image-url (item)
  "Find an <img> tag within the content of an rss-item."
  (flet ((find-image (place)
           (when place
             (re:with-re-match (img (re:find-re +img-re+ place))
               (re:with-re-match (src (re:find-re +src-re+ $$)) $1)))))

    ;; check for an actual image content, or scan the summary or html content 
    (or (when-let (image (first (rss-content-find item "image")))
          (rss-content-link image))

        ;; look for actual HTML content
        (find-image (first (mapcar #'rss-content-summary (rss-content-find item "html"))))

        ;; parse the summary
        (find-image (rss-item-summary item)))))

(defun rss-age-string (item)
  "Returns the age of an item in a string format."
  (let ((age (- (get-universal-time) (rss-item-date item))))
    (cond ((< age 60)           (format nil "Just now"))
          ((< age (* 60 60))    (format nil "~d minute~:p ago" (truncate age 60)))
          ((< age (* 60 60 24)) (format nil "~d hour~:p ago" (truncate age (* 60 60))))
          (t                    (format nil "~d day~:p ago" (truncate age (* 60 60 24)))))))

(defun rss-date-string (item)
  "Returns the time (if today) or date when the item was published."
  (multiple-value-bind (sec min hour date month)
      (decode-universal-time (rss-item-date item))
    (declare (ignore sec))
    (multiple-value-bind (s m h d mm)
        (decode-universal-time (get-universal-time))
      (declare (ignore s m h))
      (if (and (= date d)
               (= month mm))
          (format nil "at ~d:~2,'0d~:[am~;pm~]" (if (or (= hour 0) (= hour 12)) 12 (mod hour 12)) min (>= hour 12))
        (format nil "on ~a ~2,'0d" (aref +month-name+ (1- month)) date)))))

(defun rss-source-info (item &optional feed)
  "Age, author, and site of an item."
  (let ((author (rss-item-author item))
        (source (and feed (rss-feed-title feed))))
    (format nil "~:[Posted~;~:*~a~] ~a" (or author source) (rss-date-string item))))
          
(defun rss-favicon-url (feed)
  "Get the favicon URL for a given feed."
  (http:parse-url (rss-feed-link feed) :path "/favicon.ico"))
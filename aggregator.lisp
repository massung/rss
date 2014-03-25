;;;; RSS Aggregator for LispWorks
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

(defpackage :rss-aggregator
  (:use :cl :lw :mp :http :rss)
  (:export
   #:rss-aggregator
   #:rss-aggregate
   
   ;; aggregation functions
   #:rss-aggregator-start
   #:rss-aggregator-stop
   #:rss-aggregator-reset
   #:rss-aggregator-headlines
   #:rss-aggregator-feeds

   ;; feed reader functions
   #:rss-feed-reader-title
   #:rss-feed-reader-url
   #:rss-feed-reader-process

   ;; headline reader functions
   #:rss-headline-source
   #:rss-headline-item))

(in-package :rss-aggregator)

(defconstant +img-re+ (re:compile-re "<img[^>]+>")
  "Pattern for finding an img tag in a description string.")
(defconstant +src-re+ (re:compile-re "src%s*=%s*['\"]([^'\"]+)")
  "Pattern for finding the src attribute in an img tag.")

(defclass rss-aggregator ()
  ((feeds     :initarg :feed-urls       :reader   rss-aggregator-feeds    :initform nil)
   (callback  :initarg :update-callback :accessor rss-aggregator-callback :initform nil)

   ;; internal members
   (process   :initform nil)
   (mailbox   :initform nil)
   (headlines :initform nil)
   (readers   :initform nil))
  (:documentation "A collector of news headlines from feed processes."))

(defclass rss-headline ()
  ((source :initarg :source :reader rss-headline-source)
   (item   :initarg :item   :reader rss-headline-item))
  (:documentation "An rss-item with source."))

(defmethod print-object ((headline rss-headline) stream)
  "Output a headline to a stream."
  (print-unreadable-object (headline stream :type t)
    (prin1 (rss-title (rss-headline-item headline)) stream)))

(defmethod initialize-instance :after ((aggregator rss-aggregator) &key)
  "Start the aggregator immediately."
  (rss-aggregator-start aggregator))

(defmethod rss-aggregator-start ((aggregator rss-aggregator))
  "Spin up the reader processes."
  (with-slots (mailbox process headlines readers callback feeds)
      aggregator
    (labels ((headline-guid (h)
               (let ((item (rss-headline-item h)))
                 (or (rss-guid item)
                     (rss-link item))))

             ;; the aggregation process
             (aggregate ()
               (loop (let ((headline (mailbox-read mailbox)))
                       (unless (find (headline-guid headline) headlines :key #'headline-guid :test #'string=)
                         (sys:atomic-push headline headlines)

                         ;; notify another process
                         (when callback
                           (funcall callback aggregator)))))))
    
      ;; create the headline mailbox
      (setf mailbox (make-mailbox))

      ;; start the aggregation process
      (setf process (process-run-function "RSS Aggregator" () #'aggregate))

      ;; start all the reader processes
      (setf readers (loop :for feed :in feeds :collect (rss-aggregate aggregator feed))))))

(defmethod rss-aggregator-stop ((aggregator rss-aggregator))
  "Stop all feed reader processes and the headline aggregator process."
  (with-slots (mailbox readers process)
      aggregator

    ;; stop processing all the feeds and the aggregator process
    (mapc #'process-kill (cons process readers))
    
    ;; clear the process data and mailbox
    (setf readers nil
          process nil
          mailbox nil)))

(defmethod rss-aggregator-reset ((aggregator rss-aggregator))
  "Stop aggregating, clear headlines, and start again."
  (with-slots (headlines)
      aggregator

    ;; clear the headlines
    (setf headlines nil)

    ;; stop aggregating and start again
    (rss-aggregator-stop aggregator)
    (rss-aggregator-start aggregator)))

(defmethod rss-aggregate ((aggregator rss-aggregator) url)
  "Create a new feed process that will continuously push headlines to the aggregator."
  (with-slots (mailbox)
      aggregator
    (with-url (url url)
      (flet ((reader-process ()
               (loop (handler-case
                         (when-let (feed (rss-get url))

                           ;; rename the process to that of the feed
                           (when (rss-title feed)
                             (setf (process-name *current-process*) (rss-title feed)))

                           ;; send all the headlines to the aggregator
                           (loop :with source := (or (rss-title feed) (format-url url))
                                 :for item :in (rss-items feed)
                                 :do (mailbox-send mailbox (make-instance 'rss-headline :source source :item item))
                                 :finally (current-process-pause (* (or (rss-ttl feed) 5) 60))))
                       (error (c)
                         (current-process-pause 300))))))

        ;; start the feed process
        (process-run-function (format-url url) '() #'reader-process)))))

(defmethod rss-aggregator-headlines ((aggregator rss-aggregator))
  "Get a sorted list of headlines from the aggregator."
  (with-slots (headlines)
      aggregator
    (sort headlines #'> :key #'(lambda (h) (rss-date (rss-headline-item h))))))

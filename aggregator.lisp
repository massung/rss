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

   ;; headline reader functions
   #:rss-headline-feed
   #:rss-headline-item

   ;; aggregated feed reader functions
   #:rss-feed-reader-title
   #:rss-feed-reader-url
   #:rss-feed-reader-feed))

(in-package :rss-aggregator)

(defclass rss-aggregator ()
  ((name      :initarg :name              :reader rss-aggregator-name     :initform nil)
   (callback  :initarg :headline-callback :reader rss-aggregator-callback :initform nil)

   ;; internal members
   (feeds     :initform nil               :reader rss-aggregator-feeds)
   (process   :initform nil)
   (mailbox   :initform nil)
   (headlines :initform nil))
  (:documentation "A collector of news headlines from feed processes."))

(defclass rss-feed-reader ()
  ((title   :initarg :title   :reader rss-feed-reader-title   :initform nil)
   (url     :initarg :url     :reader rss-feed-reader-url     :initform nil)
   (process :initarg :process :reader rss-feed-reader-process :initform nil)
   (feed    :initarg :feed    :reader rss-feed-reader-feed    :initform nil))
  (:documentation "Maps a URL to a process that sends headlines to an aggregator."))

(defclass rss-headline ()
  ((feed :initarg :feed :reader rss-headline-feed :initform nil)
   (item :initarg :item :reader rss-headline-item :initform nil))
  (:documentation "Tracks an rss-item to its feed."))

(defmethod print-object ((aggregator rss-aggregator) stream)
  "Output an aggregator to a stream."
  (print-unreadable-object (aggregator stream :type t)
    (with-slots (name process headlines feeds)
        aggregator
      (format stream "~s (~:[inactive~;active~]) with ~d feed~:p and ~d headline~:p"
              name
              (process-alive-p process)
              (length feeds)
              (length headlines)))))

(defmethod print-object ((feed rss-feed-reader) stream)
  "Output a feed reader to a stream."
  (print-unreadable-object (feed stream :type t)
    (with-slots (process)
        feed
      (format stream "~s" (process-name process)))))

(defmethod print-object ((headline rss-headline) stream)
  "Output a headline to a stream."
  (print-unreadable-object (headline stream :type t)
    (with-slots (feed item)
        headline
      (format stream "~s via ~s" (rss-item-title item) (rss-feed-title feed)))))

(defmethod initialize-instance :after ((aggregator rss-aggregator) &key)
  "Immediately create the mailbox so feed readers can send stuff to it."
  (with-slots (mailbox)
      aggregator
    (setf mailbox (make-mailbox))))

(defmethod rss-aggregator-start ((aggregator rss-aggregator))
  "Spin up the reader processes."
  (with-slots (name mailbox process headlines callback)
      aggregator
    (labels ((headline-guid (h)
               (rss-item-guid (rss-headline-item h)))
             (headline-exists-p (h)
               (find (headline-guid h) headlines :key #'headline-guid :test #'string=))

             ;; the aggregation process
             (aggregate ()
               (loop (when-let (new (loop :for headline := (mailbox-wait-for-event mailbox :no-hang-p t)
                                          :while headline
                                          :unless (headline-exists-p headline)
                                          :collect headline))
                       (sys:atomic-exchange headlines (append new headlines))
                         
                       ;; notify other threads that a new headline is ready
                       (when callback
                         (funcall callback new)))
                       
                     ;; wait a bit so the cpu isn't hogged by lots of incoming headlines
                     (current-process-pause 0.1))))

      ;; start the aggregation process
      (unless process
        (setf process (process-run-function (or name "RSS Aggregator") () #'aggregate))))))

(defmethod rss-aggregator-stop ((aggregator rss-aggregator))
  "Stop all feed processes and the headline aggregator process. Feeds and headlines stay intact."
  (with-slots (mailbox process feeds)
      aggregator

    ;; stop processing all the feeds first
    (loop :for feed :in feeds :do (when-let (process (rss-feed-reader-process feed))
                                    (process-kill process)))

    ;; stop the aggregator process
    (process-kill process)

    ;; create a new mailbox
    (setf mailbox (make-mailbox))
    
    ;; clear the process data
    (setf process nil)))

(defmethod rss-aggregator-clear ((aggregator rss-aggregator))
  "Clear all the headlines from the aggregator."
  (with-slots (headlines)
      aggregator
    (sys:atomic-exchange headlines nil)))

(defmethod rss-aggregator-reset ((aggregator rss-aggregator))
  "Stop aggregating, clear headlines, and start again."
  (with-slots (headlines feeds mailbox)
      aggregator

    ;; stop, clear, and start
    (rss-aggregator-stop aggregator)
    (rss-aggregator-clear aggregator)
    (rss-aggregator-start aggregator)

    ;; loop over all the feeds and restart their processes
    (loop :for feed :in feeds :do (rss-aggregate-feed feed mailbox))))

(defmethod rss-aggregate ((aggregator rss-aggregator) url &key title)
  "Create a new feed process that will continuously push headlines to the aggregator."
  (with-slots (mailbox feeds)
      aggregator
    (with-url (url url)
      (let ((feed (make-instance 'rss-feed-reader :title title :url url)))
        (when (rss-aggregate-feed feed mailbox)
          (prog1 t (push feed feeds)))))))

(defmethod rss-aggregate-feed ((source rss-feed-reader) mailbox)
  "Starts the feed reader process that will send headlines to a mailbox."
  (with-slots (url title process feed)
      source
    (flet ((reader ()
             (loop (handler-case
                       (when (setf feed (rss-get url))
                         
                         ;; rename the process to that of the feed or url
                         (setf (process-name *current-process*) (or title (rss-feed-title feed) (format-url url)))
                             
                         ;; send all the headlines to the aggregator
                         (loop :for item :in (rss-feed-items feed)
                               :for headline := (make-instance 'rss-headline :feed feed :item item)
                               :do (mailbox-send mailbox headline)
                               :finally (current-process-pause (* (or (rss-feed-ttl feed) 5) 60))))

                     ;; if something bad happened, just wait and try again
                     (error (c)
                       (current-process-pause 300))))))
          
      ;; start the feed process only if the feed isn't currently in the feed list
      (setf process (process-run-function (format-url url) '() #'reader)))))

(defmethod rss-aggregator-headlines ((aggregator rss-aggregator) &key (count 100))
  "Get a sorted list of headlines from the aggregator."
  (with-slots (headlines)
      aggregator
    (loop :for n :below count
          :for headline :in (sort headlines #'> :key #'(lambda (h) (rss-item-date (rss-headline-item h))))
          :collect headline :into aggregated-headlines
          :finally (return aggregated-headlines))))
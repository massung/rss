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

(in-package :rss)

(defclass rss-aggregator ()
  ((process   :initform nil)
   (feeds     :initform nil)
   (headlines :initform nil)
   (mailbox   :initform nil)

   ;; notification condition lock
   (condition :initform (mp:make-condition-variable))
   (lock      :initform (mp:make-lock)))
  (:extra-initargs '(:feed-urls))
  (:documentation "A collection of news headlines and feed processes."))

(defclass rss-headline ()
  ((link :initarg :link :reader rss-headline-link)
   (feed :initarg :feed :reader rss-headline-feed)
   (item :initarg :item :reader rss-headline-item))
  (:documentation "A single, aggregated headline."))

(defmethod print-object ((headline rss-headline) stream)
  "Print a headline to a stream."
  (print-unreadable-object (headline stream :type t)
    (let ((item (rss-headline-item headline))
          (feed (rss-headline-feed headline)))
      (format stream "~s via ~s" (rss-item-title item) (rss-feed-title feed)))))

(defmethod initialize-instance :after ((agg rss-aggregator) &key feed-urls)
  "Initialize the aggregator and start aggregating feeds."
  (rss-aggregator-start agg)

  ;; set the list of feeds to start aggregating
  (setf (rss-aggregator-feeds agg) feed-urls))

(defmethod rss-aggregator-start ((agg rss-aggregator))
  "Create the aggregator process."
  (with-slots (process headlines mailbox condition lock)
      agg

    ;; stop the aggregator if it's running
    (when process
      (mp:process-kill process))

    ;; create a new mailbox
    (setf mailbox (mp:make-mailbox))

    ;; start the aggregation process
    (labels ((headline-guid (headline)
               (rss-item-guid (rss-headline-item headline)))
             (headline-exists-p (guid)
               (find guid headlines :test #'string-equal :key #'headline-guid))
             (aggregate ()
               (loop (when-let (headline (mp:mailbox-wait-for-event mailbox))
                       (let ((guid (rss-item-guid (rss-headline-item headline))))
                         (unless (headline-exists-p guid)
                           (sys:atomic-push headline headlines))
                         
                         ;; inform all threads waiting on new headlines that one is available
                         (mp:with-lock (lock)
                           (mp:condition-variable-broadcast condition))))

                     ;; wait before checking again, don't sleep if something is there
                     (mp:current-process-pause 0.1 #'mp:mailbox-not-empty-p mailbox))))
      (prog1
          nil
        (setf process (mp:process-run-function "RSS Aggregator" nil #'aggregate))))))

(defmethod rss-aggregator-stop ((agg rss-aggregator))
  "Stop all reader processes and the aggregation process. Headlines stay intact."
  (with-slots (process feeds)
      agg

    ;; stop processing all the feeds first
    (mapc #'mp:process-kill feeds)

    ;; clear the reader list
    (setf feeds nil)

    ;; stop the aggregation process
    (when process
      (mp:process-kill process))))

(defmethod rss-aggregator-clear ((agg rss-aggregator) &optional (before (get-universal-time)))
  "Clear all the headlines from the aggregator."
  (with-slots (headlines)
      agg
    (flet ((headline-date (h)
             (rss-item-date (rss-headline-item h))))
      (sys:atomic-exchange headlines (remove-if #'(lambda (h) (< (headline-date h) before)) headlines)))))

(defmethod rss-aggregator-reset ((agg rss-aggregator))
  "Clear headlines, stop aggregating, and restart all reader processes."
  (let ((feed-urls (rss-aggregator-feeds agg)))

    ;; stop all running feed processes and the aggregator process
    (rss-aggregator-stop agg)

    ;; clear the headlines and restart
    (rss-aggregator-clear agg)
    (rss-aggregator-start agg)
  
    ;; start feed processes again
    (setf (rss-aggregator-feeds agg) feed-urls)))

(defmethod rss-aggregator-headlines ((agg rss-aggregator) &optional (since 0))
  "Return all the headlines sorted since a given timestamp."
  (with-slots (headlines)
      agg
    (flet ((headline-date (h)
             (rss-item-date (rss-headline-item h))))
      (sort (remove-if-not #'(lambda (h) (> (headline-date h) since)) headlines) #'> :key #'headline-date))))

(defmethod rss-aggregator-wait-for-headlines ((agg rss-aggregator) &optional timeout)
  "Waits for new headlines to be available before continuing. Returns NIL if timeout elapsed instead."
  (with-slots (condition lock)
      agg
    (mp:with-lock (lock)
      (mp:condition-variable-wait condition lock :timeout timeout))))

(defmethod rss-aggregator-feeds ((agg rss-aggregator))
  "Return the list of URLs being aggregated."
  (with-slots (feeds)
      agg
    (mapcar #'mp:process-name feeds)))

(defmethod (setf rss-aggregator-feeds) (feed-urls (agg rss-aggregator))
  "Stop feeds no longer desired to be aggregated and start aggregating new feeds."
  (with-slots (feeds mailbox)
      agg

    ;; stop all feed processes
    (mapc #'mp:process-kill feeds)

    ;; clear the reader list
    (setf feeds nil)

    ;; the feed process loop
    (flet ((reader (url)
             (let (ttl)
               (loop (handler-case
                         (when-let (feed (rss-get url))
                           
                           ;; set the time-to-live value
                           (when-let (minutes (rss-feed-ttl feed))
                             (setf ttl (* minutes 60)))
                           
                           ;; send all the headlines to the aggregator
                           (dolist (item (rss-feed-items feed))
                             (mp:mailbox-send mailbox (make-instance 'rss-headline
                                                                     :link (mp:process-name mp:*current-process*)
                                                                     :feed feed
                                                                     :item item))))
                       
                       ;; something bad happened
                       (error (c) nil))
                     
                     ;; wait a bit before reading again
                     (mp:current-process-pause (or ttl 300))))))

      ;; start each feed process
      (dolist (url feed-urls)
        (let ((process (with-url (url url)
                         (mp:process-run-function (format-url url) '() #'reader url))))
          (push process feeds))))))
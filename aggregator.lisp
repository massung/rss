;;;; RSS Parer and Aggregator for ClozureCL
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

(defclass rss-aggregator ()
  ((lock      :initform (ccl:make-read-write-lock))
   (process   :initform ())
   (readers   :initform ())
   (headlines :initform ()))
  (:documentation "A collection of news feed readers."))

;;; ----------------------------------------------------

(defclass rss-reader ()
  ((process :initarg :process :reader rss-reader-process)
   (url     :initarg :url     :reader rss-reader-url)
   (feed    :initarg :feed    :reader rss-reader-feed))
  (:documentation "A process that is continuously polling an rss feed."))

;;; ----------------------------------------------------

(defclass rss-headline ()
  ((feed :initarg :feed :reader rss-headline-feed)
   (item :initarg :item :reader rss-headline-item))
  (:documentation "A single, aggregated headline."))

;;; ----------------------------------------------------

(defmethod print-object ((reader rss-reader) stream)
  "Print a reader to a stream."
  (print-unreadable-object (reader stream :type t)
    (prin1 (ccl:process-name (rss-reader-process reader) stream))))

;;; ----------------------------------------------------

(defmethod print-object ((headline rss-headline) stream)
  "Print a headline to a stream."
  (print-unreadable-object (headline stream :type t)
    (let ((item (rss-headline-item headline)))
      (prin1 (rss-item-title item) stream))))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((agg rss-aggregator) &key feed-urls (start t))
  "Initialize the aggregator and start aggregating feeds."
  (setf (rss-aggregator-feed-urls agg) feed-urls)

  ;; start the aggregator unless told not to
  (when start
    (rss-aggregator-start agg)))

(defmethod rss-aggregator-start ((agg rss-aggregator))
  "Start the feed reader processes that are not running."
  (with-slots (readers mailbox lock condition headlines process)
      agg
    (labels ((collect-headlines (r)
               (when-let (f (rss-reader-feed r))
                 (loop for i in (rss-feed-items f) collect (make-instance 'rss-headline :feed f :item i))))

             ;; wait for more headlines then re-aggregate
             (aggregate ()
               (loop (when (mp:mailbox-wait-for-event mailbox)

                       ;; collect all the headlines from the feeds
                       (sys:atomic-exchange headlines (mapcan #'collect-headlines readers))

                       ;; signal to any process waiting that there are new headlines
                       (mp:with-lock (lock)
                         (mp:condition-variable-broadcast condition))))))

      ;; start a process to aggregate headlines
      (setf process (mp:process-run-function "RSS Aggregator" nil #'aggregate)))))

(defmethod rss-aggregator-stop ((agg rss-aggregator))
  "Stop all reader processes and the aggregation process. Headlines stay intact."
  (with-slots (readers process)
      agg

    ;; stop all feed readers
    (loop for r in readers do (mp:process-kill (rss-reader-process r)))

    ;; stop aggregating headlines
    (when process
      (mp:process-kill process))))

(defmethod rss-aggregator-headlines ((agg rss-aggregator) &key (since 0))
  "Return all the headlines sorted since a given timestamp."
  (with-slots (headlines)
      agg
    (loop for h in headlines

          ;; get the item and publish date
          for i = (rss-headline-item h)
          for d = (rss-item-date i)

          ;; get all headlines since the desired time
          when (>= d since) collect h into hs

          ;; return the headlines sorted by date in descending order
          finally (return (sort hs #'> :key #'(lambda (h) (rss-item-date (rss-headline-item h))))))))

(defmethod rss-aggregator-wait-for-headlines ((agg rss-aggregator) &key timeout)
  "Waits for new headlines to be available before continuing. Returns NIL if timeout elapsed instead."
  (with-slots (condition lock)
      agg
    (mp:with-lock (lock)
      (mp:condition-variable-wait condition lock :timeout timeout))))

(defmethod rss-aggregator-feeds ((agg rss-aggregator))
  "Return the list of URLs being aggregated along with the latest feed downloaded."
  (with-slots (readers)
      agg
    (loop for r in readers collect (list (rss-reader-url r) (rss-reader-feed r)))))

(defmethod rss-aggregator-feed-urls ((agg rss-aggregator))
  "Return the list of URLs being aggregated."
  (with-slots (readers)
      agg
    (mapcar #'rss-reader-url readers)))

(defmethod (setf rss-aggregator-feed-urls) (feed-urls (agg rss-aggregator))
  "Stop feeds no longer desired to be aggregated and start aggregating new feeds."
  (with-slots (readers lock condition mailbox)
      agg

    ;; stop all the reader processes
    (dolist (reader readers)
      (mp:process-kill (rss-reader-process reader)))

    ;; create readers for each url
    (flet ((reader (r)
             (let (ttl)
               (loop (handler-case
                         (when-let (feed (rss-get (rss-reader-url r)))
                           (sys:atomic-exchange (slot-value r 'feed) feed)

                           ;; set the name of the process to the feed title
                           (setf (mp:process-name mp:*current-process*) (rss-feed-title feed))

                           ;; send the feed to the aggregator mailbox
                           (mp:mailbox-send mailbox feed)

                           ;; set the time-to-live value
                           (when-let (minutes (rss-feed-ttl feed))
                             (setf ttl (* minutes 60))))

                       ;; something bad happened, output a warning
                       (condition (c) (warn (princ-to-string c))))

                     ;; wait a bit before reading again
                     (mp:current-process-pause (or ttl 300))))))

      ;; create a reader process for each url
      (setf readers (loop for feed-url in feed-urls
                          for url = (copy-url feed-url)
                          for reader = (make-instance 'rss-reader :url url :feed nil)
                          for process = (mp:process-run-function (format-url url) nil #'reader reader)
                          do (setf (slot-value reader 'process) process)
                          collect reader)))))

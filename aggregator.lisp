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

(defclass rss-aggregator ()
  ((lock      :initform (make-read-write-lock))
   (condition :initform (make-semaphore))
   (readers   :initform ())
   (headlines :initform ()))
  (:documentation "A collection of news feed readers."))

;;; ----------------------------------------------------

(defclass rss-reader ()
  ((process :initarg :process :accessor rss-reader-process)
   (url     :initarg :url     :accessor rss-reader-url)
   (feed    :initarg :feed    :accessor rss-reader-feed))
  (:documentation "A process that is continuously polling an rss feed."))

;;; ----------------------------------------------------

(defclass rss-headline ()
  ((feed :initarg :feed :accessor rss-headline-feed)
   (item :initarg :item :accessor rss-headline-item))
  (:documentation "A single, aggregated headline."))

;;; ----------------------------------------------------

(defmethod print-object ((reader rss-reader) stream)
  "Print a reader to a stream."
  (print-unreadable-object (reader stream :type t)
    (prin1 (process-name (rss-reader-process reader)) stream)))

;;; ----------------------------------------------------

(defmethod print-object ((headline rss-headline) stream)
  "Print a headline to a stream."
  (print-unreadable-object (headline stream :type t)
    (let ((item (rss-headline-item headline)))
      (prin1 (rss-item-title item) stream))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-start ((agg rss-aggregator))
  "Start the feed reader processes that are not running."
  (with-slots (readers lock)
      agg
    (flet ((aggregate (reader)
             (do ((feed (rss-get (rss-reader-url reader))
                        (rss-get (rss-reader-url reader))))
                 ((null feed))
               (let ((ttl (rss-feed-ttl feed))
                     (title (rss-feed-title feed)))

                 ;; update the last feed of the reader
                 (setf (rss-reader-feed reader) feed)

                 ;; update the process name
                 (when title
                   (setf (process-name *current-process*) title))

                 ;; if the TTL isn't set, use a generous time
                 (setf ttl (or ttl 5))

                 ;; add all the items read to the aggregator headlines
                 (rss-aggregator-aggregate agg feed)

                 ;; wait TTL minutes for next iteration
                 (let ((stamp (+ (get-universal-time) (* ttl 60))))
                   (flet ((expired-p ()
                            (>= (get-universal-time) stamp)))
                     (process-wait "TTL wait..." #'expired-p)))))))

      (dolist (r readers)
        (setf (rss-reader-process r)
              (process-run-function (rss-reader-url r) #'aggregate r))))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-stop ((agg rss-aggregator))
  "Stop all reader processes and the aggregation process."
  (with-slots (readers)
      agg
    (loop for r in readers do (process-kill (rss-reader-process r)))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-aggregate ((agg rss-aggregator) (feed rss-feed))
  "Add all the items from a feed into the aggregator."
  (with-slots (lock condition headlines)
      agg
    (flet ((headline-guid (h)
             (rss-item-guid (rss-headline-item h)))
           (headline-date (h)
             (rss-item-date (rss-headline-item h))))

      ;; add new headlines to the aggregator
      (with-write-lock (lock)
        (dolist (item (rss-feed-items feed))
          (let* ((guid (rss-item-guid item))

                 ;; try and find the item already in the aggregator
                 (headline (find guid
                                   headlines
                                   :test #'string=
                                   :key #'headline-guid)))
              (if headline

                  ;; if the item has a newer date, update it
                  (when (> (rss-item-date item) (headline-date headline))
                    (setf (rss-headline-item headline) item
                          (rss-headline-feed headline) feed))

                ;; add the item to the headlines, count
                (let ((h (make-instance 'rss-headline
                                        :feed feed
                                        :item item)))
                  (push h headlines)))))))

    ;; signal that there are potentially new headlines
    (signal-semaphore condition)))

;;; ----------------------------------------------------

(defmethod rss-aggregator-headlines ((agg rss-aggregator) &key (since 0))
  "Return all the headlines sorted since a given timestamp."
  (with-slots (headlines lock)
      agg
    (with-read-lock (lock)
      (loop
         for h in headlines

         ;; get the item and publish date
         for i = (rss-headline-item h)
         for d = (rss-item-date i)

         ;; get all headlines since the desired time
         when (>= d since) collect h into hs

         ;; return the headlines sorted by date in descending order
         finally (flet ((headline-date (h)
                          (rss-item-date (rss-headline-item h))))
                   (return (sort hs #'> :key #'headline-date)))))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-wait-for-headlines ((agg rss-aggregator))
  "Waits for new headlines to be available before continuing."
  (with-slots (condition)
      agg
    (wait-on-semaphore condition)))

;;; ----------------------------------------------------

(defmethod rss-aggregator-feeds ((agg rss-aggregator))
  "Return the list of URLs being aggregated."
  (with-slots (readers)
      agg
    (mapcar #'rss-reader-url readers)))

;;; ----------------------------------------------------

(defmethod (setf rss-aggregator-feeds) (feed-urls (agg rss-aggregator))
  "Stop feeds no longer desired to be aggregated and start new feeds."
  (with-slots (readers)
      agg

    ;; stop all feed reader processes
    (rss-aggregator-stop agg)

    ;; create all the new feed reader objects
    (flet ((make-reader (url)
             (make-instance 'rss-reader :url url)))
      (setf readers (mapcar #'make-reader feed-urls)))

    ;; restart the feed reader processes
    (rss-aggregator-start agg)))

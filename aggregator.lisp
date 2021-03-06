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

(defclass rss-aggregator ()
  ((lock      :initform (make-mutex))

   ;; no time of last aggregation
   (stamp     :initform 0)

   ;; no feed readers and no headlines aggregated
   (readers   :initform nil)
   (headlines :initform nil))
  (:documentation "A collection of news feed readers."))

;;; ----------------------------------------------------

(defclass rss-reader ()
  ((thread :initarg :thread :accessor rss-reader-thread)
   (url    :initarg :url    :accessor rss-reader-url)
   (feed   :initarg :feed   :accessor rss-reader-feed))
  (:documentation "A thread that is continuously polling an rss feed."))

;;; ----------------------------------------------------

(defclass rss-headline ()
  ((feed :initarg :feed :accessor rss-headline-feed)
   (item :initarg :item :accessor rss-headline-item)
   (mark :initarg :mark :accessor rss-headline-mark))
  (:documentation "A single, aggregated headline."))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((agg rss-aggregator) &key feed-urls)
  "Initialize the aggregator with an initial set of feeds."
  (setf (rss-aggregator-feed-urls agg) feed-urls))

;;; ----------------------------------------------------

(defmethod print-object ((reader rss-reader) stream)
  "Print a reader to a stream."
  (print-unreadable-object (reader stream :type t)
    (prin1 (thread-name (rss-reader-thread reader)) stream)))

;;; ----------------------------------------------------

(defmethod print-object ((headline rss-headline) stream)
  "Print a headline to a stream."
  (print-unreadable-object (headline stream :type t)
    (let ((item (rss-headline-item headline)))
      (prin1 (rss-item-title item) stream))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-stop ((agg rss-aggregator))
  "Stop all reader threads and the aggregation thread."
  (with-slots (readers)
      agg
    (dolist (r readers)
      (terminate-thread (rss-reader-thread r)))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-start ((agg rss-aggregator) url)
  "Start a new feed reader thread."
  (let ((r (make-instance 'rss-reader :url url :feed nil)))
    (prog1 r

      ;; create the thread that will aggregate the headlines
      (flet ((aggregate ()
               (do ((feed (rss-get (rss-reader-url r))
                          (rss-get (rss-reader-url r))))
                   ((null feed))

                 ;; set the feed of the reader
                 (setf (rss-reader-feed r) feed)

                 ;; aggregate all the headlines in the feed
                 (rss-aggregator-aggregate agg feed)

                 ;; update the name of the thread to the feed's title
                 (let ((title (rss-feed-title feed)))
                   (when title
                     (setf (thread-name *current-thread*) title)))

                 ;; get the time-to-live (default to 5 minutes)
                 (let* ((ttl (or (rss-feed-ttl feed) 5))
                        (time (+ (get-universal-time) (* ttl 60))))
                   (flet ((expired-p ()
                            (>= (get-universal-time) time)))
                     (wait-for #'expired-p))))))

        ;; start the reader thread
        (setf (rss-reader-thread r)
              (make-thread #'aggregate :name (princ-to-string url)))))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-aggregate ((agg rss-aggregator) (feed rss-feed))
  "Add all the items from a feed into the aggregator."
  (with-slots (lock stamp headlines)
      agg
    (flet ((headline-guid (h)
             (rss-item-guid (rss-headline-item h)))
           (headline-date (h)
             (rss-item-date (rss-headline-item h))))

      ;; add new headlines to the aggregator
      (with-mutex (lock)
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
                                        :mark nil
                                        :feed feed
                                        :item item)))
                  (push h headlines)))))

        ;; finally, update the time the last aggregate happened
        (setf stamp (get-internal-real-time))))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-headlines ((agg rss-aggregator) &key since)
  "Return all the headlines sorted since a given timestamp."
  (with-slots (headlines lock)
      agg
    (with-mutex (lock)
      (loop
         for h in headlines

         ;; get the item and publish date
         for i = (rss-headline-item h)
         for d = (rss-item-date i)

         ;; get all headlines since the desired time
         when (or (null since) (>= d since)) collect h into hs

         ;; return the headlines sorted by date in descending order
         finally (flet ((headline-date (h)
                          (rss-item-date (rss-headline-item h))))
                   (return (sort hs #'> :key #'headline-date)))))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-forget ((agg rss-aggregator) &key before)
  "Forget all headlines before a given time."
  (with-slots (lock headlines)
      agg
    (flet ((old-p (h)
             (< (rss-item-date (rss-headline-item h)) before)))
      (with-mutex (lock)
        (setf headlines
              (if (null before)
                  nil
                (remove-if #'old-p headlines)))))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-wait ((agg rss-aggregator) &key since timeout)
  "Waits for new headlines to be available before continuing."
  (with-slots (stamp lock)
      agg
    (flet ((updated-p ()
             (or (null since)
                 (with-mutex (lock)
                   (> stamp since)))))
      (when (wait-for (updated-p) :timeout timeout)
        (values t stamp)))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-feeds ((agg rss-aggregator))
  "Return the list of RSS feeds that have been parsed."
  (with-slots (lock readers)
      agg
    (with-mutex (lock)
      (remove nil (mapcar #'rss-reader-feed readers)))))

;;; ----------------------------------------------------

(defmethod rss-aggregator-feed-urls ((agg rss-aggregator))
  "Return the list of URLs being aggregated."
  (with-slots (lock readers)
      agg
    (with-mutex (lock)
      (mapcar #'rss-reader-url readers))))

;;; ----------------------------------------------------

(defmethod (setf rss-aggregator-feed-urls) (urls (agg rss-aggregator))
  "Stop feeds that aren't in the new URL list."
  (with-slots (lock (hs headlines) readers)
      agg
    (let* ((parsed-urls (mapcar #'url-parse urls))

           ;; make sure there are no duplicate URLs
           (urls (remove-duplicates parsed-urls :test #'url-equal)))

      ;; kill feed readers and forget headlines
      (with-mutex (lock)
        (loop

           ;; loop over all the current readers
           for r in readers

           ;; is the reader url in the list of urls to keep?
           unless (find (rss-reader-url r) urls :test #'url-equal)

           ;; kill the thread for the reader
           do (let ((f (rss-reader-feed r))
                    (p (rss-reader-thread r)))
                (when (thread-alive-p p)
                  (terminate-thread p))

                ;; forget any headlines from this feed
                (setf hs (remove f hs :key #'rss-headline-feed))))

        ;; create new readers and keep existing ones
        (setf readers
              (loop

                 ;; loop over all the urls
                 while urls for url = (url-parse (pop urls))

                 ;; is this url already being aggregated?
                 for reader = (find url
                                    readers
                                    :test #'url-equal
                                    :key #'rss-reader-url)

                 ;; collect the existing reader or create a new one
                 collect (if reader
                             reader
                           (rss-aggregator-start agg url))))))

    ;; finally, return the new list of urls
    (rss-aggregator-feed-urls agg)))

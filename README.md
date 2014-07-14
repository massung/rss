# RSS Parser and Aggregator for LispWorks

A simple RSS package for [LispWorks](http://www.lispworks.com) that uses the [XML](http://github.com/massung/xml), [HTTP](http://github.com/massung/http), and [date](http://github.com/massung/date) packages to parse RSS feeds and aggregate headlines.

## Quickstart

Simply call the `rss-get` function with a URL to download and parse the feed.

	CL-USER > (rss-get "http://www.npr.org/rss/rss.php?id=1001")
	#<RSS::RSS-FEED "News" (15 items)>

The `rss-feed` class has the following reader methods:

	(rss-feed-title feed)    ;=> string
	(rss-feed-subtitle feed) ;=> string
	(rss-feed-link feed)     ;=> url
	(rss-feed-date feed)     ;=> univeral time
	(rss-feed-ttl feed)      ;=> time-to-live minutes
	(rss-feed-image feed)    ;=> url
	(rss-feed-icon feed)     ;=> url
	(rss-feed-items feed)    ;=> list of rss-item
	
The `rss-item` class has these reader methods:

	(rss-item-title item)    ;=> string
	(rss-item-summary item)  ;=> string
	(rss-item-link item)     ;=> url
	(rss-item-content item)  ;=> list of rss-content
	(rss-item-date item)     ;=> univeral time
	(rss-item-author item)   ;=> string
	(rss-item-guid item)     ;=> string
	
Each item can have various content types embedded within it. You can use the `rss-content-find` function to search an `rss-item` for a given type of content.

	(rss-content-find item type)

The `rss-content` class has the following reader methods:

	(rss-content-type content)    ;=> string
	(rss-content-summary content) ;=> string
	(rss-content-link content)    ;=> url

## Aggregating RSS Feeds

The `rss` package also comes with a feed aggregator. This is a class that will continuously fetch a list of feeds and aggregate the headlines they return.

First, create an aggregator, passing in an optional list of feeds to aggregate.

	CL-USER > (setf agg (make-instance 'rss-aggregator :feed-urls (list "digg.com/rss/top.rss" "www.joystiq.com/rss.xml")))
	#<RSS-AGGREGATOR 200CBE2F>
	
At this point, if you pull up the LispWorks process list, you will see there is a process running called "RSS Aggregator" and two other processes, which are the feed reader processes matching the URLs.

Let's see what's been aggregated so far...

	CL-USER > (rss-aggregator-headlines agg)
	(#<RSS::RSS-HEADLINE "A Damaging Distance" via "Digg Top Stories">
	 #<RSS::RSS-HEADLINE "Firefall officially heats things up on July 29" via "Joystiq RSS Feed">
	 ...)

The `rss-aggregator-headlines` method returns the headlines in sorted order. It also takes an optional argument, which is the universal time - before which - headlines will not be returned:

	(rss-aggregator-headlines aggregator &optional since)
	
By default, `since` is 0 (all headlines will be returned).

Now, obviously it would be a pain to have to continuously poll the aggregator for new headlines. To avoid this, use the `rss-aggregator-wait-for-headlines` function.

	(rss-aggregator-wait-for-headlines aggregator &optional timeout)

This function returns `NIL` if *timeout* elapsed and there were no new headlines or `T` if new headlines are waiting to be read.

*Note: Under-the-hood this uses a condition variable, is thread-safe, and many threads can call it and will all receive the signal as the condition is broadcast when new headlines are available.*

To stop aggregating and kill all processes, simply call `rss-aggregator-stop`. And to start the aggregator process again, simply call `rss-aggregator-start`.

	(rss-aggregator-stop aggregator)
	(rss-aggregator-start aggregator)
	
*Note: If you stop the aggregator, calling the start method will simply just restart the aggregation process and not the individual feed processes. You'll need to start those again by using `rss-aggregator-feeds`.*

To clear all headlines, call `rss-aggregator-clear`.

	(rss-aggregator-clear aggregator &optional before)
	
The `before` argument defaults to the current, universal time, and is the time which, any headlines before that are removed from the aggregator.

If you need to stop, clear, start, and begin aggregating the same RSS feeds again, use `rss-aggregator-reset`.

	(rss-aggregator-reset aggregator)
	
Finally, to get a list of all the feed URLs being aggregated, simply call `rss-aggregator-feeds`.

	CL-USER > (rss-aggregator-feeds agg)
	("http://www.joystiq.com/rss.xml"
	 "http://digg.com/rss/top.rss")

This method is also `setf`-able, and will handle terminating any readers that are not in the list and start aggregating feeds that are new.

	CL-USER > (setf (rss-aggregator-feeds agg) (list "www.npr.org/rss/rss.php?id=1004"))
	NIL

	CL-USER > (rss-aggregator-feeds agg)
	("http://www.npr.org/rss/rss.php?id=1004")

*Note: changing the reader list doesn't clear the headlines of any feeds you no longer wish to aggregate. So, clear the headlines first if you would like to start fresh.*

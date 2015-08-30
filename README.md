# RSS Parser and Aggregator for Clozure CL

A simple RSS package for [Clozure CL](http://ccl.clozure.com). It depends on the following packages:

* [URL](http://github.com/massung/url)
* [XML](http://github.com/massung/xml)
* [HTTP](http://github.com/massung/http)
* [RFC-DATE](http://github.com/massung/rfc-date)

## Quickstart

Simply call the `rss-get` function with a URL to download and parse the feed.

    CL-USER > (rss-get "http://www.npr.org/rss/rss.php?id=1001")
    #<RSS::RSS-FEED "News" (15 items)>

The `rss-feed` class has the following reader methods:

    (rss-feed-title feed)         ;=> string
    (rss-feed-subtitle feed)      ;=> string
    (rss-feed-link feed)          ;=> url
    (rss-feed-date feed)          ;=> time
    (rss-feed-ttl feed)           ;=> fixnum (minutes)
    (rss-feed-image feed)         ;=> url
    (rss-feed-icon feed)          ;=> url
    (rss-feed-items feed)         ;=> rss-items

The `rss-item` class has these reader methods:

    (rss-item-title item)         ;=> string
    (rss-item-summary item)       ;=> string
    (rss-item-link item)          ;=> url
    (rss-item-content item)       ;=> rss-contents
    (rss-item-categories item)    ;=> strings
    (rss-item-date item)          ;=> time
    (rss-item-author item)        ;=> string
    (rss-item-guid item)          ;=> string

The `rss-content` class has the following reader methods:

    (rss-content-type content)    ;=> content-type
    (rss-content-summary content) ;=> string
    (rss-content-link content)    ;=> url

Each item can have various content types embedded within it. You can use the `rss-content-find` function to search an `rss-item` for a given type of content.

    (rss-content-find item type &optional subtype)

For example, if you want to find an image associated with an item, you might try:

    (rss-content-find item "image")

## Aggregating RSS Feeds

The `rss` package also comes with a feed aggregator that will spawn a process per feed being aggregated, and sort all the headlines by time. You can pause the current process until there are new headlines to be read as well.

First, create an aggregator:

    CL-USER > (setf agg (make-instance 'rss-aggregator))
    #<RSS-AGGREGATOR 200CBE2F>

Next, add a feed to it...

    CL-USER > (push "digg.com/rss/top.rss" (rss-aggregator-feed-urls agg))
    (http://digg.com/rss/top.rss)

At this point, the aggregator is processing the feed, and there are likely already headlines ready to be inspected. Let's wait, though, just in case.

    CL-USER > (rss-aggregator-wait agg)
    T
    3649785684

The `T` indicates that at least one feed has been read since the time specified and the wait didn't time out. It also returns the universal time when the feed was processed.

Now, let's try waiting again, but since the last feed was read and with a timeout. *Note: the aggregator uses the TTL specified in the feed, so most feeds will only update every 15 minutes or so.*

    CL-USER > (rss-aggregator-wait agg :since 3649785684 :timeout 10)
    NIL

It took 10 seconds, but we timed out. So, instead, let's go look and see what's been aggregated so far...

    CL-USER > (rss-aggregator-headlines agg)
    (#<RSS::RSS-HEADLINE "A Damaging Distance">
     #<RSS::RSS-HEADLINE "Firefall officially heats things up on July 29">
     ...)

The `rss-aggregator-headlines` method returns the headlines in sorted order. It also takes an optional argument, which is the universal time - before which - headlines will not be returned:

    (rss-aggregator-headlines aggregator &key since)

By default, *since* is 0, so all headlines will be returned.

To stop aggregating and kill all feed processes, simply call `rss-aggregator-stop`.

    (rss-aggregator-stop aggregator)

To remove headlines that have been aggregated, call `rss-aggregator-forget`.

    (rss-aggregator-forget aggregator &key before)

The `before` argument defaults to the current, universal time, and is the time which, any headlines before that are removed from the aggregator's headlines list.

*Note: forgetting a headline doesn't mean it may not reappear! If a feed still has the headline, it'll just reappear later.*

To get a list of all the feed URLs being aggregated, simply call `rss-aggregator-feed-urls`.

    CL-USER > (rss-aggregator-feed-urls agg)
    (http://digg.com/rss/top.rss)

This method is also `setf`-able (which is how we **push**ed our initial feed url onto the aggregator), and will handle terminating any readers that are not in the list and start aggregating feeds that are new.

    CL-USER > (setf (rss-aggregator-feed-urls agg) (list "www.npr.org/rss/rss.php?id=1004"))
    NIL

    CL-USER > (rss-aggregator-feed-urls agg)
    (http://www.npr.org/rss/rss.php?id=1004)

*Note: changing the reader list will forget the headlines of any feeds you no longer wish to aggregate.*

Finally, if you'd like a list of all the RSS feeds that have been aggregated, use the `rss-aggregator-feeds` method.

    CL-USER > (rss-aggregator-feeds agg)
    (#<RSS-FEED "World : NPR" (15 items)>)

That's it!

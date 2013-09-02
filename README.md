# RSS Parser for LispWorks

A simple RSS package for [LispWorks](http://www.lispworks.com) that uses the [XML](http://github.com/massung/xml), [HTTP](http://github.com/massung/http), and [date](http://github.com/massung/date) packages to parse RSS feeds.

## Quickstart

Simply call the `rss-get` function with a URL to download and parse the feed.

	CL-USER > (rss-get "http://www.npr.org/rss/rss.php?id=1001")
	#<RSS::RSS-FEED "News">

The `rss-feed` and `rss-item` classes both derive from the same base-class: `rss-node`, which houses the `xml-node` for that element in the RSS. The base method `rss-query` can be used to extract information from an `rss-node`:

	(rss-query node element &key (if-found #'identity) if-not-found)
	
The optional `if-found` argument is a function that will be applied to the `node-value` of the element (if found) before returning it. The `if-not-found` will be returned if the element doesn't exist in the node.

	CL-USER > (rss-query * "title" :if-found #'string-upcase)
	"NEWS"

A plethora of common query functions exist to assist you:

	(rss-title node)        ;=> string
	(rss-link node)         ;=> url
	(rss-description node)  ;=> string
	(rss-image node)        ;=> url
	(rss-categories node)   ;=> list
	(rss-content node)      ;=> string
	(rss-date node)         ;=> universal-time

Some functions only are exposed on an `rss-feed` object:

	(rss-items feed)        ;=> list
	(rss-ttl feed)          ;=> integer
	
And some for the `rss-item` class:

	(rss-guid item)         ;=> string


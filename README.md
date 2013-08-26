# RSS Parser for LispWorks

A simple RSS package for [LispWorks](http://www.lispworks.com) that uses the [XML](http://github.com/massung/xml) and [HTTP](http://github.com/massung/http) packages to parse RSS feeds.

## Quickstart

Once all the dependency packages and the `rss` package have been loaded, simply call the `read-rss` function with a URL to download and parse the feed.

	CL-USER > (read-rss "http://www.npr.org/rss/rss.php?id=1001")
	#<RSS::RSS-FEED "News">

Use `feed-items` to get a list of all the headlines in the feed.

	CL-USER > (feed-items *)
	(#<RSS::RSS-ITEM "Quitting Your Job For Fantasy Football"> ...)

Fetch the `item-description` of the first headline.

	CL-USER > (item-description (first *))
	"Drew Dinkmeyer was an investment analyst. Pretty steady job, ..."

Or open it up in the browser...

	CL-USER > (sys:open-url (item-link (first **)))
	#<HQN-WEB::DARWIN-IC-BROWSER 21F2FEF3>

## Exported Methods

Aside from `read-rss`, the only exported functions are `rss-feed` and `rss-item` accessors. All the slots can be `nil`, so be sure and check if you don't want conditions signaled.

	;; rss-feed accessors
	(feed-title feed)          ;=> string
	(feed-link feed)           ;=> string
	(feed-description feed)    ;=> string
	(feed-categories feed)     ;=> list
	(feed-image feed)          ;=> string
	(feed-ttl feed)            ;=> integer
	(feed-items feed)          ;=> list
	
	;; rss-item accessors
	(item-title item)          ;=> string
	(item-link item)           ;=> string
	(item-description item)    ;=> string
	(item-categories item)     ;=> string
	(item-pub-date item)       ;=> string
	(item-guid item)           ;=> string

## TODO

I'd like to add parsing for the publish date so they can be sorted.

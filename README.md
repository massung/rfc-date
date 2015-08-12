# Date Parsing for Common Lisp

The `rfc-date` package is a small, portable set of functions for encoding and decoding various internet date/time formats to and from Common Lisp [universal times](http://www.lispworks.com/documentation/HyperSpec/Body/25_adb.htm). It requires the [`re`](http://github.com/massung/re) package, which is also a portable, Common Lisp library.

## Quickstart

Parsing an internet date/time string is done using one function:

    (encode-universal-rfc-time string &optional format)

The *format* parameter should be a keyword identifying one of the supported internet formats (**:rfc822, :rfc850, :rfc1036, :rfc1123, :rfc2822, :rfc3339, :atom, :cookie, :rss, :iso8601, or :w3c**), and defaults to **:rfc822**.

Decoding a universal time into a string done with the reverse, decoding function:

    (decode-universal-rfc-time universal-time &optional format)

Again, the *format* parameter defaults to **:rfc822**.

## Example Usage

    CL-USER > (decode-universal-rfc-time (get-universal-time) :rfc3339)
    "2013-08-29T12:09:30-06:00"

    CL-USER > (encode-universal-rfc3339-time * :rfc3339)
    3586745370

## Testing

If you'd like to test each of the formats, see how they end up printing dates, etc, you can use the (unexported) function `rfc-date::unit-test`. It optionally takes a universal time argument that defaults to `get-universal-time`. It will run through all the supported formats, convert the time to a string, then parse the string and verify that it got the same time back out.

## Converting Formats

While no helper function exists in the package for converting between date/time formats, it's easy enough to code one up...

    (defun convert-rfc-time (time-string from to)
      "Parse a date/time string in one format and conver it to another."
      (decode-universal-rfc-time (encode-universal-rfc-time time-string from) to))

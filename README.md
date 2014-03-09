# Date Parsing for LispWorks

The `rfc-date` package is a small [RFC822](http://tools.ietf.org/html/rfc2822.html#section-3.3) and [RFC3339](http://www.ietf.org/rfc/rfc3339.txt) internet date parsing formats. It can also convert a universal time to those formats as well.

## Quickstart

Parsing an internet date/time string is done using one of two functions:

	(encode-universal-rfc822-time string)
	(encode-universal-rfc3339-time string)

Decoding a universal time back to a string can be done with the functions:

	(decode-universal-rfc822-time universal-time)
	(decode-universal-rfc3339-time universal-time)

## Examples

	CL-USER > (decode-universal-rfc3339-time (get-universal-time))
	"2013-08-29T12:09:30-06:00"

	CL-USER > (encode-universal-rfc3339-time *)
	3586745370
	NIL

#!/usr/bin/ruby -w
####
#### Simple testing against some high-level AmiGO::WebApp stuff.
####  -or-
#### Checking to see how exactly the posting mechanism works in CGI::App...
####

require 'json'
require 'httpclient'
#require 'uri'

client = HTTPClient.new

## okay
JSON.parse(client.get_content "http://localhost/cgi-bin/amigo/form_test?mode=simple", {'c' => 'b', 'f' => File.new('/tmp/test.txt')})

## bad (same as kappa...)
JSON.parse(client.get_post "http://localhost/cgi-bin/amigo/form_test?mode=simple", {'c' => 'b', 'f' => File.new('/tmp/test.txt')})

## okay
client.request 'GET', "http://localhost/cgi-bin/amigo/form_test", {'mode' => 'simple', 'bar' => 'bibble', 'file' => File.open('/tmp/test.txt')}

## bad
client.request 'POST', "http://localhost/cgi-bin/amigo/form_test", {'mode' => 'simple', 'bar' => 'bibble', 'file' => File.open('/tmp/test.txt')}

# okay
#curl -d "mode=simple&pa5&id=blablabla&ding=submit" "http://localhost/cgi-bin/amigo/form_test"

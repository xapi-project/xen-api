## 2.5.0 (2018-07-04)
- Port to jbuilder
- Make safe-string compatible

## 2.4.0 (2017-06-17):
- Replace io-page.unix with io-page-unix
- Update to the Mirage 3 BLOCK interfaces

## 2.3.0 (2017-02-09):
- Replace camlp4 with ppx

## 2.2.0 (2016-04-18):
- Bug fixes (including API changes) for concurrency issues
- Switch to Lwt log

## 2.0.0 (2015-04-30):
- Ring is now functorised over a LOGging module
- Switch to a Rresult-like error type
- Add debug information into the producer and consumer sectors
- Don't treat all-zeroes as valid data

## 1.0.0 (2015-03-06):
- initial stable API, including both a "Ring" and a "Journal"

## 0.9.0 (2015-01-19):
- initial proof-of-concept release


# dev-lib
Personal use spying/logging/checking library. Only intended for use in development code. The spying functions are of
the form `probe-*`, the logging functions of the form `log-*` and the checking functions of the form `safe-*`. The
alias used is always `dev`. 

The intention is that this library is never included in a maven artifact. All the functions are non-standard and 
unnecessary, yet sometimes useful to me when writing fresh code. Can be kept for code that will only be used for 
unit tests and more manual visual verification tests.   

# Steps for production quality code
* Rid of most asserts, replacing with guardrails and `(throw (ex-info \"Some bad error\" {}))`
* Rid of `log-*` functions. Either remove them or use a proper logging library
* Rid of spying functions (`probe-*`)
* Remove the `safe-` prefix from functions

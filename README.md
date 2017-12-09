REGEX - regular expression library in CommonLisp
================================================================
REGEX is POSIX compatible regular expression library.

## Syntax
REGEX supports following syntax.

- `.` 
- `[ ]` see also [Character classes](#Character classes)
- `[^ ]`
- `*`
- `+`
- `?`
- `{n[,[m]]}`
- `|`
- `( )`
- `\n`

see also [Regular expression - Wikipedia](https://en.wikipedia.org/wiki/Regular_expression)

### Character classes
REGEX supports following character classes

- `[:alnum:]` is expanded to `[A-Za-z0-9]`
- `[:alpha:]` is expanded to `[A-Za-z]`
- `[:blank:]` is expanded to `[ \t]`
- `[:lower:]` is expanded to `[a-z]`
- `[:upper:]` is expanded to `[A-Z]`
- `[:punct:]` is expanded to ``[\]\[!"#$%&'()*+,./:;<=>?@\^_`{|}~-]``
- `[:digit:]` is expanded to `[0-9]`
- `[:xdigit:]` is expanded to `[0-9A-Fa-f]`

## Benchmarks

vs cl-ppcre

~~~shellscript
$ ./t/glep.lisp "defun" ./src/*

|======== ppcre-glep ========|
17 hits
Evaluation took:
  0.006 seconds of real time
  0.008000 seconds of total run time (0.008000 user, 0.000000 system)
  133.33% CPU
  16 lambdas converted
  14,587,828 processor cycles
  1,635,520 bytes consed
  

|======== glep-interp ========|
17 hits
Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  2,762,600 processor cycles
  719,792 bytes consed
  

|======== glep ========|
17 hits
Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  2,785,544 processor cycles
  719,744 bytes consed
  
~~~

~~~shellscript
$ ./t/glep.lisp "def.*" ./src/*

|======== ppcre-glep ========|
35 hits
Evaluation took:
  0.008 seconds of real time
  0.008000 seconds of total run time (0.008000 user, 0.000000 system)
  100.00% CPU
  16 lambdas converted
  17,093,525 processor cycles
  1,799,008 bytes consed
  

|======== glep-interp ========|
35 hits
Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  2,573,234 processor cycles
  818,000 bytes consed
  

|======== glep ========|
35 hits
Evaluation took:
  0.001 seconds of real time
  0.004000 seconds of total run time (0.004000 user, 0.000000 system)
  400.00% CPU
  2,879,968 processor cycles
  818,304 bytes consed
  
~~~

~~~lisp
$ ./t/glep.lisp "a+b*.*[^abc]$" ./src/*

|======== ppcre-glep ========|
249 hits
Evaluation took:
  0.011 seconds of real time
  0.008000 seconds of total run time (0.008000 user, 0.000000 system)
  72.73% CPU
  41 lambdas converted
  24,666,103 processor cycles
  3,076,016 bytes consed
  

|======== glep-interp ========|
249 hits
Evaluation took:
  0.002 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  3,377,599 processor cycles
  1,997,360 bytes consed
  

|======== glep ========|
249 hits
Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  3,366,872 processor cycles
  1,997,024 bytes consed
  
~~~

## Usage
- `(re:compile-regex regex)`
- `(re:scan regex target)`
- `(re:regex-replace regex target string :global t)`
- `(re:regex-replace regex target string :global nil)`

## Requirements
- cl-cont

## License
The REGEX source code is licensed under the terms of the Lisp Lesser GNU Public License, known as the LLGPL. The LLGPL consists of a preamble and the LGPL. Where these conflict, the preamble takes precedence. This project is referenced in the preamble as the LIBRARY.
## Author
asciian (asciian@outlook.jp)

## Copyright
Copyright (c) 2015 asciian (asciian@outlook.jp)


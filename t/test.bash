#! /bin/bash
PROJECT_ROOT="$(cd $(dirname $0); pwd)/../"

cat <<EOF > ${PROJECT_ROOT}/README.md
REGEX - regular expression library in CommonLisp
================================================================
REGEX is POSIX compatible regular expression library.

## Syntax
REGEX supports following syntax.

- \`.\` 
- \`[ ]\` see also [Character classes](#Character classes)
- \`[^ ]\`
- \`*\`
- \`+\`
- \`?\`
- \`{n[,[m]]}\`
- \`|\`
- \`( )\`
- \`\n\`

see also [Regular expression - Wikipedia](https://en.wikipedia.org/wiki/Regular_expression)

### Character classes
REGEX supports following character classes

- \`[:alnum:]\` is expanded to \`[A-Za-z0-9]\`
- \`[:alpha:]\` is expanded to \`[A-Za-z]\`
- \`[:blank:]\` is expanded to \`[ \t]\`
- \`[:lower:]\` is expanded to \`[a-z]\`
- \`[:upper:]\` is expanded to \`[A-Z]\`
- \`[:punct:]\` is expanded to \`\`[\]\[!"#$%&'()*+,./:;<=>?@\\^_\`{|}~-]\`\`
- \`[:digit:]\` is expanded to \`[0-9]\`
- \`[:xdigit:]\` is expanded to \`[0-9A-Fa-f]\`

## Benchmarks

vs cl-ppcre

~~~shellscript
$ ./t/glep.lisp "defun" ./src/*
`${PROJECT_ROOT}/t/glep.lisp "defun" ${PROJECT_ROOT}/src/*`
~~~

~~~shellscript
$ ./t/glep.lisp "def.*" ./src/*
`${PROJECT_ROOT}/t/glep.lisp "def.*" ${PROJECT_ROOT}/src/*`
~~~

~~~lisp
$ ./t/glep.lisp "a+b*.*[^abc]$" ./src/*
`${PROJECT_ROOT}/t/glep.lisp "a+b*.*[^abc]$" ${PROJECT_ROOT}/src/*`
~~~

## Usage
- \`(re:compile-regex regex)\`
- \`(re:scan regex target)\`
- \`(re:regex-replace regex target string :global t)\`
- \`(re:regex-replace regex target string :global nil)\`

## Requirements
- cl-cont

## License
The REGEX source code is licensed under the terms of the Lisp Lesser GNU Public License, known as the LLGPL. The LLGPL consists of a preamble and the LGPL. Where these conflict, the preamble takes precedence. This project is referenced in the preamble as the LIBRARY.
## Author
TANIGUCHI Masaya (ta2gch@gmail.com)

## Copyright
Copyright (c) 2015 Masaya TANIGUCHI (ta2gch@gmail.com)

EOF

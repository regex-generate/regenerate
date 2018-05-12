[Regenerate][web] [![Build Status](https://travis-ci.org/Drup/regenerate.svg?branch=master)](https://travis-ci.org/Drup/regenerate) [![Opam](https://img.shields.io/badge/opam-0.1-green.svg)](https://opam.ocaml.org/packages/regenerate/) [![doc](https://img.shields.io/badge/doc-online-green.svg)][docdev]
----------

Regenerate is a tool to generate test-cases for regular expression engines.

Regenerate takes a [*regular* expression][regex] and generates strings that match it.
It handles most posix extended regular expressions along with
complement (`~a`) and intersection (`a&b`).
Since it handles complement, it can also generate strings that
*don't* match a given regular expression.

Regenerate is both a tool to generate samples and a library to define test harnesses. There is also an [online demo][web] of the tool.

## Command line tool

The command line tool can generate example strings matching a given regex. it takes
a regex using the POSIX extended regular expression syntax (or [ERE][] for short). For example, here
is a set of samples that are matched by the regex `(1(01*0)*1|0)*` on the alphabet composed of `0` and `1`:

```
% regenerate gen --sample 5 --alphabet "01" "(1(01*0)*1|0)*"
01001
11000
000011
001001
001100
011000
111111
0000110
0011000
0101101
1011101
1101001
00010101
00100100
```

Please consult the help for more details, or play with the [online demo][web].

## Library

The `regenerate` library allow to easily define test harnesses for regular expression engines. It contains utilities to randomly generate regular expressions and
associated positive and negative samples. The main entry point of the library
is the `Regenerate.arbitrary` function which exposes the sample generation as a 
[qcheck][] generator.

See [`test_re.ml`](test/re/test_re.ml) for an example test harness for
[`re`](https://github.com/ocaml/ocaml-re). 
You can also find (wip) [documentation for the dev version of the API][docdev].

## Website

Code for the online demo is hosted in the [web/](web) directory. It uses fairly
simple [`js_of_ocaml`][jsoo] code.

[regex]: https://en.wikipedia.org/wiki/Regular_expression
[web]: https://drup.github.io/regenerate/
[ERE]: https://en.wikipedia.org/wiki/Regular_expression#Standards
[jsoo]: http://ocsigen.org/js_of_ocaml
[docdev]: https://drup.github.io/regenerate/doc/dev/regenerate/Regenerate/
[qcheck]: https://github.com/c-cube/qcheck/

(lang dune 3.17)

(name imandrax-tools)

(version 0.1)

; Pin imandrakit packages to GitHub repository
(pin
 (url "git+https://github.com/imandra-ai/imandrakit")
 (package (name imandrakit)))

(pin
 (url "git+https://github.com/imandra-ai/imandrakit")
 (package (name imandrakit-io)))

(pin
 (url "git+https://github.com/imandra-ai/imandrakit")
 (package (name imandrakit-log)))

(pin
 (url "git+https://github.com/imandra-ai/imandrakit")
 (package (name imandrakit-thread)))

(lang dune 2.8)
(name py)
(version 0.0.0)
(generate_opam_files true)

(source (github sano-jin/python-in-ocaml))
(license MIT)
(authors "sano<sano@ueda.info.waseda.ac.jp>")
(maintainers "sano<sano@ueda.info.waseda.ac.jp>")

(package
 (name py)
 (synopsis "py")
 (description "A minimal interpreter")
 (depends
  ; (ounit :with-test)
  (menhir (>= 20210419))
  ; (patdiff (>= 0.14.0))
  (dune (> 1.5))
  (odoc (>= 1.5.3))
  ))
(using menhir 2.1)

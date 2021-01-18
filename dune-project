(lang dune 2.7)
(name ordre)
(version 0.0.1)

(generate_opam_files true)

(source (github yutopp/ordre))
(license "BSL-1.0")
(authors "yutopp")
(maintainers "yutopp@gmail.com")

(package
 (name ordre)
 (synopsis "One-line description")
 (description "A longer description")
 (depends
  (ocaml               (>= 4.09.0))
  (dune                (>= 2.7))
  (base                (>= v0.13.0))
  (stdio               (>= v0.13.0))
  (ppx_yojson_conv     (>= v0.14.0))
  (ppx_let             (>= v0.13.0))
  (ppx_deriving        (>= 4.5))
  (alcotest            (and :with-test (>= 1.2.3)))
))

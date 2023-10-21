(lang dune 3.11)

(name pot1)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name pot1)
 (synopsis "Generate random orders of inputs")
 (description
  "A small learning project that generates random orders of given objects.")
 (depends
  ocaml
  dune
  (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

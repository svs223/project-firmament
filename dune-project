(lang dune 3.17)

(name func)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name func)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_deriving))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

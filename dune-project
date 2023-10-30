(lang dune 3.8)
(name queries)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Felipe Sasdelli")

(maintainers "Felipe Sasdelli")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name queries)
 ; (allow_empty)
 (synopsis "DataBase Queries")
 (description "idk")
 (depends base core stdio ocaml dune utop ocaml-lsp-server)
 (tags
  (topics "dbs" your project)))

; See the complete stanza docs at
; https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

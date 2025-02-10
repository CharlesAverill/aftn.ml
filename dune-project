(lang dune 3.9)

(name AFTN)

(generate_opam_files true)

(source
 (github CharlesAverill/AFTN))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license LICENSE)

(documentation https://github.com/CharlesAverill/AFTN)

(package
 (name AFTN)
 (synopsis "")
 (description "Alien: Fate of the Nostromo port")
 (depends ocaml dune alcotest ocamlformat))


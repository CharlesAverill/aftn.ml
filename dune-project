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
 (sites (share game_data))
 (depends ocaml dune alcotest ocamlformat odoc dune-site))

(using directory-targets 0.1)
(using dune_site 0.1)

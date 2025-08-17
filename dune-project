(lang dune 3.9)

(name AFTN)

(generate_opam_files true)

(source
 (github CharlesAverill/AFTN))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license MIT)

(documentation https://charlesaverill.github.io/aftn.ml)

(package
 (name AFTN)
 (description "A terminal-based OCaml clone of the 'ALIEN: Fate of The Nostromo' board game")
 (synopsis "Alien: Fate of the Nostromo port")
 (sites (share game_data))
 (depends ocaml dune alcotest ocamlformat odoc dune-site ocplib-endian))

(using directory-targets 0.1)
(using dune_site 0.1)

let array =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/JSON/array.dhall

let x = ./expr0.dhall

let rec = ./subdir/lib.dhall

in  rec ⫽ { a = x, c = 3, array }

let k8s =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/package.dhall

let test =
      k8s.ConfigMap::{
      , metadata = k8s.ObjectMeta::{ name = Some "test" }
      , data = Some (toMap { foo = ./foo.conf as Text })
      }

in  test

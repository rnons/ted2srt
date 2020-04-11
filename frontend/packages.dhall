let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200404/packages.dhall sha256:f239f2e215d0cbd5c203307701748581938f74c4c78f4aeffa32c11c131ef7b6

let nonbili =
      https://raw.githubusercontent.com/nonbili/package-sets/d56927bf7d0378647d8302d1bfac30698c208ab9/packages.dhall sha256:4ead482f4ed450dac36166109f54299eeabbac5b30f7e95b9d21d994a84fb5cf

let overrides = {=}

let additions = {=}

in  upstream // nonbili // overrides // additions

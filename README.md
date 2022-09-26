# Natural deduction in Scala

  - `sbt root/test` in the main folder runs the tests (and triggers a compile if needed).
  - `sbt rootJS/publishLocal` compiles for JS and puts the packages in the local maven repo.
  - In `scalajs`, `sbt run` runs the JS main method.
  - In `scalajs`, `sbt fastLinkJS` creates the javascript file for the web.

On the console, to see proper unicode fonts, `chcp 65001` and use a unicode
compatible font.

## Building the web part for export

  - Copy `scalajs/target/scala-3.2.0/scalanat-web-fastopt/main.js` to `program.js`.
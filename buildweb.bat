@rem Build the whole project and create the web folder.

@echo Compiling and testing ... 1>&2
@echo === NEW BUILD === >> log
@date /T >> log
@time /T >> log
@echo Compiling ... >> log
call sbt root/test
@if errorlevel 1 goto fail

@echo Publishing locally ... 1>&2
@echo Publishing ... >> log
call sbt rootJS/publishLocal
@if errorlevel 1 goto fail


@echo Building web part ... 1>&2
@echo Building web ... >> log
cd scalajs
call sbt fastLinkJS
@if errorlevel 1 goto fail

cd ..
copy /Y scalajs\target\scala-3.2.0\scalanat-web-fastopt\main.js out\program.js
@if errorlevel 1 goto fail

@echo Build complete. 1>&2
@echo Complete. >> log

goto end

:fail
@echo Build failed 1>&2
@echo Error >> log

:end

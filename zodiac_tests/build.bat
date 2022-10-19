
REM Build script for compiler tests
@ECHO OFF
SetLocal EnableDelayedExpansion

REM Get a list of all the .cpp files.
SET cppFilenames=
FOR /R %%f in (*.cpp) do (
    SET cppFilenames=!cppFilenames! %%f
)

REM echo "Files:" %cppFilenames%

SET assembly=zodiac_tests
SET compilerFlags=-g -Wvarargs -Wall -Werror
SET includeFlags=-Isrc -I..\zodiac_lib\src
SET linkerFlags=../bin/zodiac_lib.lib
SET defines=-D_DEBUG -DZIMPORT

ECHO "Building %assembly%%..."
clang++ %cppFilenames% %compilerFlags% -o ../bin/%assembly%.exe %defines% %includeFlags% %linkerFlags%
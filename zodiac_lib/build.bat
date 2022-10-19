
REM Build script for compiler
@ECHO OFF
SetLocal EnableDelayedExpansion

REM Get a list of all the .cpp files.
SET cppFilenames=
FOR /R %%f in (*.cpp) do (
    SET cppFilenames=!cppFilenames! %%f
)

REM echo "Files:" %cppFilenames%

SET assembly=zodiac_lib
SET compilerFlags=-g -static -Wvarargs -Wall -Werror
SET includeFlags=-Isrc
SET linkerFlags=
SET defines=-D_DEBUG -DZEXPORT -D_CRT_SECURE_NO_WARNINGS

ECHO "Building %assembly%%..."
clang++ -c %cppFilenames% %compilerFlags% -o ../bin/%assembly%.o %defines% %includeFlags% %linkerFlags%
llvm-lib ../bin/%assembly%.o
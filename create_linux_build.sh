#! /usr/bin/env bash

check_cmake_result () {
    cmake_res=$?
    if [[ $cmake_res -ne 0 ]]; then
        echo
        echo Error encountered while generating cmake project...
        echo
        exit $cmake_res
    fi
}

call_cmake () {
    cmake .. -G Ninja ../.. -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE="$1" -DCMAKE_PREFIX_PATH="${LLVM_PREFIX}" $2
    check_cmake_result
    echo
}

git submodule init
git submodule update

if  [ ! -d "lib/dyncall-1.2" ]; then
    echo Unpacking dyncall
    pushd lib
    tar xvf dyncall-1.2.tar.gz
    echo Building dyncall
    pushd dyncall-1.2
    ./configure
    make -j $(nproc)
    popd
    popd
fi

if  [ ! -d "lib/tracy-0.7.8-1" ]; then
    echo Unpacking tracy
    pushd lib
    tar xvf tracy-0.7.8-1.tar.gz
    ln -s tracy-0.7.8-1 tracy
    popd
fi

# if  [ ! -f "lib/tracy-0.7.8-1/profiler/build/unix/Tracy-Profiler" ]; then
#     echo Building tracy profiler
#     pushd lib/tracy-0.7.8-1/profiler/build/unix
#     make -j $(nproc)
#     popd
# fi

rm -rf build_gcc
rm -rf build_clang

mkdir -p build_gcc/debug
mkdir -p build_gcc/release

pushd build_gcc
pushd debug

call_cmake Debug

popd

mkdir release_debug
pushd release_debug
call_cmake RelWithDebInfo
popd

pushd release
call_cmake Release
popd

mkdir release_debug
pushd release_debug
call_cmake RelWithDebInfo
popd

popd


mkdir -p build_clang/debug
mkdir -p build_clang/release
mkdir -p build_clang/tracy

pushd build_clang
pushd debug

CC=clang CXX=clang++ call_cmake Debug

popd

pushd release
CC=clang CXX=clang++ call_cmake Release
popd

mkdir release_debug
pushd release_debug
CC=clang CXX=clang++ call_cmake RelWithDebInfo
popd

mkdir release_debug
pushd release_debug
CC=clang CXX=clang++ call_cmake RelWithDebInfo
popd

pushd tracy
CC=clang CXX=clang++ call_cmake Release "-DTRACY_ENABLE=1"
popd

popd


ln -s build_clang/debug build
ln -s build_clang/debug/compile_commands.json compile_commands.json

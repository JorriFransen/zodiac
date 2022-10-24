#pragma once

#if defined(__clang__) || defined(__gcc__)
#define STATIC_ASSERT __cpp_static_assert
#else
#define STATIC_ASSERT static_assert
#endif

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)

#define ZPLATFORM_WINDOWS 1
#ifndef _WIN64
#error "64-bit is required on Windows!"
#endif // _WIN64

#elif defined(__linux__) || defined(__gnu_linux__) 

//STATIC_ASSERT(false, "Unsupported platform (linux).");
#define ZPLATFORM_LINUX 1

#elif defined(__unix__)

#define ZPLATFORM_UNIX 1
STATIC_ASSERT(false, "Unsupported platform (unix).");

#elif __APPLE__

#define ZPLATFORM_APPLE 1
STATIC_ASSERT(false, "Unsupported platform (Apple).");

#endif // defined(WIN32) || defined(_WIN32) || defined(__WIN32__)

#ifdef ZEXPORT

// Exports
#ifdef _MSC_VER
#define ZAPI __declspec(dllexport)
#else
#define ZAPI __attribute__((visibility("default")))
#endif
#else
// Imports
#ifdef _MSC_VER
#define ZAPI __declspec(dllimport)
#else
#define ZAPI
#endif

#endif // ZEXPORT


#include <asserts.h>
#include <logger.h>
#include <memory/zmemory.h>

using namespace Zodiac;

int main() {

    if (!Zodiac::logging_system_initialize()) return 1;
    if (!Zodiac::memory_system_initialize()) return 1;


    debug_assert_msg(false, "This assertion failure is intentional");

    return 0;
}

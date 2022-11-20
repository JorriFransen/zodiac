
#include <memory/zmemory.h>
#include <logger.h>

using namespace Zodiac;

int main() {
    Zodiac::memory_system_initialize();

    log_message(Log_Level::FATAL, "FATAL message");
    log_message(Log_Level::ERROR, "ERROR message");
    log_message(Log_Level::WARN, "WARN message");
    log_message(Log_Level::INFO, "INFO message");
    log_message(Log_Level::DEBUG, "DEBUG message");
    log_message(Log_Level::TRACE, "TRACE message");

    return 0;
}

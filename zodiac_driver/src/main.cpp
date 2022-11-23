
#include <memory/zmemory.h>
#include <logger.h>

using namespace Zodiac;

int main() {
    if (!Zodiac::logging_system_initialize()) return 1;
    log_message(Log_Level::FATAL, "FATAL message");
    log_message(Log_Level::ERROR, "ERROR message");
    log_message(Log_Level::WARN, "WARN message");
    log_message(Log_Level::INFO, "INFO message");
    log_message(Log_Level::DEBUG, "DEBUG message");
    log_message(Log_Level::TRACE, "TRACE message");

    return 0;
}

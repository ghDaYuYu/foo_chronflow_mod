#include "utils.h"

#include <cstdio>

void errorPopup(const char* message) {
  // This should be:
  // popup_message::g_show(... , popup_message::icon_error);
  // But we sometimes need this to be modal (as it will be followed by crash)
  MessageBoxW(nullptr,
              uT(PFC_string_formatter()
                 << "foo_chronflow: " << message
                 << "\r\n\r\nIf this happens more than once, please report this error "
                    "in the foo_chronflow "
                    "thread on Hydrogenaudo or via mail to foocomp@chronial.de"),
              L"Error in foo_chronflow", MB_OK | MB_ICONERROR);
}

void errorPopupWin32(const char* message) {
  errorPopup(PFC_string_formatter() << message << "\r\nWin32 Error Message: "
                                    << format_win32_error(GetLastError()));
}

std::string linux_lineendings(std::string s) {
  boost::replace_all(s, "\r\n", "\n");
  return std::move(s);
}

std::string windows_lineendings(std::string s) {
  boost::replace_all(s, "\r\n", "\n");
  boost::replace_all(s, "\n", "\r\n");
  return std::move(s);
}

// Returns the time in seconds with maximum resolution
double Helpers::getHighresTimer() {
  static double timerResolution = 0;
  static __int64 timerOffset = 0;
  static bool timerSupported;
  if (timerResolution == 0) {
    LARGE_INTEGER res;
    if (!QueryPerformanceFrequency(&res) || res.QuadPart == 0) {
      timerSupported = false;
      timerResolution = 1;
    } else {
      timerSupported = true;
      timerResolution = 1.0 / static_cast<double>(res.QuadPart);
      LARGE_INTEGER count;
      QueryPerformanceCounter(&count);
      timerOffset = count.QuadPart;
    }
  }
  if (timerSupported) {
    LARGE_INTEGER count;
    QueryPerformanceCounter(&count);
    return timerResolution * (count.QuadPart - timerOffset);
  } else {
    return timeGetTime() / 1000.0;
  }
}
bool Helpers::isPerformanceCounterSupported() {
  LARGE_INTEGER res;
  return QueryPerformanceFrequency(&res) == TRUE && res.QuadPart > 0;
}

// adjusts a given path for certain discrepancies between how foobar2000
// and GDI+ handle paths, and other oddities
//
// Currently fixes:
//   - User might use a forward-slash instead of a
//     backslash for the directory separator
//   - GDI+ ignores trailing periods '.' in directory names
//   - GDI+ and FindFirstFile ignore double-backslashes
//   - makes relative paths absolute to core_api::get_profile_path()
// Copied from  foo_uie_albumart
void Helpers::fixPath(pfc::string_base& path) {
  if (path.get_length() == 0)
    return;

  pfc::string8 temp;
  titleformat_compiler::remove_forbidden_chars_string(temp, path, ~0u, "*?<>|\"");

  // fix directory separators
  temp.replace_char('/', '\\');

  bool is_unc = (pfc::strcmp_partial(temp, "\\\\") == 0);
  if ((temp[1] != ':') && (!is_unc)) {
    pfc::string8 profilePath;
    filesystem::g_get_display_path(core_api::get_profile_path(), profilePath);
    profilePath.add_byte('\\');

    temp.insert_chars(0, profilePath);
  }

  // fix double-backslashes and trailing periods in directory names
  t_size temp_len = temp.get_length();
  path.reset();
  path.add_byte(temp[0]);
  for (t_size n = 1; n < temp_len - 1; n++) {
    if (temp[n] == '\\') {
      if (temp[n + 1] == '\\')
        continue;
    } else if (temp[n] == '.') {
      if ((temp[n - 1] != '.' && temp[n - 1] != '\\') && temp[n + 1] == '\\')
        continue;
    }
    path.add_byte(temp[n]);
  }
  if (temp_len > 1)
    path.add_byte(temp[temp_len - 1]);
}

#ifdef _DEBUG
namespace console {
static HANDLE screenBuffer = nullptr;

out::~out() {
  println(this->str().c_str());
}

void create() {
  AllocConsole();
  screenBuffer = GetStdHandle(STD_OUTPUT_HANDLE);
}

void print(const wchar_t* str) {
  WriteConsole(screenBuffer, str, wcslen(str), nullptr, nullptr);
}

void println(const wchar_t* str) {
  print((std::wstring(str) + L"\n").c_str());
}

void printf(const wchar_t* format, ...) {
  va_list args;
  va_start(args, format);  // NOLINT
  wchar_t out[1024];
  int len = vswprintf_s(out, 1024, format, args);  // NOLINT
  WriteConsole(screenBuffer, out, len, nullptr, nullptr);  // NOLINT
}
}  // namespace console
#endif

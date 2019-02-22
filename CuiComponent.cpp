#pragma warning(push, 1)
#include "../columns_ui-sdk/ui_extension.h"
#pragma warning(pop)

#include "ContainerWindow.h"

class cui_chronflow : public ui_extension::window {
  ui_extension::window_host_ptr m_host;
  std::optional<ContainerWindow> window;

 public:
  cui_chronflow() = default;

  void get_category(pfc::string_base& out) const final { out = "Panels"; }

  const GUID& get_extension_guid() const final {
    // {DA317C70-654F-4A75-A4F2-10F4873773FC}
    static const GUID guid_foo_chronflow = {
        0xda317c70, 0x654f, 0x4a75, {0xa4, 0xf2, 0x10, 0xf4, 0x87, 0x37, 0x73, 0xfc}};
    return guid_foo_chronflow;
  }

  void get_name(pfc::string_base& out) const final { out = "Chronflow"; }

  unsigned get_type() const final { return ui_extension::type_panel; }

  bool is_available(const ui_extension::window_host_ptr& p_host) const final {
    return !p_host.is_valid() || !m_host.is_valid() ||
           p_host->get_host_guid() != m_host->get_host_guid();
  }

  HWND create_or_transfer_window(HWND wnd_parent,
                                 const ui_extension::window_host_ptr& p_host,
                                 const ui_helpers::window_position_t& p_position) final {
    if (!window) {
      m_host = p_host;
      window.emplace(wnd_parent);
      ShowWindow(window->getHWND(), SW_HIDE);
      SetWindowPos(window->getHWND(), nullptr, p_position.x, p_position.y, p_position.cx,
                   p_position.cy, SWP_NOZORDER);
    } else {
      ShowWindow(window->getHWND(), SW_HIDE);
      SetParent(window->getHWND(), wnd_parent);
      SetWindowPos(window->getHWND(), nullptr, p_position.x, p_position.y, p_position.cx,
                   p_position.cy, SWP_NOZORDER);
      m_host->relinquish_ownership(window->getHWND());
      m_host = p_host;
    }

    return window->getHWND();
  }

  void destroy_window() final {
    window.reset();
    m_host.release();
  }

  HWND get_wnd() const final { return window->getHWND(); }
  const bool get_is_single_instance() const final { return true; }
};

static service_factory_single_t<cui_chronflow> cui_chronflow_instance;

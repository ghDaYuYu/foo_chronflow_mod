
#include <utility>

#include "ContainerWindow.h"
#include "style_manager.h"
#include "ConfigData.h"

namespace {

using engine::ContainerWindow;
using render::StyleManager;

class DuiStyleManager : public StyleManager {
 public:
  DuiStyleManager(ui_element_instance_callback::ptr instance_callback)
      : instance_callback(instance_callback) {
    updateCache();
  }
  virtual COLORREF defaultTitleColor() final {
    return instance_callback->query_std_color(ui_color_text);
  }
  virtual LOGFONT defaultTitleFont() final {
    LOGFONT font = {};
    HFONT hfont = instance_callback->query_font_ex(ui_font_default);
    check(GetObject(hfont, sizeof(font), &font));
    return font;
  }
  virtual COLORREF defaultBgColor() final {
    return instance_callback->query_std_color(ui_color_background);
  }

 private:
  ui_element_instance_callback::ptr instance_callback;
};

// {6D514EFE-4413-4978-94E0-3EB8D8981271}
static const GUID guid_dui_foo_coverflow = {
     0x6d514efe, 0x4413, 0x4978, {0x94, 0xe0, 0x3e, 0xb8, 0xd8, 0x98, 0x12, 0x71}}; //modded
static const GUID element_subclass = ui_element_subclass_media_library_viewers;

class dui_coverflow : public ui_element_instance {

  const ui_element_config::ptr config;
  uint32_t coverArt = 0;
  DuiStyleManager style_manager;
  ContainerWindow window;

 public:

  dui_coverflow(HWND parent, ui_element_config::ptr config,
                ui_element_instance_callback_ptr p_callback)
      : config(config.get_ptr()), style_manager(p_callback),
        window(parent, style_manager, p_callback) {

    // host to dlg
    set_configuration(config);
  }

  ~dui_coverflow(){
    //..
  }

  HWND get_wnd() override {
    return window.getHWND();
  };

  static void g_get_name(pfc::string_base& out) { out = component_NAME; }
  static const char* g_get_description() {
    return "Displays a 3D rendering of the Album Art in your Media Library";
  }

  virtual void notify(const GUID& p_what, t_size p_param1, const void* p_param2, t_size p_param2size) override {
    if (p_what == ui_element_notify_colors_changed ||
        p_what == ui_element_notify_font_changed) {
      style_manager.onChange();
    }
  }

  GUID get_guid() override { return guid_dui_foo_coverflow; }
  GUID get_subclass() override { return element_subclass; }

  // TODO: broken callback (temp fix calling from constructor instead)
  // host to dlg
  void set_configuration(ui_element_config::ptr data) override {
   ::ui_element_config_parser in(data);
   window.set_uicfg(&in, data->get_data_size() , fb2k::noAbort);
  }
  // dlg to host
  ui_element_config::ptr get_configuration() override final {
    ui_element_config_builder out;
    window.get_uicfg(&out, fb2k::noAbort);
    return out.finish(get_guid());
  }
};

class UiElement : public ui_element {
 public:
  GUID get_guid() final { return guid_dui_foo_coverflow; }
  GUID get_subclass() final { return element_subclass; }
  void get_name(pfc::string_base& out) final { dui_coverflow::g_get_name(out); }
  ui_element_instance::ptr instantiate(HWND parent, ui_element_config::ptr cfg,
                                       ui_element_instance_callback::ptr callback) final {
    PFC_ASSERT(cfg->get_guid() == get_guid());
    service_nnptr_t<dui_coverflow> item =
        new service_impl_t<dui_coverflow>(parent, cfg, callback);
    return item;
  }
  ui_element_config::ptr get_default_configuration() final {
    return ui_element_config::g_create_empty(guid_dui_foo_coverflow);
  }
  ui_element_children_enumerator_ptr enumerate_children(
      ui_element_config::ptr /*cfg*/) final {
    return nullptr;
  }
  bool get_description(pfc::string_base& out) final {
    out = dui_coverflow::g_get_description();
    return true;
  }
};

static service_factory_single_t<UiElement> uiElement;

} // namespace

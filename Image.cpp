#include "Image.h"
#include "webp/include/decode.h"
#include "libPPUI/gdiplus-helpers-webp.h"

#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_RESIZE_IMPLEMENTATION

#include "lib/stb_image.h"
#include "lib/stb_image_resize2.h"
#include "ConfigData.h"

using coverflow::configData;

namespace {
/// adjusts a given path for certain discrepancies between how foobar2000
/// and GDI+ handle paths, and other oddities
///
/// Currently fixes:
///   - User might use a forward-slash instead of a
///     backslash for the directory separator
///   - GDI+ ignores trailing periods '.' in directory names
///   - GDI+ and FindFirstFile ignore double-backslashes
///   - makes relative paths absolute to core_api::get_profile_path()
/// Copied from foo_uie_albumart
void fixPath(pfc::string_base& path) {
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
}  // namespace

Image::Image(malloc_ptr data, int width, int height, bool alpha, bool webp)
    : width(width), height(height), alpha(alpha), webp(webp), data(std::move(data)) {}

Image Image::fromFile(const char* filename) {
  auto wideName = pfc::stringcvt::string_wide_from_utf8(filename);
  int width;
  int height;
  int channels_in_file;

  gsl::owner<FILE*> f;
  if (0 != _wfopen_s(&f, wideName, L"rb"))
    throw std::runtime_error{"Failed to open image file"};

  int tcomp = 3;

  if (configData->CoverArtEnablePngAlpha) {
    //increase tcomp to 4 if this is a png8 with alpha channel
    stbi__context s;
    int testw, testh;
    const long pos = ftell(f);
    stbi__start_file(&s, f);
    if (stbi__png_info(&s, &testw, &testh, &tcomp));
    else
      tcomp = 3;
    fseek(f, pos, SEEK_SET);
  }

  malloc_ptr data{
      static_cast<void*>(stbi_load_from_file(f, &width, &height, &channels_in_file, tcomp))};
  fclose(f);
  if (data == nullptr) {
    throw std::runtime_error{"Failed to load image file"};
  }
  return Image{std::move(data), width, height, tcomp>3, false};
}

bool findPngAlphaFromBuffer(const stbi_uc* buffer, int len) {

  bool ispng = false;
  stbi__context s;
  stbi__start_mem(&s, buffer, len);
  stbi__rewind(&s);
  
  stbi__png p;
  p.s = &s;

  ispng = stbi__parse_png_file(&p, STBI__SCAN_header, 4);
  stbi__rewind(p.s);
  int comp = p.s->img_n;
  return (comp == 4 && ispng);
}

// extended GdiplusImageFromMem2 (libPPUI\gdiplus-helpers-webp.h) including WebP resize
// please get in contact if it should be acknowledged in the component license terms  
static std::unique_ptr<Gdiplus::Image> GdiplusImageFromMem3(const void* ptr, size_t bytes) {
  GdiplusErrorHandler EH;
  using namespace Gdiplus;
  if (IsImageWebP(ptr, bytes)) {
    WebPBitstreamFeatures bs;
    if (WebPGetFeatures((const uint8_t*)ptr, bytes, &bs) != VP8_STATUS_OK) {
      throw std::runtime_error("WebP decoding failure");
    }

    const int maxWidth = std::min(configData->MaxTextureSize, 1024);
    auto scaledWidth = ReadyImageDims(bs.width, bs.height, maxWidth);

    const Gdiplus::PixelFormat pf = PixelFormat32bppARGB;

    const int pfBytes = 4;  // Gdiplus RGB is 4 bytes
    int w = 0, h = 0;

    /* GdiplusImageFromMem3 */
    
    WebPDecoderConfig config;
    WebPInitDecoderConfig(&config);
    config.options.no_fancy_upsampling = 1;
    config.output.colorspace = MODE_BGRA;
    config.options.use_scaling = scaledWidth != SIZE_MAX;
    config.options.scaled_width = static_cast<int>(scaledWidth);
    WebPDecode((const uint8_t*)ptr, bytes, &config);
    w = config.output.width;
    h = config.output.height;

    uint8_t* decodedData = config.output.private_memory;

    pfc::onLeaving scope([&config] { WebPFreeDecBuffer(&config.output); });

    if (decodedData == nullptr || w <= 0 || h <= 0)
      throw std::runtime_error("WebP decoding failure");

    std::unique_ptr<Bitmap> ret(new Gdiplus::Bitmap(w, h, pf));
    EH << ret->GetLastStatus();
    Rect rc(0, 0, w, h);
    Gdiplus::BitmapData bitmapData;
    EH << ret->LockBits(&rc, 0, pf, &bitmapData);
    uint8_t* target = (uint8_t*)bitmapData.Scan0;
    const uint8_t* source = decodedData;
    for (int y = 0; y < h; ++y) {
      memcpy(target, source, w * pfBytes);
      target += pfBytes * w;
      source += pfBytes * w;
    }

    EH << ret->UnlockBits(&bitmapData);
    return ret;
  }
  return GdiplusImageFromMem(ptr, bytes);
}

Image Image::fromFileBuffer(const void* buffer, size_t len) {
  int width;
  int height;
  int channels_in_file;

  int req_comp = 3;
  bool bWepP = IsImageWebP(buffer, len);
  if (bWepP) {
    //BGRA
    auto gdi_data = GdiplusImageFromMem3(buffer, len);

    Image img = Image::fromGdiBitmapWebp(*(Gdiplus::Bitmap*)gdi_data.get());
    img.webp  = true;
    img.alpha = true;
    WebPBitstreamFeatures bs;
    if (WebPGetFeatures((const uint8_t*) buffer, len, &bs) !=
        VP8_STATUS_OK) {
      throw std::runtime_error("WebP decoding failure");
    }
    
    img.alpha = bs.has_alpha;

    Gdiplus::Bitmap* tmpBitmap = dynamic_cast<Gdiplus::Bitmap*>(gdi_data.get());

    Image imgf = fromGdiBitmap(*tmpBitmap);
    malloc_ptr data = std::move(imgf.data);
    return img;

  } else {

    if (configData->CoverArtEnablePngAlpha) {
      if (findPngAlphaFromBuffer((stbi_uc*)buffer, static_cast<int>(len)))
        req_comp = 4;
    }
    malloc_ptr data{static_cast<void*>(stbi_load_from_memory(static_cast<const stbi_uc*>(buffer), static_cast<int>(len),
        &width, &height, &channels_in_file, req_comp))};


    if (data == nullptr) {
      throw std::runtime_error{"Failed to load image buffer"};
    }
    return Image{std::move(data), width, height, req_comp == 4, false };
  }
}

Image Image::fromResource(LPCTSTR pName, LPCTSTR pType, HMODULE hInst) {
  HRSRC hResource = FindResource(hInst, pName, pType);
  if (hResource == nullptr)
    throw std::runtime_error{"Failed to find image resource"};

  DWORD resourceSize = SizeofResource(hInst, hResource);
  if (!resourceSize)
    throw std::runtime_error{"Failed to size image resource"};

  HGLOBAL resourceHandle = LoadResource(hInst, hResource);
  if (resourceHandle == nullptr)
    throw std::runtime_error{"Failed to load image resource"};

  const void* resourceData = LockResource(resourceHandle);
  if (resourceData == nullptr)
    throw std::runtime_error{"Failed to lock image resource"};

  return Image::fromFileBuffer(resourceData, resourceSize);
}

Image Image::fromResource(UINT id, LPCTSTR pType, HMODULE hInst) {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-cstyle-cast)
  return Image::fromResource(MAKEINTRESOURCE(id), pType, hInst);
}

Image Image::fromGdiBitmap(Gdiplus::Bitmap& bitmap) {
  Gdiplus::BitmapData bitmapData{};
  Gdiplus::Rect rc(0, 0, bitmap.GetWidth(), bitmap.GetHeight());
  if (Gdiplus::Ok != bitmap.LockBits(&rc, Gdiplus::ImageLockModeRead, PixelFormat24bppRGB,
                                     &bitmapData)) {
    throw std::runtime_error{"Failed to lock bitmap data"};
  }
  auto _ = gsl::finally([&] { bitmap.UnlockBits(&bitmapData); });
  size_t bufferSize = bitmapData.Width * bitmapData.Height * 3;
  // NOLINTNEXTLINE(cppcoreguidelines-no-malloc)
  malloc_ptr outBuffer{malloc(bufferSize)};
  if (outBuffer == nullptr) {
    throw std::bad_alloc{};
  }
  memcpy(outBuffer.get(), bitmapData.Scan0, bufferSize);

  // convert bgr to rgb
  auto* p = static_cast<uint8_t*>(outBuffer.get());
  for (t_size i = 0; i < bitmapData.Width * bitmapData.Height; ++i) {
    stbi_uc t = p[0];
    p[0] = p[2];
    p[2] = t;
    p += 3;
  }

  return Image(std::move(outBuffer), bitmapData.Width, bitmapData.Height, false, false);
}

Image Image::fromGdiBitmapWebp(Gdiplus::Bitmap& bitmap) {
  Gdiplus::BitmapData bitmapData{};
  Gdiplus::Rect rc(0, 0, bitmap.GetWidth(), bitmap.GetHeight());
  if (Gdiplus::Ok != bitmap.LockBits(&rc, Gdiplus::ImageLockModeRead,
                                     PixelFormat32bppARGB,
                                     &bitmapData)) {
    throw std::runtime_error{"Failed to lock bitmap data"};
  }
  auto _ = gsl::finally([&] { bitmap.UnlockBits(&bitmapData); });
  size_t bufferSize = bitmapData.Width * bitmapData.Height * 4;

  malloc_ptr outBuffer{malloc(bufferSize)};
  if (outBuffer == nullptr) {
    throw std::bad_alloc{};
  }
  memcpy(outBuffer.get(), bitmapData.Scan0, bufferSize);

  return Image(std::move(outBuffer), bitmapData.Width, bitmapData.Height, false, false);
}


Image Image::resize(int width, int height) const {

  const size_t channels = alpha ? 4 : 3;

  if (webp) {
    throw std::runtime_error{"Failed to resize image"};
  }

  const size_t new_size = width * height * channels;

  // NOLINTNEXTLINE(cppcoreguidelines-no-malloc)
  malloc_ptr new_buffer{static_cast<stbi_uc*>(malloc(new_size))};
  if (new_buffer == nullptr) {
    throw std::bad_alloc{};
  }

  auto result =
      stbir_resize(static_cast<stbi_uc*>(data.get()), this->width,
                              this->height, 0, static_cast<stbi_uc*>(new_buffer.get()), width, height, 0,
                              static_cast<stbir_pixel_layout>(channels),
                              STBIR_TYPE_UINT8_SRGB_ALPHA, STBIR_EDGE_ZERO, STBIR_FILTER_DEFAULT);

  if (!result) {
    throw std::runtime_error{"Failed to resize image"};
  }
  return Image{std::move(new_buffer), width, height, this->alpha, this->webp};
}

std::optional<UploadReadyImage> loadAlbumArt(const metadb_handle_ptr& track,
                                             abort_callback& abort) {
  IF_DEBUG(double preLoad = time());
  static_api_ptr_t<album_art_manager_v2> aam;

  auto extractor = aam->open(
    pfc::list_single_ref_t(track),
    pfc::list_single_ref_t(configData->GetGuidArt(configData->CustomCoverFrontArt)),
    abort);

  try {
    auto art = extractor->query(
      configData->GetGuidArt(configData->CustomCoverFrontArt),
      abort);

    Image image = Image::fromFileBuffer(art->get_ptr(), art->get_size());

    IF_DEBUG(auto x = gsl::finally([&] {
               console::out() << "ART [done] " << std::setw(6)
                              << (1000 * (time() - preLoad)) << " ms";
             }));
    return UploadReadyImage(std::move(image));
  } catch (const exception_album_art_not_found&) {
    IF_DEBUG(console::out() << "ART [miss] " << std::setw(6)
                            << (1000 * (time() - preLoad)) << " ms");
    return std::nullopt;
  } catch (const exception_aborted&) {
    throw;
  } catch (...) {
    IF_DEBUG(console::out() << "ART [fail] " << std::setw(6)
                            << (1000 * (time() - preLoad)) << " ms");
    return std::nullopt;
  }
}
bool HasImageExtension(pfc::string8 extension) {
  const std::vector<pfc::string8> vext{".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tiff"};
  return std::find(vext.begin(), vext.end(), extension) != vext.end();
}
std::optional<UploadReadyImage> loadAlbumArtv2(const metadb_handle_ptr& track,
                                               abort_callback& abort) {

  IF_DEBUG(double preLoad = time());
  static_api_ptr_t<album_art_manager_v2> aam;
  //info: race condition

  auto extractor = aam->open(pfc::list_single_ref_t(track),
      pfc::list_single_ref_t(configData->GetGuidArt(configData->CustomCoverFrontArt)),
                             abort);

  try {
    auto art =
        extractor->query(configData->GetGuidArt(configData->CustomCoverFrontArt), abort);
    Image image = Image::fromFileBuffer(art->get_ptr(), art->get_size());
    IF_DEBUG(auto x = gsl::finally([&] {
                console::out() << "ART [done] " << std::setw(6)
                              << (1000 * (time() - preLoad)) << " ms";
              }));
    return UploadReadyImage(std::move(image));
  } catch (const exception_album_art_not_found&) {
    IF_DEBUG(
        console::out() << "ART [miss] " << std::setw(6) << (1000 * (time() - preLoad)) <<
        " ms");
    return std::nullopt;
  } catch (const exception_aborted&) {
    throw;
  } catch (...) {
    IF_DEBUG(
        console::out() << "ART [fail] " << std::setw(6) << (1000 * (time() - preLoad)) <<
        " ms");
    return std::nullopt;
  }
}

UploadReadyImage loadSpecialArt(WORD resource, pfc::string8 userImage, bool hasAlpha) {
  userImage.skip_trailing_char(' ');
  if (userImage.get_length() > 0) {
    fixPath(userImage);
    try {
      return UploadReadyImage(Image::fromFile(userImage));
    } catch (...) {
    };
  }
  // either no userImage or loading failed
  return UploadReadyImage(Image::fromResource(resource, !hasAlpha? L"JPG" : L"PNG", core_api::get_my_instance())
  );
}

UploadReadyImage::UploadReadyImage(Image&& src)
    : image(std::move(src)), originalAspect(static_cast<double>(src.width) / src.height) {

  const int maxSize = std::min(configData->MaxTextureSize, 1024);
  size_t readyWidth = ReadyImageDims(src.width, src.height, maxSize);

  if (!image.webp && readyWidth != SIZE_MAX) {
    // resize
    image = image.resize(readyWidth, readyWidth);
  }
}

UploadReadyImage& UploadReadyImage::operator=(UploadReadyImage&& other) {
  originalAspect = other.originalAspect;
  image = std::move(other.image);
  return *this;
}

GLImage UploadReadyImage::upload() const {
  TRACK_CALL_TEXT("UploadReadyImage::upload");
  IF_DEBUG(double preLoad = time());
  // TODO: handle opengl errors?
  GLTexture texture{};
  texture.bind();
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 16);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  GLint glInternalFormat;
  if (configData->TextureCompression) {
    glInternalFormat = image.alpha? GL_COMPRESSED_RGBA : GL_COMPRESSED_RGB;
  } else {
    glInternalFormat = image.alpha || image.webp ? GL_RGBA : GL_RGB;
  }

  glTexImage2D(GL_TEXTURE_2D, 0, glInternalFormat, image.width, image.height, 0,
                    image.alpha ? image.webp ? GL_BGRA : GL_RGBA
                   : image.webp ? GL_BGRA : GL_RGB,
                    GL_UNSIGNED_BYTE, image.data.get());
  IF_DEBUG(console::out() << "GLUpload " << (time() - preLoad) * 1000 << " ms");
  return GLImage(std::move(texture), static_cast<float>(originalAspect), true);
}

GLTexture::GLTexture() {
  glGenTextures(1, &glTexture);
}

GLTexture::GLTexture(GLTexture&& other) noexcept : glTexture(other.glTexture) {
  other.glTexture = 0;
}
GLTexture::GLTexture(GLTexture&& other, bool hasalpha) noexcept : glTexture(other.glTexture) {
  other.glTexture = 0;
}

GLTexture& GLTexture::operator=(GLTexture&& other) noexcept {
  reset();
  glTexture = other.glTexture;
  other.glTexture = 0;
  return *this;
}

GLTexture::~GLTexture() noexcept {
  reset();
}

void GLTexture::reset() noexcept {
  if (glTexture != 0) {
    IF_DEBUG(console::out() << "DELETE");
    glDeleteTextures(1, &glTexture);
    glTexture = 0;
  }
}

void GLTexture::bind() const {
  PFC_ASSERT(glTexture != 0);
  glBindTexture(GL_TEXTURE_2D, glTexture);
}

GLImage loadSpinner() {
  LPCWSTR pName = MAKEINTRESOURCE(IDB_SPINNER);
  auto hInst = core_api::get_my_instance();

  HRSRC hResource = check(FindResource(hInst, pName, L"PNG"));
  DWORD resourceSize = check(SizeofResource(hInst, hResource));
  HGLOBAL resourceHandle = check(LoadResource(hInst, hResource));
  const void* resourceData = check(LockResource(resourceHandle));

  int width;
  int height;
  int channels_in_file;
  stbi_uc* data =
      stbi_load_from_memory(static_cast<const stbi_uc*>(resourceData), resourceSize,
                            &width, &height, &channels_in_file, 4);
  auto free_data = gsl::finally([&] { stbi_image_free(data); });
  if (data == nullptr) {
    throw std::runtime_error{"Failed to load image buffer"};
  }
  GLTexture texture{};
  texture.bind();
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 16);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexImage2D(
    GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
  return GLImage(std::move(texture), 1.0, true);
}

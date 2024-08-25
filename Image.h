#pragma once
#include "utils.h"

class Image {
 public:
  using malloc_ptr = unique_ptr_del<void, &free>;
  int width;
  int height;
  bool alpha = false;
  bool webp = false;
  malloc_ptr data;

  Image(malloc_ptr data, int width, int height, bool alpha, bool webp);

  static Image fromFile(const char* filename);
  static Image fromFileBuffer(const void* buffer, size_t len);
  static Image fromResource(LPCTSTR pName, LPCTSTR pType, HMODULE hInst);
  static Image fromResource(UINT id, LPCTSTR pType, HMODULE hInst);
  static Image fromGdiBitmap(Gdiplus::Bitmap& bitmap);
  static Image fromGdiBitmapWebp(Gdiplus::Bitmap& bitmap);

  Image resize(int width, int height) const;
};

class GLTexture {
 public:
  GLTexture();
  GLTexture(const GLTexture&) = delete;
  GLTexture& operator=(const GLTexture&) = delete;
  GLTexture(GLTexture&&) noexcept;
  GLTexture(GLTexture&&, bool hasalpha) noexcept;
  GLTexture& operator=(GLTexture&&) noexcept;
  ~GLTexture() noexcept;

  void bind() const;

 private:
  void reset() noexcept;
  GLuint glTexture = 0;
};

class GLImage {
 public:
  GLImage(GLTexture glTexture, float originalAspect, bool hasAlpha)
      : glTexture(std::move(glTexture), hasAlpha), originalAspect(originalAspect) {
  };

  void bind() const { glTexture.bind(); };
  float getAspect() const { return originalAspect; };

 private:
  GLTexture glTexture;
  float originalAspect;
};

class UploadReadyImage {
 public:
  explicit UploadReadyImage(Image&& src);
  UploadReadyImage(const UploadReadyImage&) = delete;
  UploadReadyImage& operator=(const UploadReadyImage&) = delete;
  UploadReadyImage(UploadReadyImage&& other)
      : image(std::move(other.image)), originalAspect(other.originalAspect){};
  UploadReadyImage& operator=(UploadReadyImage&&);
  ~UploadReadyImage() = default;

  GLImage upload() const;

 private:
  Image image;
  double originalAspect;
};

std::optional<UploadReadyImage> loadAlbumArt(const metadb_handle_ptr& track,
                                             abort_callback& abort);
std::optional<UploadReadyImage> loadAlbumArtv2(const metadb_handle_ptr & track,
                                             abort_callback & abort);
UploadReadyImage loadSpecialArt(WORD resource, pfc::string8 userImage, bool hasAlpha);

GLImage loadSpinner();

inline size_t ReadyImageDims(int originalWidth, int originalHeight, const int maxSize) {

  double originalAspect(double(originalWidth) / originalHeight);

  int width = originalWidth;
  int height = originalHeight;

  if ((width > maxSize) || (height > maxSize)) {
    if (width > height) {
      height = int(maxSize / originalAspect);
      width = maxSize;
    } else {
      width = int(originalAspect * maxSize);
      height = maxSize;
    }
  }

  // turn texture sizes into powers of two
  int p2w = 1;
  while (p2w <= width) p2w = p2w << 1;
  width = p2w <= width ? p2w : p2w / 2;
  int p2h = 1;
  while (p2h <= height) p2h = p2h << 1;
  height = p2h <= height ? p2h : p2h / 2;

  if (width != originalWidth || height != originalHeight) {
    return std::max(width, height);
  } else {
    return SIZE_MAX;
  }
}


# PNG decoder

This is a toy project for decoding PNG files. It currently supports only a small subset of PNG images, namely ones that fit these conditions:

- RGB/RGBA/Mono (Grayscale) color scheme
- 8-bit depth
- pixels filtered via:
  - None
  - Sub

The information is printed to stdout on application start. An example of such info:

```text
Image info: IHDR {
  Width: 1,
  Height: 1,
  Bit depth: 1,
  Color type: 0 (grayscale),
  Compression method: 0 (deflate/inflate),
  Filter method: 0 (adaptive filtering with five basic filter types),
  Interlace method: 0 (no interlace)
}
```

This image type is not supported because it has bit depth: 1. Thus, we get the following error:
```text
Bit depth not supported: 1
```

For more 

use compress::zlib;
use core::str;
use std::fmt;
use std::fs;
use std::io;
use std::io::Read;
use std::isize;
use std::path;

const PNG_SIGNATURE_LENGTH: usize = 8;
const PNG_SIGNATURE: [u8; PNG_SIGNATURE_LENGTH] = [0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A];
const CHUNK_TYPE_LENGTH: usize = 4;
const IHDR: [u8; CHUNK_TYPE_LENGTH] = [b'I', b'H', b'D', b'R'];
const IDAT: [u8; CHUNK_TYPE_LENGTH] = [b'I', b'D', b'A', b'T'];
const IEND: [u8; CHUNK_TYPE_LENGTH] = [b'I', b'E', b'N', b'D'];

#[derive(Debug, PartialEq)]
pub(crate) enum ChunkData {
    Ihdr(IhdrChunkData),
    Idat(IdatChunkData),
    Iend,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitDepth {
    _1,
    _2,
    _4,
    _8,
    _16,
}

impl TryFrom<u8> for BitDepth {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(BitDepth::_1),
            2 => Ok(BitDepth::_2),
            4 => Ok(BitDepth::_4),
            8 => Ok(BitDepth::_8),
            16 => Ok(BitDepth::_16),
            v => Err(format!("Invalid bit depth: {v}")),
        }
    }
}

impl BitDepth {
    pub fn value(&self) -> u8 {
        match &self {
            BitDepth::_1 => 1,
            BitDepth::_2 => 2,
            BitDepth::_4 => 4,
            BitDepth::_8 => 8,
            BitDepth::_16 => 16,
        }
    }
}

impl fmt::Display for BitDepth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ColorType {
    Grayscale,
    Truecolor,
    Indexed,
    GrayscaleAndAlpha,
    TruecolorAndAlpha,
}

impl TryFrom<u8> for ColorType {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ColorType::Grayscale),
            2 => Ok(ColorType::Truecolor),
            3 => Ok(ColorType::Indexed),
            4 => Ok(ColorType::GrayscaleAndAlpha),
            6 => Ok(ColorType::TruecolorAndAlpha),
            v => Err(format!("Invalid color type: {v}")),
        }
    }
}

impl ColorType {
    pub(crate) fn value(&self) -> u8 {
        match &self {
            ColorType::Grayscale => 0,
            ColorType::Truecolor => 2,
            ColorType::Indexed => 3,
            ColorType::GrayscaleAndAlpha => 4,
            ColorType::TruecolorAndAlpha => 6,
        }
    }

    pub fn num_samples(&self) -> u8 {
        match &self {
            ColorType::Grayscale => 1,
            ColorType::Truecolor => 3,
            ColorType::Indexed => 1,
            ColorType::GrayscaleAndAlpha => 2,
            ColorType::TruecolorAndAlpha => 4,
        }
    }

    pub(crate) fn combination_allowed(&self, bit_depth: &BitDepth) -> bool {
        match &self {
            ColorType::Grayscale => {
                *bit_depth == BitDepth::_1
                    || *bit_depth == BitDepth::_2
                    || *bit_depth == BitDepth::_4
                    || *bit_depth == BitDepth::_8
                    || *bit_depth == BitDepth::_16
            }
            ColorType::Truecolor => *bit_depth == BitDepth::_8 || *bit_depth == BitDepth::_16,
            ColorType::Indexed => {
                *bit_depth == BitDepth::_1
                    || *bit_depth == BitDepth::_2
                    || *bit_depth == BitDepth::_4
                    || *bit_depth == BitDepth::_8
            }
            ColorType::GrayscaleAndAlpha => {
                *bit_depth == BitDepth::_8 || *bit_depth == BitDepth::_16
            }
            ColorType::TruecolorAndAlpha => {
                *bit_depth == BitDepth::_8 || *bit_depth == BitDepth::_16
            }
        }
    }
}

impl fmt::Display for ColorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str_representation = match self {
            ColorType::Grayscale => "grayscale",
            ColorType::Truecolor => "truecolor",
            ColorType::Indexed => "indexed",
            ColorType::GrayscaleAndAlpha => "grayscale and alpha",
            ColorType::TruecolorAndAlpha => "truecolor and alpha",
        };
        write!(f, "{} ({})", self.value(), str_representation)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompressionMethod {
    DeflateInflate,
}

impl TryFrom<u8> for CompressionMethod {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(CompressionMethod::DeflateInflate),
            v => Err(format!("Unknown compression method: {v}")),
        }
    }
}

impl CompressionMethod {
    pub(crate) fn value(&self) -> u8 {
        match self {
            CompressionMethod::DeflateInflate => 0,
        }
    }
}

impl fmt::Display for CompressionMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str_representation = match self {
            CompressionMethod::DeflateInflate => "deflate/inflate",
        };
        write!(f, "{} ({})", self.value(), str_representation)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FilterMethod {
    AdaptiveFiltering,
}

impl TryFrom<u8> for FilterMethod {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(FilterMethod::AdaptiveFiltering),
            v => Err(format!("Unknown filter method: {v}")),
        }
    }
}

impl FilterMethod {
    fn value(&self) -> u8 {
        match self {
            FilterMethod::AdaptiveFiltering => 0,
        }
    }
}

impl fmt::Display for FilterMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str_representation = match self {
            FilterMethod::AdaptiveFiltering => "adaptive filtering with five basic filter types",
        };
        write!(f, "{} ({})", self.value(), str_representation)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterlaceMethod {
    None,
    Adam7,
}

impl TryFrom<u8> for InterlaceMethod {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(InterlaceMethod::None),
            1 => Ok(InterlaceMethod::Adam7),
            v => Err(format!("Unknown interlace method: {v}")),
        }
    }
}

impl InterlaceMethod {
    fn value(&self) -> u8 {
        match self {
            InterlaceMethod::None => 0,
            InterlaceMethod::Adam7 => 1,
        }
    }
}

impl fmt::Display for InterlaceMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str_representation = match self {
            InterlaceMethod::None => "no interlace",
            InterlaceMethod::Adam7 => "Adam7 interlace",
        };
        write!(f, "{} ({})", self.value(), str_representation)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IhdrChunkData {
    width: u32,
    height: u32,
    bit_depth: BitDepth,
    color_type: ColorType,
    compression_method: CompressionMethod,
    filter_method: FilterMethod,
    interlace_method: InterlaceMethod,
}

impl IhdrChunkData {
    pub fn new(
        width: u32,
        height: u32,
        bit_depth: BitDepth,
        color_type: ColorType,
        compression_method: CompressionMethod,
        filter_method: FilterMethod,
        interlace_method: InterlaceMethod,
    ) -> Self {
        Self {
            width,
            height,
            bit_depth,
            color_type,
            compression_method,
            filter_method,
            interlace_method,
        }
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn bit_depth(&self) -> &BitDepth {
        &self.bit_depth
    }

    pub fn color_type(&self) -> &ColorType {
        &self.color_type
    }

    /// Calculates bytes needed per pixel
    pub fn bpp(&self) -> u8 {
        // FIXME: This does not need to be calculated every time
        let mut bpp = self.color_type().num_samples() * self.bit_depth().value();
        let round_up = (bpp % 8) != 0;
        bpp /= 8;
        if round_up {
            bpp += 1;
        }
        bpp
    }
}

impl TryFrom<Vec<u8>> for IhdrChunkData {
    type Error = String;
    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        let mut iter = value.iter();

        // Per specification, the IHDR chunk looks as follows:
        // Width:              4 bytes
        // Height:             4 bytes
        // Bit depth:          1 byte
        // Color type:         1 byte
        // Compression method: 1 byte
        // Filter method:      1 byte
        // Interlace method:   1 byte
        let width = u32::from_be_bytes(
            read_bytes(&mut iter, &mut [0; 4]).ok_or("Expected 4 bytes for width")?,
        );
        let height = u32::from_be_bytes(
            read_bytes(&mut iter, &mut [0; 4]).ok_or("Expected 4 bytes for height")?,
        );
        let bit_depth = BitDepth::try_from(*iter.next().ok_or("Expected bit depth byte")?)?;
        let color_type = ColorType::try_from(*iter.next().ok_or("Expected color type byte")?)?;
        let compression_method =
            CompressionMethod::try_from(*iter.next().ok_or("Expected compression method byte")?)?;
        let filter_method =
            FilterMethod::try_from(*iter.next().ok_or("Expected filter method byte")?)?;
        let interlace_method =
            InterlaceMethod::try_from(*iter.next().ok_or("Expected interlace method byte")?)?;

        if !color_type.combination_allowed(&bit_depth) {
            return Err(format!("The combination of bit depth {bit_depth} and color type {color_type} is not allowed"));
        }

        Ok(Self {
            width,
            height,
            bit_depth,
            color_type,
            compression_method,
            filter_method,
            interlace_method,
        })
    }
}

impl fmt::Display for IhdrChunkData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "IHDR {{\n  Width: {},\n  Height: {},\n  Bit depth: {},\n  Color type: {},\n  Compression method: {},\n  Filter method: {},\n  Interlace method: {}\n}}",
            self.width, self.height, self.bit_depth, self.color_type, self.compression_method, self.filter_method, self.interlace_method
        )
    }
}

pub type IdatChunkData = Vec<u8>;

#[derive(PartialEq, Debug)]
pub struct IdatCombinedChunkData {
    scanlines: Vec<(FilterType, IdatChunkData)>,
}

impl IdatCombinedChunkData {
    pub fn new(scanlines: Vec<(FilterType, IdatChunkData)>) -> Self {
        Self { scanlines }
    }
    pub fn into_scanlines(self) -> Vec<(FilterType, IdatChunkData)> {
        self.scanlines
    }
    pub fn scanlines(&self) -> &Vec<(FilterType, IdatChunkData)> {
        &self.scanlines
    }
}

impl TryFrom<(&IhdrChunkData, Vec<IdatChunkData>)> for IdatCombinedChunkData {
    type Error = String;
    fn try_from(value: (&IhdrChunkData, Vec<IdatChunkData>)) -> Result<Self, Self::Error> {
        // Decode the raw data using zlib (DEFLATE)
        // FIXME: The only valid de/compression for PNG is DEFLATE, however it's open
        //       to extension, so we should definitely check the compression type here
        let all_data = value.1.into_iter().flatten().collect::<Vec<u8>>();
        let raw_data = all_data.as_slice();

        let mut decoder = zlib::Decoder::new(raw_data);
        let mut data = vec![];
        let _ = decoder
            .read_to_end(&mut data)
            .map_err(|err| err.to_string())?;

        let ihdr = value.0;
        let row_width = ihdr.width();
        let color_depth_bytes = match ihdr.bit_depth() {
            BitDepth::_1 | BitDepth::_2 | BitDepth::_4 | BitDepth::_8 => 1,
            BitDepth::_16 => 2,
        };
        let pixel_width = ihdr.color_type().num_samples();
        const FILTER_TYPE_WIDTH: usize = 1;
        let scanline_stride =
            FILTER_TYPE_WIDTH + (row_width as usize * color_depth_bytes * pixel_width as usize);

        let scanlines = data
            .into_iter()
            .as_slice()
            .chunks_exact(scanline_stride)
            .map(|chunk| (FilterType::try_from(chunk[0]).unwrap(), chunk[1..].to_vec()))
            .collect::<Vec<(FilterType, Vec<u8>)>>();
        Ok(Self::new(scanlines))
    }
}

#[derive(PartialEq, Debug)]
pub enum FilterType {
    None,
    Sub,
    Up,
    Average,
    Paeth,
}

impl TryFrom<u8> for FilterType {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(FilterType::None),
            1 => Ok(FilterType::Sub),
            2 => Ok(FilterType::Up),
            3 => Ok(FilterType::Average),
            4 => Ok(FilterType::Paeth),
            _ => Err(format!("Invalid filter type: {value}")),
        }
    }
}

impl fmt::Display for ChunkData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChunkData::Ihdr(d) => d.fmt(f),
            ChunkData::Idat(d) => write!(f, "{:?}", d),
            ChunkData::Iend => write!(f, "<none>"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Chunk {
    data: ChunkData,
}

impl Chunk {
    pub fn new(data: ChunkData) -> Self {
        Self { data }
    }
    pub fn data(&self) -> &ChunkData {
        &self.data
    }
    pub fn into_data(self) -> ChunkData {
        self.data
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Data: {}", self.data)
    }
}

struct PngIterator<I: ExactSizeIterator<Item = u8>> {
    inner: I,
    crc_table: [u32; 256],
}

impl<I: ExactSizeIterator<Item = u8>> PngIterator<I> {
    fn new(inner: I) -> Self {
        let mut crc_table: [u32; 256] = [0; 256];
        for (i, item) in crc_table.iter_mut().enumerate() {
            let mut c = i;
            for _ in 0..8 {
                if (c & 1) != 0 {
                    c = 0xedb88320 ^ (c >> 1);
                } else {
                    c >>= 1;
                }
            }
            *item = c as u32;
        }
        PngIterator { inner, crc_table }
    }

    fn validate_png_signature(&mut self) -> io::Result<()> {
        let png_signature = self.read_bytes::<PNG_SIGNATURE_LENGTH>()?;
        if png_signature == PNG_SIGNATURE {
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid PNG signature",
            ))
        }
    }

    /// The chunk consists of the following parts:
    /// - length (4 bytes)
    /// - chunk type (4 bytes)
    /// - chunk data (length bytes)
    /// - CRC (4 bytes)
    fn parse_chunk(&mut self) -> io::Result<Option<Chunk>> {
        // Read the chunk
        let length = u32::from_be_bytes(self.read_bytes()?);
        let chunk_type = self.read_bytes::<4>()?;
        let mut chunk_data_raw: Vec<u8> = Vec::with_capacity(length as usize);
        for _ in 0..length {
            chunk_data_raw.push(self.inner.next().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "Failed to read the whole chunk",
                )
            })?);
        }
        let crc_got = u32::from_be_bytes(self.read_bytes()?);

        let crc_expected = self.crc(chunk_type, &chunk_data_raw);
        if crc_expected != crc_got {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!("CRC failed - expected (computed): {crc_expected}, got (inside chunk): {crc_got}"),
            ));
        }

        // Parse the chunk data according to the chunk type
        let chunk_data = match chunk_type {
            IHDR => ChunkData::Ihdr(IhdrChunkData::try_from(chunk_data_raw).unwrap()),
            IDAT => ChunkData::Idat(chunk_data_raw),
            IEND => ChunkData::Iend,
            _ => {
                warn!(
                    "Skipping PNG chunk: {}",
                    str::from_utf8(&chunk_type).unwrap_or("<non-utf8-data>")
                );
                return Ok(None);
            }
        };

        Ok(Some(Chunk::new(chunk_data)))
    }
    fn update_crc(&self, mut crc: u32, chunk_type: [u8; 4], chunk_data_raw: &[u8]) -> u32 {
        chunk_type
            .chain(chunk_data_raw)
            .bytes()
            .map(|b| b.unwrap())
            .for_each(|b| crc = self.crc_table[((crc ^ b as u32) & 0xff) as usize] ^ (crc >> 8u32));

        crc
    }

    fn crc(&self, chunk_type: [u8; 4], chunk_data_raw: &[u8]) -> u32 {
        self.update_crc(0xffffffff, chunk_type, chunk_data_raw) ^ 0xffffffff
    }

    // FIXME: This is a dupliate of the generic `read_bytes(Iter)` function.
    fn read_bytes<const N: usize>(&mut self) -> io::Result<[u8; N]> {
        let mut buf = [0; N];
        if N == 0 {
            return Ok(buf);
        }

        for b in &mut buf {
            *b = self
                .inner
                .next()
                .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "Unexpected EOF"))?;
        }

        Ok(buf)
    }
}

fn read_bytes<'a, const N: usize>(
    iter: &mut impl Iterator<Item = &'a u8>,
    buf: &mut [u8; N],
) -> Option<[u8; N]> {
    if N == 0 {
        return Some(*buf);
    }

    for b in buf.iter_mut().take(N) {
        *b = *iter.next()?;
    }

    Some(*buf)
}

#[derive(Debug)]
pub struct PngFile {
    chunks: Vec<Chunk>,
}

impl PngFile {
    pub fn ihdr(&self) -> &IhdrChunkData {
        let ihdr_chunk = self.chunks.first().expect("IHDR (PNG header) is not present, although the file was parsed just fine (this is wrong!)");
        let data = ihdr_chunk.data();
        match data {
            ChunkData::Ihdr(ihdr_data) => ihdr_data,
            _ => panic!("The first chunk MUST be the IHDR chunk. The parses should have thrown an error! This is an invalid PNG.")
        }
    }
    pub fn try_into_pixels(self) -> Result<Vec<u8>, String> {
        let mut iter = self.chunks.into_iter();
        let first_chunk = iter.next().unwrap();
        let ihdr_chunk = match first_chunk.into_data() {
            ChunkData::Ihdr(ihdr_data) => ihdr_data,
            _ => unreachable!(),
        };

        // Combine the IDAT chunks
        let encoded_pixels = iter
            .filter(|chunk| matches!(chunk.data, ChunkData::Idat(_)))
            .map(|data_chunk| match data_chunk.data {
                ChunkData::Idat(data) => data,
                _ => unreachable!(),
            })
            .collect::<Vec<IdatChunkData>>();

        let bpp = ihdr_chunk.bpp() as usize;
        let scanlines = IdatCombinedChunkData::try_from((&ihdr_chunk, encoded_pixels))
            .map(|data| data.into_scanlines())?;
        debug!("Total scanlines: {}", scanlines.len());

        Ok(scanlines
            .into_iter()
            .fold(Vec::new(), |mut raw_bytes, scanline| {
                let row_start = raw_bytes.len();
                let filter_type = scanline.0;
                let data = scanline.1;

                fn raw(x: isize, raw_bytes: &[u8], row_start: usize) -> u8 {
                    if x < 0 {
                        0
                    } else {
                        raw_bytes[x as usize + row_start]
                    }
                }

                fn sub_reverse(
                    x: usize,
                    sub: u8,
                    bpp: usize,
                    raw_bytes: &[u8],
                    row_start: usize,
                ) -> u8 {
                    // Raw(x) = Sub(x) + Raw(x-bpp)
                    // bpp = bytes per pixel (rounded up)
                    // for all x < 0 assume Raw(x) = 0
                    // x = 0..scanline.len()-1
                    ((sub as usize + raw(x as isize - bpp as isize, raw_bytes, row_start) as usize)
                        % 256_usize) as u8
                }

                fn prior(x: isize, row_width: usize, raw_bytes: &[u8], row_start: usize) -> u8 {
                    if x < 0 || row_start == 0 {
                        0
                    } else {
                        raw_bytes[(x - row_width as isize + row_start as isize) as usize]
                    }
                }

                fn up_reverse(
                    x: usize,
                    up: u8,
                    raw_bytes: &[u8],
                    row_start: usize,
                    row_width: usize,
                ) -> u8 {
                    // Raw(x) = Up(x) + Prior(x)
                    // On the first scanline of an image (or of a pass of an interlaced image), assume Prior(x) = 0 for all x.
                    // x = 0..scanline.len()-1
                    ((up as usize + prior(x as isize, row_width, raw_bytes, row_start) as usize)
                        % 256_usize) as u8
                }

                fn paeth_predictor(a: isize, b: isize, c: isize) -> u8 {
                    let p = a + b - c;
                    let pa = (p - a).abs();
                    let pb = (p - b).abs();
                    let pc = (p - c).abs();

                    let res = if pa <= pb && pa <= pc {
                        a
                    } else if pb <= pc {
                        b
                    } else {
                        c
                    };
                    let res = res % 256_isize;
                    res as u8
                }

                fn paeth_rev(
                    paeth: u8,
                    x: usize,
                    bpp: usize,
                    raw_bytes: &[u8],
                    row_start: usize,
                    row_width: usize,
                ) -> u8 {
                    ((paeth as usize
                        + paeth_predictor(
                            raw(x as isize - bpp as isize, raw_bytes, row_start) as isize,
                            prior(x as isize, row_width, raw_bytes, row_start) as isize,
                            prior(x as isize - bpp as isize, row_width, raw_bytes, row_start)
                                as isize,
                        ) as usize)
                        % 256_usize) as u8
                }
                match filter_type {
                    FilterType::None => data.into_iter().for_each(|b| raw_bytes.push(b)),
                    FilterType::Sub => data.iter().enumerate().for_each(|(x, sub)| {
                        raw_bytes.push(sub_reverse(x, *sub, bpp, &raw_bytes, row_start));
                    }),
                    FilterType::Up => data.iter().enumerate().for_each(|(x, up)| {
                        raw_bytes.push(up_reverse(x, *up, &raw_bytes, row_start, data.len()));
                    }),
                    FilterType::Paeth => {
                        data.iter().enumerate().for_each(|(x, paeth)| {
                            raw_bytes.push(paeth_rev(
                                *paeth,
                                x,
                                bpp,
                                &raw_bytes,
                                row_start,
                                data.len(),
                            ));
                        });
                    }
                    _ => todo!("Unsupported filter type: {filter_type:?}"),
                };

                raw_bytes
            }))
    }
}

impl fmt::Display for PngFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Chunk #: {}\n", self.chunks.len())?;
        self.chunks
            .iter()
            .enumerate()
            .for_each(|(idx, chunk)| writeln!(f, "==Chunk {idx}==\n{chunk}\n").unwrap());
        Ok(())
    }
}

pub fn parse_png(file_path: &path::Path) -> Result<PngFile, io::Error> {
    info!("Parsing {file_path:?}");
    parse_png_content(fs::read(file_path)?)
}

/// Parses the PNG
pub fn parse_png_content(content: Vec<u8>) -> Result<PngFile, io::Error> {
    let mut png_iter = PngIterator::new(content.into_iter());
    png_iter.validate_png_signature()?;

    let mut chunks = vec![];
    loop {
        let chunk = png_iter.parse_chunk()?;
        if let Some(chunk) = chunk {
            if chunk.data() == &ChunkData::Iend {
                chunks.push(chunk);
                return Ok(PngFile { chunks });
            }
            chunks.push(chunk);
        }
    }
}

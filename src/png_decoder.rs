use std::fmt;
use std::fs;
use std::io;
use std::usize;

const PNG_SIGNATURE_LENGTH: usize = 8;
const PNG_SIGNATURE: [u8; PNG_SIGNATURE_LENGTH] = [0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A];

#[derive(Debug, PartialEq)]
pub(crate) enum ChunkData {
    Ihdr(IhdrChunkData),
    Idat(Vec<u8>),
    Iend,
}

#[derive(Debug, PartialEq)]
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
    fn value(&self) -> u8 {
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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
}

impl TryFrom<Vec<u8>> for IhdrChunkData {
    type Error = String;
    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        let mut buf = [0; 4];
        let mut iter = value.iter();

        for b in &mut buf {
            *b = *iter.next().ok_or("Expected 4 bytes for width")?;
        }

        let width = u32::from_be_bytes(buf);

        buf = [0; 4];

        for b in &mut buf {
            *b = *iter.next().ok_or("Expected 4 bytes for height")?;
        }

        let height = u32::from_be_bytes(buf);

        let bit_depth = BitDepth::try_from(*iter.next().ok_or("Expected bit depth byte")?)?;

        let color_type = ColorType::try_from(*iter.next().ok_or("Expected color type byte")?)?;

        if !color_type.combination_allowed(&bit_depth) {
            return Err(format!("The combination of bit depth {bit_depth} and color type {color_type} is not allowed"));
        }

        let compression_method =
            CompressionMethod::try_from(*iter.next().ok_or("Expected compression method byte")?)?;

        let filter_method =
            FilterMethod::try_from(*iter.next().ok_or("Expected filter method byte")?)?;

        let interlace_method =
            InterlaceMethod::try_from(*iter.next().ok_or("Expected interlace method byte")?)?;

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
    length: usize,
    data: ChunkData,
    crc: [u8; 4],
}

impl Chunk {
    pub fn new(length: usize, data: ChunkData, crc: [u8; 4]) -> Self {
        Self { length, data, crc }
    }
    fn crc(&self) -> &[u8; 4] {
        &self.crc
    }
    fn data(&self) -> &ChunkData {
        &self.data
    }
    fn chunk_type(&self) -> &str {
        match self.data {
            ChunkData::Ihdr(_) => "IHDR",
            ChunkData::Idat(_) => "IDAT",
            ChunkData::Iend => "IEND",
        }
    }
    fn length(&self) -> usize {
        self.length
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Length: {}", self.length)?;
        writeln!(f, "Data: {}", self.data)?;
        write!(f, "CRC: {:?}", self.crc)
    }
}

struct PngIterator<I: ExactSizeIterator<Item = u8>> {
    inner: I,
}

impl<I: ExactSizeIterator<Item = u8>> PngIterator<I> {
    fn new(inner: I) -> Self {
        PngIterator { inner }
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
    fn parse_chunk(&mut self) -> io::Result<Chunk> {
        let length = u32::from_be_bytes(self.read_bytes()?);
        let chunk_type = String::from_utf8(self.read_bytes::<4>()?.to_vec()).map_err(|e| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Failed to parse chunk type: {e}"),
            )
        })?;
        let mut chunk_data_raw: Vec<u8> = Vec::with_capacity(length as usize);

        println!("Reading {length} bytes (chunk data)");

        for _ in 0..length {
            chunk_data_raw.push(self.inner.next().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "Failed to read the whole chunk",
                )
            })?);
        }

        let crc = self.read_bytes()?;
        let chunk_data = match chunk_type.as_str() {
            "IHDR" => ChunkData::Ihdr(IhdrChunkData::try_from(chunk_data_raw).unwrap()),
            "IDAT" => ChunkData::Idat(chunk_data_raw),
            "IEND" => ChunkData::Iend,
            _ => todo!(),
        };

        Ok(Chunk::new(length as usize, chunk_data, crc))
    }

    fn read_bytes<const N: usize>(&mut self) -> io::Result<[u8; N]> {
        let mut buf = [0; N];
        if N == 0 {
            return Ok(buf);
        }

        println!("Reading {N} bytes");

        for b in &mut buf {
            *b = self
                .inner
                .next()
                .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "Unexpected EOF"))?;
        }

        Ok(buf)
    }
}

#[derive(Debug)]
pub struct PngFile<'a> {
    name: &'a str,
    chunks: Vec<Chunk>,
}

impl<'a> PngFile<'a> {
    pub fn ihdr(&self) -> &IhdrChunkData {
        let ihdr_chunk = self.chunks.first().unwrap();
        let data = ihdr_chunk.data();
        match data {
            ChunkData::Ihdr(ihdr_data) => &ihdr_data,
            _ => unreachable!(),
        }
    }
    pub fn name(&self) -> &'a str {
        self.name
    }
    pub(crate) fn chunks(&self) -> &Vec<Chunk> {
        &self.chunks
    }
}

impl fmt::Display for PngFile<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Name: {}", self.name)?;
        writeln!(f, "Chunk #: {}\n", self.chunks.len())?;
        self.chunks
            .iter()
            .enumerate()
            .for_each(|(idx, chunk)| writeln!(f, "==Chunk {idx}==\n{chunk}\n").unwrap());
        Ok(())
    }
}

pub fn parse_png<'a>(file_path: &'a str) -> Result<PngFile<'a>, io::Error> {
    let mut chunks = vec![];
    let contents = fs::read(file_path)?;
    let mut png_iter = PngIterator::new(contents.into_iter());
    png_iter.validate_png_signature()?;
    println!("PNG signature valid");
    loop {
        let chunk = png_iter.parse_chunk()?;
        println!("Chunk type: {}", chunk.chunk_type());
        if chunk.data() == &ChunkData::Iend {
            chunks.push(chunk);
            println!("Read all chunks");
            return Ok(PngFile {
                name: file_path,
                chunks,
            });
        }
        chunks.push(chunk);
    }
}

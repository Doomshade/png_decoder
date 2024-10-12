use std::env;
use std::fmt;
use std::fs;
use std::io::{self, Error, ErrorKind};

const PNG_SIGNATURE: [u8; 8] = [0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A];

#[derive(Debug, PartialEq, Eq)]
enum ChunkType {
    Ihdr,
    Idat,
    Iend,
    Eof,
    Invalid,
}

impl fmt::Display for ChunkType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChunkType::Ihdr => write!(f, "IHDR")?,
            ChunkType::Idat => write!(f, "IDAT")?,
            ChunkType::Iend => write!(f, "IEND")?,
            ChunkType::Eof => write!(f, "EOF")?,
            ChunkType::Invalid => write!(f, "Invalid")?,
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Chunk {
    length: usize,
    chunk_type: ChunkType,
    data: Vec<u8>,
    crc: [u8; 4],
}

impl Chunk {
    pub fn new(length: usize, chunk_type: ChunkType, data: Vec<u8>, crc: [u8; 4]) -> Self {
        Self {
            length,
            chunk_type,
            data,
            crc,
        }
    }
}

impl From<[u8; 4]> for ChunkType {
    fn from(value: [u8; 4]) -> Self {
        let s = match std::str::from_utf8(&value) {
            Ok(v) => v,
            Err(e) => panic!("Invalid shit: {}", e),
        };

        match s {
            "IHDR" => ChunkType::Ihdr,
            "IDAT" => ChunkType::Idat,
            "IEND" => ChunkType::Iend,
            _ => ChunkType::Invalid,
        }
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
        for &expected in &PNG_SIGNATURE {
            match self.inner.next() {
                Some(byte) if byte == expected => continue,
                _ => return Err(Error::new(ErrorKind::InvalidData, "Invalid PNG signature")),
            }
        }
        Ok(())
    }

    /// The chunk consists of the following parts:
    /// - length (4 bytes)
    /// - chunk type (4 bytes)
    /// - chunk data (length bytes)
    /// - CRC (4 bytes)
    fn parse_chunk(&mut self) -> io::Result<Chunk> {
        if self.inner.len() == 0 {
            return Ok(Chunk::new(0, ChunkType::Eof, vec![], [0; 4]));
        }

        let length = u32::from_be_bytes(self.read_bytes()?);
        let chunk_type = ChunkType::from(self.read_bytes()?);
        let mut chunk_data: Vec<u8> = Vec::with_capacity(length as usize);

        for _ in 0..length {
            chunk_data.push(self.inner.next().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "Failed to read the whole chunk",
                )
            })?);
        }

        let crc = self.read_bytes()?;

        Ok(Chunk::new(length as usize, chunk_type, chunk_data, crc))
    }

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

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("Please provide a path to the PNG file");
    let contents = fs::read(file)?;
    let mut png_iter = PngIterator::new(contents.into_iter());
    png_iter.validate_png_signature()?;
    println!("PNG signature valid");
    loop {
        let chunk = png_iter.parse_chunk()?;
        if chunk.chunk_type == ChunkType::Eof {
            println!("Read all chunks");
            return Ok(());
        }

        println!("Length: {}", &chunk.length);
        println!("Chunk type: {}", &chunk.chunk_type);
        println!("Data: {:?}", &chunk.data);
    }
}

#[test]
fn test_valid_pngs() {
    todo!();
}

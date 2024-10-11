use std::env;
use std::fs;
use std::io::{self, Error, ErrorKind};

const PNG_SIGNATURE: [u8; 8] = [0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A];

#[derive(Debug)]
enum ChunkType {
    Ihdr,
    Invalid,
}

#[derive(Debug)]
struct Chunk {
    length: usize,
    chunk_type: ChunkType,
    data: Vec<u8>,
}

impl Chunk {
    pub fn new(length: usize, chunk_type: ChunkType, data: Vec<u8>) -> Self {
        Self {
            length,
            chunk_type,
            data,
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
            _ => ChunkType::Invalid,
        }
    }
}

struct PngIterator<I: Iterator<Item = u8>> {
    inner: I,
}

impl<I: Iterator<Item = u8>> PngIterator<I> {
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

        Ok(Chunk::new(length as usize, chunk_type, chunk_data))
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
    loop {
        let chunk = png_iter.parse_chunk()?;
        dbg!(&chunk);
        dbg!(&chunk.length);
        dbg!(&chunk.chunk_type);
        dbg!(&chunk.data);
    }
}

use std::env;
use std::fs;
use std::io::{self, Error, ErrorKind};

const PNG_SIGNATURE: [u8; 8] = [0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A];

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
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("Please provide a path to the PNG file");
    let contents = fs::read(file)?;
    let mut png_iter = PngIterator::new(contents.into_iter());
    png_iter.validate_png_signature()?;
    println!("PNG signature valid");
    Ok(())
}

use std::env;

use png_decoder::{BitDepth, ColorType};
use show_image::PixelFormat;
pub mod png_decoder;

#[show_image::main]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("Please provide a path to the PNG file");
    let png_file = png_decoder::parse_png(file)?;

    let ihdr = png_file.ihdr();

    if ihdr.bit_depth() != &BitDepth::_8 {
        panic!("Bit depth not supported: {}", ihdr.bit_depth());
    }

    println!("{ihdr}");

    let width = ihdr.width();
    let height = ihdr.height();

    println!("Image width: {width}, height: {height}");

    let data = png_file.data();
    println!("Image data: {data:?}");
    let image_data = data.pixels();

    let pixel_format = match ihdr.color_type() {
        ColorType::Grayscale => PixelFormat::Mono8,
        ColorType::Truecolor => PixelFormat::Rgb8,
        _ => panic!("Unknown pixel format: {}", ihdr.color_type()),
    };

    let image = show_image::ImageView::new(
        show_image::ImageInfo::new(pixel_format, width, height),
        &image_data,
    );

    println!("Showing image");
    let window = show_image::create_window("image", Default::default())?;
    window.set_image(file, image)?;

    for event in window.event_channel()? {
        if let show_image::event::WindowEvent::KeyboardInput(event) = event {
            println!("{:#?}", event);
            if event.input.key_code == Some(show_image::event::VirtualKeyCode::Escape)
                && event.input.state.is_pressed()
            {
                break;
            }
        }
    }

    Ok(())
}

mod test {
    use std::fs;
    use std::io;

    use crate::png_decoder;

    #[test]
    fn test_valid_pngs() -> Result<(), Box<dyn std::error::Error + 'static>> {
        let png_dir = fs::read_dir("resources/pngs/valid")?;

        for png in png_dir {
            let path = png?.path();
            let path_str = path
                .to_str()
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Path not found"))?;
            let p = png_decoder::parse_png(path_str)?;
            println!("{p}");
        }
        Ok(())
    }
}

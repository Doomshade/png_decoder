use std::{env, fs, io, path};

use png_decoder::{BitDepth, ColorType};
use show_image::{Alpha, PixelFormat};
pub mod png_decoder;

#[show_image::main]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = env::args()
        .nth(1)
        .expect("Please provide a path to the PNG file");
    let path = path::Path::new(&file);

    // TODO: This could be handled better, but it's a toy project so who cares
    let os_file_name = path::Path::file_name(path).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("Failed to get the file name of {file}"),
        )
    })?;
    let file_name = os_file_name.to_str().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::Other,
            format!("Failed to geth the file name of {file}"),
        )
    })?;

    println!("Parsing {file_name}");
    let png_file = png_decoder::parse_png(fs::read(path)?)?;
    println!("{file_name} is valid");

    // The PNG header contains information regarding the image,
    // such as the width, height, bit depth, color type, ...
    // We pass this information to the image library.
    let ihdr = png_file.ihdr();
    println!("Image info: {ihdr}");

    // FIXME: We can *technically* support bit depth of 1 as the image library supports grayscale
    if ihdr.bit_depth() != &BitDepth::_8 {
        panic!("Bit depth not supported: {}", ihdr.bit_depth());
    }

    // Create an image view for the image library.
    let width = ihdr.width();
    let height = ihdr.height();
    let pixel_format = match ihdr.color_type() {
        ColorType::Grayscale => PixelFormat::Mono8,
        ColorType::Truecolor => PixelFormat::Rgb8,
        ColorType::TruecolorAndAlpha => PixelFormat::Rgba8(Alpha::Unpremultiplied),
        ColorType::GrayscaleAndAlpha => PixelFormat::MonoAlpha8(Alpha::Unpremultiplied),
        _ => panic!("Unknown pixel format: {}", ihdr.color_type()),
    };

    let pixels = png_file.try_into_pixels()?;
    println!("Raw data length: {}", pixels.len());
    let image = show_image::ImageView::new(
        show_image::ImageInfo::new(pixel_format, width, height),
        &pixels,
    );

    // Create a window and display the image.
    let window = show_image::create_window(file_name, Default::default())?;
    window.set_image(file, image)?;

    for event in window.event_channel()? {
        if let show_image::event::WindowEvent::KeyboardInput(event) = event {
            //println!("{:#?}", event);
            if event.input.key_code == Some(show_image::event::VirtualKeyCode::Escape)
                && event.input.state.is_pressed()
            {
                break;
            }
        }
    }

    Ok(())
}

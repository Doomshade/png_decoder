use std::{env, fs, path};

use png_decoder::{BitDepth, ColorType};
use show_image::{Alpha, PixelFormat};
#[macro_use]
extern crate log;
extern crate simplelog;
use simplelog::*;
pub mod png_decoder;

#[show_image::main]
fn main() {
    const LOG_FILE: &str = "png_decoder.log";
    let log_file = fs::File::create(LOG_FILE).map_or_else(
        |e| {
            warn!("Failed to create file. Reason: {e}");
            None
        },
        Some,
    );
    let mut loggers: Vec<Box<dyn SharedLogger>> = vec![TermLogger::new(
        LevelFilter::Warn,
        Config::default(),
        TerminalMode::Mixed,
        ColorChoice::Auto,
    )];

    if let Some(file) = log_file {
        loggers.push(WriteLogger::new(
            LevelFilter::Debug,
            Config::default(),
            file,
        ));
    }
    if let Err(e) = CombinedLogger::init(loggers) {
        println!("Failed to initialize logger. Reason: {e}");
        return;
    };

    let file = match env::args().nth(1) {
        Some(file) => file,
        None => {
            error!("Please provide a path to the PNG file");
            return;
        }
    };
    let path = path::Path::new(&file);

    // TODO: This could be handled better, but it's a toy project so who cares
    // "Failed to get the file name of {file}"
    let os_file_name = match path::Path::file_name(path) {
        Some(os_file_name) => os_file_name,
        None => {
            error!("Failed to get the file name of {file}");
            return;
        }
    };
    let file_name = match os_file_name.to_str() {
        Some(file_name) => file_name,
        None => {
            error!("Failed to get the file name of {file}");
            return;
        }
    };

    info!("Parsing {file_name}");

    let file_content = match fs::read(path) {
        Ok(file_content) => file_content,
        Err(e) => {
            error!("Failed to read the contents of file {file}: {e}");
            return;
        }
    };
    let png_file = match png_decoder::parse_png(file_content) {
        Ok(png_file) => png_file,
        Err(e) => {
            error!("Failed to parse PNG file {file}: {e}");
            return;
        }
    };

    debug!("{file_name} is valid");

    // The PNG header contains information regarding the image,
    // such as the width, height, bit depth, color type, ...
    // We pass this information to the image library.
    let ihdr = png_file.ihdr();
    debug!("Image info: {ihdr}");

    // FIXME: We can *technically* support bit depth of 1 as the image library supports grayscale
    if ihdr.bit_depth() != &BitDepth::_8 {
        error!("Bit depth not supported: {}", ihdr.bit_depth());
        return;
    }

    // Create an image view for the image library.
    let width = ihdr.width();
    let height = ihdr.height();
    let pixel_format = match ihdr.color_type() {
        ColorType::Grayscale => PixelFormat::Mono8,
        ColorType::Truecolor => PixelFormat::Rgb8,
        ColorType::TruecolorAndAlpha => PixelFormat::Rgba8(Alpha::Unpremultiplied),
        ColorType::GrayscaleAndAlpha => PixelFormat::MonoAlpha8(Alpha::Unpremultiplied),
        _ => {
            error!("Unsupported pixel format: {}", ihdr.color_type());
            return;
        }
    };

    let pixels = match png_file.try_into_pixels() {
        Ok(pixels) => pixels,
        Err(e) => {
            error!("Failed to read pixels: {e}");
            return;
        }
    };
    debug!("Raw data length: {}", pixels.len());
    let image = show_image::ImageView::new(
        show_image::ImageInfo::new(pixel_format, width, height),
        &pixels,
    );

    // Create a window and display the image.
    let window = match show_image::create_window(file_name, Default::default()) {
        Ok(window) => window,
        Err(e) => {
            error!("Failed to create window. Reason: {e}");
            return;
        }
    };
    match window.set_image(file, image) {
        Ok(()) => {}
        Err(e) => {
            error!(
                "Failed to set the image for the window {:?}. Reason: {e}",
                window.id()
            );
            return;
        }
    };

    let event_channel = match window.event_channel() {
        Ok(event_channel) => event_channel,
        Err(e) => {
            error!("Failed to create window event channel. Reason: {e}");
            return;
        }
    };
    for event in event_channel {
        if let show_image::event::WindowEvent::KeyboardInput(event) = event {
            //println!("{:#?}", event);
            if event.input.key_code == Some(show_image::event::VirtualKeyCode::Escape)
                && event.input.state.is_pressed()
            {
                break;
            }
        }
    }
}

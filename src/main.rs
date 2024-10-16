use std::env;
use std::fs;
use std::io;
pub mod png_decoder;

#[show_image::main]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("Please provide a path to the PNG file");
    let png_file = png_decoder::parse_png(file)?;

    let ihdr = png_file.ihdr();

    let width = ihdr.width();
    let height = ihdr.height();

    println!("Image width: {width}, height: {height}");
    let image_data = vec![255; width as usize * height as usize * 3];
    let image = show_image::ImageView::new(show_image::ImageInfo::rgb8(width, height), &image_data);

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

use clap::Parser;
use rustboy::cpu::CPU;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    rom: String,

    #[arg(short, long)]
    debug: bool,

    #[arg(short, long)]
    gb_doctor: bool,

    #[arg(short, long, default_value_t = 5)]
    pixel_size: u32,
}

fn main() {
    let args = Args::parse();
    let mut cpu = CPU::new(&args.rom, args.debug, args.gb_doctor, args.pixel_size);
    cpu.run();
}

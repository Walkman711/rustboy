use clap::Parser;
use rustboy::cpu::CPU;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    rom: String,

    #[arg(short, long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();
    let mut cpu = CPU::new(&args.rom, args.debug);
    cpu.run();
}

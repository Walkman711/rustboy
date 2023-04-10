use clap::Parser;
use rustboy::cpu::CPU;

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    rom: String,
}

fn main() {
    let args = Args::parse();
    let mut cpu = CPU::new(&args.rom);
    cpu.run();
}

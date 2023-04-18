#![allow(dead_code)]
use crate::{cpu::CPU_HZ, io_registers};

const DIV_HZ: u16 = 16_384;
const DIV_CYCLES_PER_TICK: u16 = (CPU_HZ / (DIV_HZ as u32)) as u16;

#[derive(Debug, Default)]
pub struct Timer {
    // internally represented as a u16, but only the high byte is exposed for R/W
    div: u16,
    tima: u8,
    tma: u8,
    tac: u8,
    timer_cycles: u16,
    cycles: u32,
}

impl Timer {
    fn tick(&mut self, cy: u8) -> bool {
        self.cycles = self.cycles.wrapping_add(cy as u32);

        if self.div > 0xFF {
            self.div = self.tma.into();
        }

        true
    }

    fn read(&self, addr: u16) -> u8 {
        match addr {
            io_registers::DIV => (self.div >> 8) as u8,
            io_registers::TIMA => self.tima,
            io_registers::TMA => self.tma,
            io_registers::TAC => self.tac,
            _ => panic! {"tried to access a non-timer register in timer.rs: {addr}"},
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // Any write resets the DIV register
            io_registers::DIV => self.div = 0x00,
            io_registers::TIMA => self.tima = val,
            io_registers::TMA => self.tma = val,
            io_registers::TAC => self.tac = val,
            _ => panic! {"tried to access a non-timer register in timer.rs: {addr}"},
        }
    }

    fn tac(&self) -> u16 {
        match self.tac {
            0x00 => 1024,
            0x01 => 16,
            0x10 => 64,
            0x11 => 256,
            _ => panic!("bad value in TAC register"),
        }
    }
}

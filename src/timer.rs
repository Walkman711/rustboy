#![allow(dead_code)]
use crate::{cpu::CPU_HZ, io_registers};

// TODO: need to use 16779Hz for SGB
const DIV_HZ: u16 = 16_384;
const DIV_CYCLES_PER_TICK: u16 = (CPU_HZ / (DIV_HZ as u32)) as u16;

#[derive(Debug)]
pub struct Timer {
    // internally represented as a u16, but only the high byte is exposed for R/W
    div: u16,
    tima: u8,
    tma: u8,
    tac: u8,
    timer_cycles: u16,
    cycles: u32,
    ticks_till_next_div: u16,
    ticks_till_next_timer: u16,
}

impl Default for Timer {
    fn default() -> Self {
        Self {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
            timer_cycles: 0,
            cycles: 0,
            ticks_till_next_div: DIV_CYCLES_PER_TICK,
            ticks_till_next_timer: 0,
        }
    }
}

impl Timer {
    fn tick(&mut self, cy: u8) -> bool {
        self.cycles = self.cycles.wrapping_add(cy as u32);

        // XXX: this can definitely be neater with some kind of clever wrapping
        // trigger div
        if self.ticks_till_next_div < (cy as u16) {
            self.div = self.div.wrapping_add(1);
            // Ex. there are 2 cycles till next div, but instruction took 4 cycles.
            // Next div should trigger in 16384 - (cycles - ticks_left) => 16384 - (4 - 2) => 16382
            self.ticks_till_next_div = DIV_CYCLES_PER_TICK - (cy as u16 - self.ticks_till_next_div);
        } else {
            self.ticks_till_next_div -= cy as u16
        }

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

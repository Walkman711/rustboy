#![allow(dead_code)]
use crate::io_registers;

// TODO: need to use 16779Hz for SGB
const DIV_HZ: u32 = 16_384;
// const DIV_CYCLES_PER_TICK: u16 = (CPU_HZ / (DIV_HZ as u32)) as u16;
const DIV_CYCLES_PER_TICK: u32 = 256;

#[derive(Debug)]
pub struct Timer {
    // internally represented as a u16, but only the high byte is exposed for R/W
    div: u16,
    tima: u8,
    tma: u8,
    tac: u8,
    internal_div: u32,
    internal_timer: u32,
    timer_enabled: bool,
}

impl Default for Timer {
    fn default() -> Self {
        Self {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
            internal_div: 0,
            internal_timer: 0,
            timer_enabled: true,
        }
    }
}

impl Timer {
    // TODO: should probably change the return type to the Interrupts enum
    pub fn tick(&mut self, cy: u32) -> bool {
        let mut timer_overflowed = false;
        self.internal_div += cy;

        if self.internal_div > DIV_CYCLES_PER_TICK {
            self.internal_div -= DIV_CYCLES_PER_TICK;
            self.div = self.div.wrapping_add(1);
            if self.div > 0xFF {
                self.div = self.tma.into();
            }
        }

        if self.timer_enabled {
            self.internal_timer += cy;
            if self.internal_timer > self.tac() as u32 {
                if self.tima == 0xFF {
                    self.tima = self.tma.into();
                    timer_overflowed = true;
                } else {
                    self.tima = self.tima.wrapping_add(1);
                }
            }
        }

        timer_overflowed
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            io_registers::DIV => (self.div >> 8) as u8,
            io_registers::TIMA => self.tima,
            io_registers::TMA => self.tma,
            io_registers::TAC => self.tac,
            _ => panic! {"tried to access a non-timer register in timer.rs: {addr}"},
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // Any write resets the DIV register
            io_registers::DIV => self.div = 0x00,
            io_registers::TIMA => self.tima = val,
            io_registers::TMA => self.tma = val,
            io_registers::TAC => {
                self.tac = val;
                // Timer enabled is set by bit 2 (0-indexed) of the value in TAC
                self.timer_enabled = (self.tac & (1 << 2)) == (1 << 2)
            }

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

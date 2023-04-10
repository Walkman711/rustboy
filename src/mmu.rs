#![allow(non_snake_case)]

use crate::{io_registers::IORegisters, mem_constants::*};

#[derive(Debug)]
pub struct MMU {
    // ROM 0
    rom_bank_0: [u8; (ROM_BANK_0_END - ROM_BANK_0_START + 1) as usize],
    // SWITCHABLE ROM
    // VRAM
    // TODO: can switch banks 0/1 in CGB mode
    vram: [u8; (VRAM_END - VRAM_START + 1) as usize],
    // EXT RAM
    ext_ram: [u8; (EXT_RAM_END - EXT_RAM_START + 1) as usize],
    // WRAM I
    wramI: [u8; (WRAM_I_END - WRAM_I_START + 1) as usize],
    // WRAM II
    // TODO: can switch banks 1-7 in CGB mode
    wramII: [u8; (WRAM_II_END - WRAM_II_START + 1) as usize],
    // SPRITE TABLE
    // IO Registers
    io: IORegisters,
    // HRAM
    hram: [u8; (HRAM_END - HRAM_START + 1) as usize],
    // Interrupt Enable Register (IE)
    ie: u8,
}

impl MMU {
    pub fn new(rom: &str) -> Self {
        let mut rom_bank_0 = [0; (ROM_BANK_0_END - ROM_BANK_0_START + 1) as usize];
        let bytes = std::fs::read(rom).expect("Reading from ROM failed in MMU::new()");
        // TODO: this is messy
        for i in 0..(ROM_BANK_0_END - ROM_BANK_0_START + 1) {
            rom_bank_0[i as usize] = bytes[i as usize];
        }
        Self {
            rom_bank_0,
            ext_ram: [0; (EXT_RAM_END - EXT_RAM_START + 1) as usize],
            vram: [0; (VRAM_END - VRAM_START + 1) as usize],
            wramI: [0; (WRAM_I_END - WRAM_I_START + 1) as usize],
            wramII: [0; (WRAM_II_END - WRAM_II_START + 1) as usize],
            io: IORegisters::default(),
            hram: [0; (HRAM_END - HRAM_START + 1) as usize],
            ie: 0,
        }
    }
}

impl MMU {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            // FIX: assuming a 32 kb cart
            ROM_BANK_0_START..=ROM_BANK_0_END => {
                self.rom_bank_0[(addr - ROM_BANK_0_START) as usize]
            }
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => unimplemented!("16kb switchable rom bank"),
            VRAM_START..=VRAM_END => self.vram[(addr - VRAM_START) as usize],
            EXT_RAM_START..=EXT_RAM_END => self.ext_ram[(addr - EXT_RAM_START) as usize],
            WRAM_I_START..=WRAM_I_END => self.wramI[(addr - WRAM_I_START) as usize],
            WRAM_II_START..=WRAM_II_END => self.wramII[(addr - WRAM_II_START) as usize],
            ECHO_RAM_START..=ECHO_RAM_END => panic!("Nintendo forbids reading from Echo RAM"),
            SPRITE_TABLE_START..=SPRITE_TABLE_END => unimplemented!("Sprite Attrib Memory"),
            FORBIDDEN_START..=FORBIDDEN_END => {
                panic!("Nintendo forbids reading from {FORBIDDEN_START}-{FORBIDDEN_END}")
            }
            IO_START..=IO_END => self.io.read_byte(addr),
            HRAM_START..=HRAM_END => self.hram[(addr - HRAM_START) as usize],
            INTERRUPT_START..=INTERRUPT_END => self.ie,
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            // FIX: assuming a 32 kb cart
            ROM_BANK_0_START..=ROM_BANK_0_END => {
                self.rom_bank_0[(addr - ROM_BANK_0_START) as usize] = val
            }
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => unimplemented!("16kb switchable rom bank"),
            VRAM_START..=VRAM_END => self.vram[(addr - VRAM_START) as usize] = val,
            EXT_RAM_START..=EXT_RAM_END => self.ext_ram[(addr - EXT_RAM_START) as usize] = val,
            WRAM_I_START..=WRAM_I_END => self.wramI[(addr - WRAM_I_START) as usize] = val,
            WRAM_II_START..=WRAM_II_END => self.wramII[(addr - WRAM_II_START) as usize] = val,
            ECHO_RAM_START..=ECHO_RAM_END => panic!("Nintendo forbids reading from Echo RAM"),
            SPRITE_TABLE_START..=SPRITE_TABLE_END => unimplemented!("Sprite Attrib Memory"),
            FORBIDDEN_START..=FORBIDDEN_END => {
                panic!("Nintendo forbids writing to {FORBIDDEN_START}-{FORBIDDEN_END}")
            }
            IO_START..=IO_END => self.io.set_byte(addr, val),
            HRAM_START..=HRAM_END => self.hram[(addr - HRAM_START) as usize] = val,
            INTERRUPT_START..=INTERRUPT_END => self.ie = val,
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        let lo: u16 = self.read_byte(addr).into();
        let hi: u16 = self.read_byte(addr + 1).into();
        (hi << 8) | lo
    }

    pub fn write_word(&mut self, addr: u16, val: u16) {
        let lo = (val & 0x00FF) as u8;
        let hi = ((val & 0xFF00) >> 8) as u8;
        self.write_byte(addr, lo);
        self.write_byte(addr + 1, hi);
    }
}

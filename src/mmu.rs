#![allow(non_snake_case)]

use crate::{boot_roms::DMG_BOOT_ROM, io_registers::IORegisters, mem_constants::*};

const BANK_0_SIZE: usize = (ROM_BANK_0_END - ROM_BANK_0_START + 1) as usize;
const SWITCHABLE_ROM_SIZE: usize = (SWITCHABLE_ROM_END - SWITCHABLE_ROM_START + 1) as usize;

#[derive(Debug)]
pub struct MMU {
    boot_rom_active: bool,
    boot_rom: [u8; 256],
    // ROM 0
    rom_bank_0: [u8; BANK_0_SIZE],
    // SWITCHABLE ROM
    switchable_rom: [u8; SWITCHABLE_ROM_SIZE],
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
        let mut rom_bank_0 = [0; BANK_0_SIZE];
        // TODO: this is messy
        let bytes = std::fs::read(rom).expect("Reading from ROM failed in MMU::new()");
        for i in 0x00..BANK_0_SIZE {
            rom_bank_0[i as usize] = bytes[i as usize];
        }

        // FIX: !!!! need to map back to rom from boot rom after it's done by modifying the BANK
        // register (0xFF50) - This is completely broken
        let boot_rom = DMG_BOOT_ROM.to_owned();

        let mut switchable_rom = [0; SWITCHABLE_ROM_SIZE];
        // TODO: this is messy
        for i in 0..SWITCHABLE_ROM_SIZE {
            switchable_rom[i as usize] = bytes[BANK_0_SIZE + (i as usize)];
        }
        Self {
            boot_rom_active: true,
            boot_rom,
            rom_bank_0,
            switchable_rom,
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
        // Gameboy doctor
        if addr == 0xFF44 {
            return 0x90;
        }

        match addr {
            // FIX: assuming a 32 kb cart
            ROM_BANK_0_START..=ROM_BANK_0_END => {
                if addr < 0x100 && self.boot_rom_active {
                    self.boot_rom[addr as usize]
                } else {
                    self.rom_bank_0[(addr - ROM_BANK_0_START) as usize]
                }
            }
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => {
                self.switchable_rom[(addr - SWITCHABLE_ROM_START) as usize]
            }
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
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => {
                self.switchable_rom[(addr - SWITCHABLE_ROM_START) as usize] = val
            }
            VRAM_START..=VRAM_END => {
                println!("wrote to vram: {addr:#02X}, {val}");
                self.vram[(addr - VRAM_START) as usize] = val;
            }
            EXT_RAM_START..=EXT_RAM_END => self.ext_ram[(addr - EXT_RAM_START) as usize] = val,
            WRAM_I_START..=WRAM_I_END => self.wramI[(addr - WRAM_I_START) as usize] = val,
            WRAM_II_START..=WRAM_II_END => self.wramII[(addr - WRAM_II_START) as usize] = val,
            ECHO_RAM_START..=ECHO_RAM_END => panic!("Nintendo forbids reading from Echo RAM"),
            SPRITE_TABLE_START..=SPRITE_TABLE_END => unimplemented!("Sprite Attrib Memory"),
            FORBIDDEN_START..=FORBIDDEN_END => {
                panic!("Nintendo forbids writing to {FORBIDDEN_START}-{FORBIDDEN_END}")
            }
            IO_START..=IO_END => {
                // XXX: how to handle weird cases where "writing" to a register is just a signal
                // that some state has changed?
                match addr {
                    // XXX: does this need to do anything with the 160 cycles?
                    0xFF46 => self.dma_transfer(val),
                    0xFF50 => {
                        self.boot_rom_active = false;
                        panic!("boot rom done");
                    }
                    _ => {}
                }

                self.io.set_byte(addr, val);
            }
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

    fn dma_transfer(&mut self, val: u8) {
        // Same as multiplying by 0x100 (256)
        let src: u16 = (val as u16) << 8;
        const OAM_START: u16 = 0xFE00;
        for i in 0..=0xDF {
            let ram_byte = self.read_byte(src + i);
            self.write_byte(OAM_START, ram_byte);
        }
    }
}

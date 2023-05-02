#![allow(non_snake_case)]

use crate::{
    boot_roms::DMG_BOOT_ROM, mem_constants::*, ppu::ppu::PPU, timer::Timer, traits::ReadWriteByte,
};

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
    ppu: PPU,
    timer: Timer,
    // HRAM
    hram: [u8; (HRAM_END - HRAM_START + 1) as usize],
    // Interrupt Flag (R/W)
    if_reg: u8,
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
            ppu: PPU::default(),
            timer: Timer::default(),
            wramI: [0; (WRAM_I_END - WRAM_I_START + 1) as usize],
            wramII: [0; (WRAM_II_END - WRAM_II_START + 1) as usize],
            hram: [0; (HRAM_END - HRAM_START + 1) as usize],
            if_reg: 0,
            ie: 0,
        }
    }
}

impl MMU {
    // FIX: there will be a variety of states updated by an MMU tick. This should not be a
    // bool. Instead, return interrupt vector
    pub fn tick(&mut self, cy: u32) -> bool {
        self.timer.tick(cy)
    }
}

impl ReadWriteByte for MMU {
    fn read(&self, addr: u16) -> u8 {
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
            IO_START..=IO_END => self.io_read(addr),
            HRAM_START..=HRAM_END => self.hram[(addr - HRAM_START) as usize],
            INTERRUPT_START..=INTERRUPT_END => self.ie,
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // FIX: assuming a 32 kb cart
            ROM_BANK_0_START..=ROM_BANK_0_END => {
                self.rom_bank_0[(addr - ROM_BANK_0_START) as usize] = val
            }
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => {
                self.switchable_rom[(addr - SWITCHABLE_ROM_START) as usize] = val
            }
            VRAM_START..=VRAM_END => {
                // println!("wrote to vram: {addr:#02X}, {val}");
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
            IO_START..=IO_END => self.io_write(addr, val),
            HRAM_START..=HRAM_END => self.hram[(addr - HRAM_START) as usize] = val,
            INTERRUPT_START..=INTERRUPT_END => self.ie = val,
        }
    }
}

impl MMU {
    fn io_read(&self, addr: u16) -> u8 {
        match addr {
            0xFF00 => todo!("Joypad"),
            0xFF01 | 0xFF02 => todo!("Serial Transfer"),
            0xFF04..=0xFF07 => self.timer.read(addr),
            0xFF0F => self.if_reg,
            0xFF11..=0xFF3F | 0xFF76 | 0xFF77 => todo!("Sound"),
            0xFF40..=0xFF55 | 0xFF68..=0xFF6C => self.ppu.read(addr),
            0xFF56 => todo!("IR Port"),
            0xFF70 => todo!("SVBK"),
            _ => panic!("Invalid address: {addr:#04X}"),
        }
    }

    fn io_write(&mut self, addr: u16, val: u8) {
        // XXX: how to handle weird cases where "writing" to a register is just a signal
        // that some state has changed?
        match addr {
            // XXX: does this need to do anything with the 160 cycles?
            0xFF46 => self.dma_transfer(val),
            0xFF50 => {
                self.boot_rom_active = false;
                // panic!("boot rom done");
                return;
            }
            _ => {}
        }

        match addr {
            0xFF00 => todo!("Joypad"),
            0xFF01 | 0xFF02 => {} // todo!("Serial Transfer"),
            0xFF04..=0xFF07 => self.timer.write(addr, val),
            0xFF0F => self.if_reg = val,
            0xFF11..=0xFF3F | 0xFF76 | 0xFF77 => {} //todo!("Sound"),
            0xFF40..=0xFF55 | 0xFF68..=0xFF6C => self.ppu.write(addr, val),
            0xFF56 => todo!("IR Port"),
            0xFF70 => todo!("SVBK"),
            _ => panic!("Invalid register address"),
        }
    }
}

impl MMU {
    pub fn read_word(&self, addr: u16) -> u16 {
        let lo: u16 = self.read(addr).into();
        let hi: u16 = self.read(addr + 1).into();
        (hi << 8) | lo
    }

    pub fn write_word(&mut self, addr: u16, val: u16) {
        let lo = (val & 0x00FF) as u8;
        let hi = ((val & 0xFF00) >> 8) as u8;
        self.write(addr, lo);
        self.write(addr + 1, hi);
    }

    fn dma_transfer(&mut self, val: u8) {
        // Same as multiplying by 0x100 (256)
        let src: u16 = (val as u16) << 8;
        const OAM_START: u16 = 0xFE00;
        for i in 0..=0xDF {
            let ram_byte = self.read(src + i);
            self.write(OAM_START, ram_byte);
        }
    }
}

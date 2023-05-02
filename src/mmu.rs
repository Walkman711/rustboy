#![allow(non_snake_case)]

use crate::{
    boot_roms::DMG_BOOT_ROM,
    io_registers::Interrupt,
    mem_constants::*,
    ppu::ppu::{VBlankStatus, PPU},
    timer::Timer,
    traits::ReadWriteByte,
};

const BANK_0_SIZE: usize = ROM_BANK_0_END - ROM_BANK_0_START + 1;
const SWITCHABLE_ROM_SIZE: usize = SWITCHABLE_ROM_END - SWITCHABLE_ROM_START + 1;

#[derive(Debug)]
pub struct MMU {
    boot_rom_active: bool,
    boot_rom: [u8; 256],
    rom_bank_0: [u8; BANK_0_SIZE],
    switchable_rom: [u8; SWITCHABLE_ROM_SIZE],
    // CGB: can switch banks 0/1 in CGB mode
    vram: [u8; VRAM_END - VRAM_START + 1],
    ext_ram: [u8; EXT_RAM_END - EXT_RAM_START + 1],
    wramI: [u8; WRAM_I_END - WRAM_I_START + 1],
    // CGB: can switch banks 1-7 in CGB mode
    wramII: [u8; WRAM_II_END - WRAM_II_START + 1],
    pub ppu: PPU,
    timer: Timer,
    hram: [u8; HRAM_END - HRAM_START + 1],
    // Interrupt Flag (IF)
    if_reg: u8,
    // Interrupt Enable Register (IE)
    ie: u8,
    vblank_status: VBlankStatus,
}

impl MMU {
    pub fn new(rom: &str) -> Self {
        let bytes = std::fs::read(rom).expect("Reading from ROM failed in MMU::new()");
        let mut rom_bank_0 = [0; BANK_0_SIZE];
        rom_bank_0[0..BANK_0_SIZE].copy_from_slice(&bytes[0..BANK_0_SIZE]);

        let boot_rom = DMG_BOOT_ROM.to_owned();

        let mut switchable_rom = [0; SWITCHABLE_ROM_SIZE];
        switchable_rom[0..SWITCHABLE_ROM_SIZE]
            .copy_from_slice(&bytes[BANK_0_SIZE..BANK_0_SIZE + SWITCHABLE_ROM_SIZE]);

        Self {
            boot_rom_active: true,
            boot_rom,
            rom_bank_0,
            switchable_rom,
            ext_ram: [0; EXT_RAM_END - EXT_RAM_START + 1],
            vram: [0; VRAM_END - VRAM_START + 1],
            ppu: PPU::default(),
            timer: Timer::default(),
            wramI: [0; WRAM_I_END - WRAM_I_START + 1],
            wramII: [0; WRAM_II_END - WRAM_II_START + 1],
            hram: [0; HRAM_END - HRAM_START + 1],
            if_reg: 0,
            ie: 0,
            vblank_status: VBlankStatus::Drawing,
        }
    }
}

impl MMU {
    pub fn tick(&mut self, cy: u32) -> Vec<Interrupt> {
        let mut interrupts = vec![];
        if self.timer.tick(cy) {
            interrupts.push(Interrupt::TimerOverflow);
        }

        self.vblank_status = self.ppu.tick(cy);
        if let VBlankStatus::VBlankRequested = self.vblank_status {
            interrupts.push(Interrupt::VBlank);
        }

        interrupts
    }
}

impl ReadWriteByte for MMU {
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        // Gameboy doctor
        // if addr == 0xFF44 {
        //     return 0x90;
        // }

        match addr {
            // FEATURE: assuming a 32 kb cart - need to implement other MBC types
            ROM_BANK_0_START..=ROM_BANK_0_END => {
                if addr < 0x100 && self.boot_rom_active {
                    self.boot_rom[addr]
                } else {
                    self.rom_bank_0[(addr - ROM_BANK_0_START)]
                }
            }
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => {
                self.switchable_rom[(addr - SWITCHABLE_ROM_START)]
            }
            VRAM_START..=VRAM_END => self.vram[(addr - VRAM_START)],
            EXT_RAM_START..=EXT_RAM_END => self.ext_ram[(addr - EXT_RAM_START)],
            WRAM_I_START..=WRAM_I_END => self.wramI[(addr - WRAM_I_START)],
            WRAM_II_START..=WRAM_II_END => self.wramII[(addr - WRAM_II_START)],
            ECHO_RAM_START..=ECHO_RAM_END => panic!("Nintendo forbids reading from Echo RAM"),
            SPRITE_TABLE_START..=SPRITE_TABLE_END => unimplemented!("Sprite Attrib Memory"),
            FORBIDDEN_START..=FORBIDDEN_END => {
                panic!("Nintendo forbids reading from {FORBIDDEN_START}-{FORBIDDEN_END}")
            }
            IO_START..=IO_END => self.io_read(addr as u16),
            HRAM_START..=HRAM_END => self.hram[(addr - HRAM_START)],
            INTERRUPT_START..=INTERRUPT_END => self.ie,
            _ => unreachable!("convert addr to usize in order to remove annoying casts"),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;
        match addr {
            ROM_BANK_0_START..=ROM_BANK_0_END => self.rom_bank_0[(addr - ROM_BANK_0_START)] = val,
            SWITCHABLE_ROM_START..=SWITCHABLE_ROM_END => {
                self.switchable_rom[(addr - SWITCHABLE_ROM_START)] = val
            }
            VRAM_START..=VRAM_END => {
                self.vram[(addr - VRAM_START)] = val;
            }
            EXT_RAM_START..=EXT_RAM_END => self.ext_ram[(addr - EXT_RAM_START)] = val,
            WRAM_I_START..=WRAM_I_END => self.wramI[(addr - WRAM_I_START)] = val,
            WRAM_II_START..=WRAM_II_END => self.wramII[(addr - WRAM_II_START)] = val,
            ECHO_RAM_START..=ECHO_RAM_END => panic!("Nintendo forbids reading from Echo RAM"),
            SPRITE_TABLE_START..=SPRITE_TABLE_END => unimplemented!("Sprite Attrib Memory"),
            FORBIDDEN_START..=FORBIDDEN_END => {
                panic!("Nintendo forbids writing to {FORBIDDEN_START}-{FORBIDDEN_END}")
            }
            IO_START..=IO_END => self.io_write(addr as u16, val),
            HRAM_START..=HRAM_END => self.hram[(addr - HRAM_START)] = val,
            INTERRUPT_START..=INTERRUPT_END => self.ie = val,
            _ => unreachable!("convert addr to usize in order to remove annoying casts"),
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
        match addr {
            // TODO: the mmu needs to inform the cpu when we spend cycles on transfers
            0xFF46 => self.dma_transfer(val),
            0xFF50 => {
                self.boot_rom_active = false;
                panic!("boot rom done");
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

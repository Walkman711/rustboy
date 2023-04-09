#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CartType {
    RomOnly = 0x00,
    Rom_Mbc1 = 0x01,
    Rom_Mbc1_Ram = 0x02,
    Rom_Mbc1_Ram_Batt = 0x03,
    Rom_Mbc2 = 0x05,
    Rom_Mbc2_Batt = 0x06,
    Rom_Ram = 0x08,
    Rom_Ram_Batt = 0x09,
    Rom_Mmm01 = 0x0B,
    Rom_Mmm01_Sram = 0x0C,
    Rom_Mmm01_Sram_Batt = 0x0D,
    Rom_Mbc3_Timer_Batt = 0x0F,
    Rom_Mbc3_Timer_Ram_Batt = 0x10,
    Rom_Mbc3 = 0x11,
    Rom_Mbc3_Ram = 0x12,
    Rom_Mbc3_Ram_Batt = 0x13,
    Rom_Mbc5 = 0x19,
    Rom_Mbc5_Ram = 0x1A,
    Rom_Mbc5_Ram_Batt = 0x1B,
    Rom_Mbc5_Rumble = 0x1C,
    Rom_Mbc5_Rumble_Sram = 0x1D,
    Rom_Mbc5_Rumble_Sram_Batt = 0x1E,
    PocketCamera = 0x1F,
    BandaiTama5 = 0xFD,
    HudsonHuC3 = 0xFE,
    HudsonHuC1 = 0xFF,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RomSize {
    ThirtyTwoKb = 0,
    SixtyFourKb = 1,
    OneTwentyEightKb = 2,
    TwoFiftySixKb = 3,
    FiveHundredTwelveKb = 4,
    OneMb = 5,
    TwoMb = 6,
    NineMBit = 0x52,
    TenMbit = 0x53,
    TwelveMbit = 0x54,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RamSize {
    None = 0,
    TwoKB = 1,
    EightKB = 2,
    ThirtyTwoKb = 3,
    OneTwentyEightKb = 4,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DestinationCode {
    Japanese = 0,
    NonJapanese = 1,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LicenseeCode {
    Check144 = 33,
    Accolade = 79,
    Konami = 0xA4,
}

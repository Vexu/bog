pub const zero_width_cf = [_]u21{
    0, // Null (Cc)
    0x034F, // Combining grapheme joiner (Mn)
    0x200B, // Zero width space
    0x200C, // Zero width non-joiner
    0x200D, // Zero width joiner
    0x200E, // Left-to-right mark
    0x200F, // Right-to-left mark
    0x2028, // Line separator (Zl)
    0x2029, // Paragraph separator (Zp)
    0x202A, // Left-to-right embedding
    0x202B, // Right-to-left embedding
    0x202C, // Pop directional formatting
    0x202D, // Left-to-right override
    0x202E, // Right-to-left override
    0x2060, // Word joiner
    0x2061, // Function application
    0x2062, // Invisible times
    0x2063, // Invisible separator
};

pub const zero_width = [_][2]u21{
    [2]u21{
        0x0300,
        0x036f,
    }, // Combining Grave Accent  ..Combining Latin Small Le
    [2]u21{
        0x0483,
        0x0489,
    }, // Combining Cyrillic Titlo..Combining Cyrillic Milli
    [2]u21{
        0x0591,
        0x05bd,
    }, // Hebrew Accent Etnahta   ..Hebrew Point Meteg
    [2]u21{
        0x05bf,
        0x05bf,
    }, // Hebrew Point Rafe       ..Hebrew Point Rafe
    [2]u21{
        0x05c1,
        0x05c2,
    }, // Hebrew Point Shin Dot   ..Hebrew Point Sin Dot
    [2]u21{
        0x05c4,
        0x05c5,
    }, // Hebrew Mark Upper Dot   ..Hebrew Mark Lower Dot
    [2]u21{
        0x05c7,
        0x05c7,
    }, // Hebrew Point Qamats Qata..Hebrew Point Qamats Qata
    [2]u21{
        0x0610,
        0x061a,
    }, // Arabic Sign Sallallahou ..Arabic Small Kasra
    [2]u21{
        0x064b,
        0x065f,
    }, // Arabic Fathatan         ..Arabic Wavy Hamza Below
    [2]u21{
        0x0670,
        0x0670,
    }, // Arabic Letter Superscrip..Arabic Letter Superscrip
    [2]u21{
        0x06d6,
        0x06dc,
    }, // Arabic Small High Ligatu..Arabic Small High Seen
    [2]u21{
        0x06df,
        0x06e4,
    }, // Arabic Small High Rounde..Arabic Small High Madda
    [2]u21{
        0x06e7,
        0x06e8,
    }, // Arabic Small High Yeh   ..Arabic Small High Noon
    [2]u21{
        0x06ea,
        0x06ed,
    }, // Arabic Empty Centre Low ..Arabic Small Low Meem
    [2]u21{
        0x0711,
        0x0711,
    }, // Syriac Letter Superscrip..Syriac Letter Superscrip
    [2]u21{
        0x0730,
        0x074a,
    }, // Syriac Pthaha Above     ..Syriac Barrekh
    [2]u21{
        0x07a6,
        0x07b0,
    }, // Thaana Abafili          ..Thaana Sukun
    [2]u21{
        0x07eb,
        0x07f3,
    }, // Nko Combining Short High..Nko Combining Double Dot
    [2]u21{
        0x07fd,
        0x07fd,
    }, // Nko Dantayalan          ..Nko Dantayalan
    [2]u21{
        0x0816,
        0x0819,
    }, // Samaritan Mark In       ..Samaritan Mark Dagesh
    [2]u21{
        0x081b,
        0x0823,
    }, // Samaritan Mark Epentheti..Samaritan Vowel Sign A
    [2]u21{
        0x0825,
        0x0827,
    }, // Samaritan Vowel Sign Sho..Samaritan Vowel Sign U
    [2]u21{
        0x0829,
        0x082d,
    }, // Samaritan Vowel Sign Lon..Samaritan Mark Nequdaa
    [2]u21{
        0x0859,
        0x085b,
    }, // Mandaic Affrication Mark..Mandaic Gemination Mark
    [2]u21{
        0x08d3,
        0x08e1,
    }, // Arabic Small Low Waw    ..Arabic Small High Sign S
    [2]u21{
        0x08e3,
        0x0902,
    }, // Arabic Turned Damma Belo..Devanagari Sign Anusvara
    [2]u21{
        0x093a,
        0x093a,
    }, // Devanagari Vowel Sign Oe..Devanagari Vowel Sign Oe
    [2]u21{
        0x093c,
        0x093c,
    }, // Devanagari Sign Nukta   ..Devanagari Sign Nukta
    [2]u21{
        0x0941,
        0x0948,
    }, // Devanagari Vowel Sign U ..Devanagari Vowel Sign Ai
    [2]u21{
        0x094d,
        0x094d,
    }, // Devanagari Sign Virama  ..Devanagari Sign Virama
    [2]u21{
        0x0951,
        0x0957,
    }, // Devanagari Stress Sign U..Devanagari Vowel Sign Uu
    [2]u21{
        0x0962,
        0x0963,
    }, // Devanagari Vowel Sign Vo..Devanagari Vowel Sign Vo
    [2]u21{
        0x0981,
        0x0981,
    }, // Bengali Sign Candrabindu..Bengali Sign Candrabindu
    [2]u21{
        0x09bc,
        0x09bc,
    }, // Bengali Sign Nukta      ..Bengali Sign Nukta
    [2]u21{
        0x09c1,
        0x09c4,
    }, // Bengali Vowel Sign U    ..Bengali Vowel Sign Vocal
    [2]u21{
        0x09cd,
        0x09cd,
    }, // Bengali Sign Virama     ..Bengali Sign Virama
    [2]u21{
        0x09e2,
        0x09e3,
    }, // Bengali Vowel Sign Vocal..Bengali Vowel Sign Vocal
    [2]u21{
        0x09fe,
        0x09fe,
    }, // Bengali Sandhi Mark     ..Bengali Sandhi Mark
    [2]u21{
        0x0a01,
        0x0a02,
    }, // Gurmukhi Sign Adak Bindi..Gurmukhi Sign Bindi
    [2]u21{
        0x0a3c,
        0x0a3c,
    }, // Gurmukhi Sign Nukta     ..Gurmukhi Sign Nukta
    [2]u21{
        0x0a41,
        0x0a42,
    }, // Gurmukhi Vowel Sign U   ..Gurmukhi Vowel Sign Uu
    [2]u21{
        0x0a47,
        0x0a48,
    }, // Gurmukhi Vowel Sign Ee  ..Gurmukhi Vowel Sign Ai
    [2]u21{
        0x0a4b,
        0x0a4d,
    }, // Gurmukhi Vowel Sign Oo  ..Gurmukhi Sign Virama
    [2]u21{
        0x0a51,
        0x0a51,
    }, // Gurmukhi Sign Udaat     ..Gurmukhi Sign Udaat
    [2]u21{
        0x0a70,
        0x0a71,
    }, // Gurmukhi Tippi          ..Gurmukhi Addak
    [2]u21{
        0x0a75,
        0x0a75,
    }, // Gurmukhi Sign Yakash    ..Gurmukhi Sign Yakash
    [2]u21{
        0x0a81,
        0x0a82,
    }, // Gujarati Sign Candrabind..Gujarati Sign Anusvara
    [2]u21{
        0x0abc,
        0x0abc,
    }, // Gujarati Sign Nukta     ..Gujarati Sign Nukta
    [2]u21{
        0x0ac1,
        0x0ac5,
    }, // Gujarati Vowel Sign U   ..Gujarati Vowel Sign Cand
    [2]u21{
        0x0ac7,
        0x0ac8,
    }, // Gujarati Vowel Sign E   ..Gujarati Vowel Sign Ai
    [2]u21{
        0x0acd,
        0x0acd,
    }, // Gujarati Sign Virama    ..Gujarati Sign Virama
    [2]u21{
        0x0ae2,
        0x0ae3,
    }, // Gujarati Vowel Sign Voca..Gujarati Vowel Sign Voca
    [2]u21{
        0x0afa,
        0x0aff,
    }, // Gujarati Sign Sukun     ..Gujarati Sign Two-circle
    [2]u21{
        0x0b01,
        0x0b01,
    }, // Oriya Sign Candrabindu  ..Oriya Sign Candrabindu
    [2]u21{
        0x0b3c,
        0x0b3c,
    }, // Oriya Sign Nukta        ..Oriya Sign Nukta
    [2]u21{
        0x0b3f,
        0x0b3f,
    }, // Oriya Vowel Sign I      ..Oriya Vowel Sign I
    [2]u21{
        0x0b41,
        0x0b44,
    }, // Oriya Vowel Sign U      ..Oriya Vowel Sign Vocalic
    [2]u21{
        0x0b4d,
        0x0b4d,
    }, // Oriya Sign Virama       ..Oriya Sign Virama
    [2]u21{
        0x0b55,
        0x0b56,
    }, // [2]u21{ nil }                   ..Oriya Ai Length Mark
    [2]u21{
        0x0b62,
        0x0b63,
    }, // Oriya Vowel Sign Vocalic..Oriya Vowel Sign Vocalic
    [2]u21{
        0x0b82,
        0x0b82,
    }, // Tamil Sign Anusvara     ..Tamil Sign Anusvara
    [2]u21{
        0x0bc0,
        0x0bc0,
    }, // Tamil Vowel Sign Ii     ..Tamil Vowel Sign Ii
    [2]u21{
        0x0bcd,
        0x0bcd,
    }, // Tamil Sign Virama       ..Tamil Sign Virama
    [2]u21{
        0x0c00,
        0x0c00,
    }, // Telugu Sign Combining Ca..Telugu Sign Combining Ca
    [2]u21{
        0x0c04,
        0x0c04,
    }, // Telugu Sign Combining An..Telugu Sign Combining An
    [2]u21{
        0x0c3e,
        0x0c40,
    }, // Telugu Vowel Sign Aa    ..Telugu Vowel Sign Ii
    [2]u21{
        0x0c46,
        0x0c48,
    }, // Telugu Vowel Sign E     ..Telugu Vowel Sign Ai
    [2]u21{
        0x0c4a,
        0x0c4d,
    }, // Telugu Vowel Sign O     ..Telugu Sign Virama
    [2]u21{
        0x0c55,
        0x0c56,
    }, // Telugu Length Mark      ..Telugu Ai Length Mark
    [2]u21{
        0x0c62,
        0x0c63,
    }, // Telugu Vowel Sign Vocali..Telugu Vowel Sign Vocali
    [2]u21{
        0x0c81,
        0x0c81,
    }, // Kannada Sign Candrabindu..Kannada Sign Candrabindu
    [2]u21{
        0x0cbc,
        0x0cbc,
    }, // Kannada Sign Nukta      ..Kannada Sign Nukta
    [2]u21{
        0x0cbf,
        0x0cbf,
    }, // Kannada Vowel Sign I    ..Kannada Vowel Sign I
    [2]u21{
        0x0cc6,
        0x0cc6,
    }, // Kannada Vowel Sign E    ..Kannada Vowel Sign E
    [2]u21{
        0x0ccc,
        0x0ccd,
    }, // Kannada Vowel Sign Au   ..Kannada Sign Virama
    [2]u21{
        0x0ce2,
        0x0ce3,
    }, // Kannada Vowel Sign Vocal..Kannada Vowel Sign Vocal
    [2]u21{
        0x0d00,
        0x0d01,
    }, // Malayalam Sign Combining..Malayalam Sign Candrabin
    [2]u21{
        0x0d3b,
        0x0d3c,
    }, // Malayalam Sign Vertical ..Malayalam Sign Circular
    [2]u21{
        0x0d41,
        0x0d44,
    }, // Malayalam Vowel Sign U  ..Malayalam Vowel Sign Voc
    [2]u21{
        0x0d4d,
        0x0d4d,
    }, // Malayalam Sign Virama   ..Malayalam Sign Virama
    [2]u21{
        0x0d62,
        0x0d63,
    }, // Malayalam Vowel Sign Voc..Malayalam Vowel Sign Voc
    [2]u21{
        0x0d81,
        0x0d81,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x0dca,
        0x0dca,
    }, // Sinhala Sign Al-lakuna  ..Sinhala Sign Al-lakuna
    [2]u21{
        0x0dd2,
        0x0dd4,
    }, // Sinhala Vowel Sign Ketti..Sinhala Vowel Sign Ketti
    [2]u21{
        0x0dd6,
        0x0dd6,
    }, // Sinhala Vowel Sign Diga ..Sinhala Vowel Sign Diga
    [2]u21{
        0x0e31,
        0x0e31,
    }, // Thai Character Mai Han-a..Thai Character Mai Han-a
    [2]u21{
        0x0e34,
        0x0e3a,
    }, // Thai Character Sara I   ..Thai Character Phinthu
    [2]u21{
        0x0e47,
        0x0e4e,
    }, // Thai Character Maitaikhu..Thai Character Yamakkan
    [2]u21{
        0x0eb1,
        0x0eb1,
    }, // Lao Vowel Sign Mai Kan  ..Lao Vowel Sign Mai Kan
    [2]u21{
        0x0eb4,
        0x0ebc,
    }, // Lao Vowel Sign I        ..Lao Semivowel Sign Lo
    [2]u21{
        0x0ec8,
        0x0ecd,
    }, // Lao Tone Mai Ek         ..Lao Niggahita
    [2]u21{
        0x0f18,
        0x0f19,
    }, // Tibetan Astrological Sig..Tibetan Astrological Sig
    [2]u21{
        0x0f35,
        0x0f35,
    }, // Tibetan Mark Ngas Bzung ..Tibetan Mark Ngas Bzung
    [2]u21{
        0x0f37,
        0x0f37,
    }, // Tibetan Mark Ngas Bzung ..Tibetan Mark Ngas Bzung
    [2]u21{
        0x0f39,
        0x0f39,
    }, // Tibetan Mark Tsa -phru  ..Tibetan Mark Tsa -phru
    [2]u21{
        0x0f71,
        0x0f7e,
    }, // Tibetan Vowel Sign Aa   ..Tibetan Sign Rjes Su Nga
    [2]u21{
        0x0f80,
        0x0f84,
    }, // Tibetan Vowel Sign Rever..Tibetan Mark Halanta
    [2]u21{
        0x0f86,
        0x0f87,
    }, // Tibetan Sign Lci Rtags  ..Tibetan Sign Yang Rtags
    [2]u21{
        0x0f8d,
        0x0f97,
    }, // Tibetan Subjoined Sign L..Tibetan Subjoined Letter
    [2]u21{
        0x0f99,
        0x0fbc,
    }, // Tibetan Subjoined Letter..Tibetan Subjoined Letter
    [2]u21{
        0x0fc6,
        0x0fc6,
    }, // Tibetan Symbol Padma Gda..Tibetan Symbol Padma Gda
    [2]u21{
        0x102d,
        0x1030,
    }, // Myanmar Vowel Sign I    ..Myanmar Vowel Sign Uu
    [2]u21{
        0x1032,
        0x1037,
    }, // Myanmar Vowel Sign Ai   ..Myanmar Sign Dot Below
    [2]u21{
        0x1039,
        0x103a,
    }, // Myanmar Sign Virama     ..Myanmar Sign Asat
    [2]u21{
        0x103d,
        0x103e,
    }, // Myanmar Consonant Sign M..Myanmar Consonant Sign M
    [2]u21{
        0x1058,
        0x1059,
    }, // Myanmar Vowel Sign Vocal..Myanmar Vowel Sign Vocal
    [2]u21{
        0x105e,
        0x1060,
    }, // Myanmar Consonant Sign M..Myanmar Consonant Sign M
    [2]u21{
        0x1071,
        0x1074,
    }, // Myanmar Vowel Sign Geba ..Myanmar Vowel Sign Kayah
    [2]u21{
        0x1082,
        0x1082,
    }, // Myanmar Consonant Sign S..Myanmar Consonant Sign S
    [2]u21{
        0x1085,
        0x1086,
    }, // Myanmar Vowel Sign Shan ..Myanmar Vowel Sign Shan
    [2]u21{
        0x108d,
        0x108d,
    }, // Myanmar Sign Shan Counci..Myanmar Sign Shan Counci
    [2]u21{
        0x109d,
        0x109d,
    }, // Myanmar Vowel Sign Aiton..Myanmar Vowel Sign Aiton
    [2]u21{
        0x135d,
        0x135f,
    }, // Ethiopic Combining Gemin..Ethiopic Combining Gemin
    [2]u21{
        0x1712,
        0x1714,
    }, // Tagalog Vowel Sign I    ..Tagalog Sign Virama
    [2]u21{
        0x1732,
        0x1734,
    }, // Hanunoo Vowel Sign I    ..Hanunoo Sign Pamudpod
    [2]u21{
        0x1752,
        0x1753,
    }, // Buhid Vowel Sign I      ..Buhid Vowel Sign U
    [2]u21{
        0x1772,
        0x1773,
    }, // Tagbanwa Vowel Sign I   ..Tagbanwa Vowel Sign U
    [2]u21{
        0x17b4,
        0x17b5,
    }, // Khmer Vowel Inherent Aq ..Khmer Vowel Inherent Aa
    [2]u21{
        0x17b7,
        0x17bd,
    }, // Khmer Vowel Sign I      ..Khmer Vowel Sign Ua
    [2]u21{
        0x17c6,
        0x17c6,
    }, // Khmer Sign Nikahit      ..Khmer Sign Nikahit
    [2]u21{
        0x17c9,
        0x17d3,
    }, // Khmer Sign Muusikatoan  ..Khmer Sign Bathamasat
    [2]u21{
        0x17dd,
        0x17dd,
    }, // Khmer Sign Atthacan     ..Khmer Sign Atthacan
    [2]u21{
        0x180b,
        0x180d,
    }, // Mongolian Free Variation..Mongolian Free Variation
    [2]u21{
        0x1885,
        0x1886,
    }, // Mongolian Letter Ali Gal..Mongolian Letter Ali Gal
    [2]u21{
        0x18a9,
        0x18a9,
    }, // Mongolian Letter Ali Gal..Mongolian Letter Ali Gal
    [2]u21{
        0x1920,
        0x1922,
    }, // Limbu Vowel Sign A      ..Limbu Vowel Sign U
    [2]u21{
        0x1927,
        0x1928,
    }, // Limbu Vowel Sign E      ..Limbu Vowel Sign O
    [2]u21{
        0x1932,
        0x1932,
    }, // Limbu Small Letter Anusv..Limbu Small Letter Anusv
    [2]u21{
        0x1939,
        0x193b,
    }, // Limbu Sign Mukphreng    ..Limbu Sign Sa-i
    [2]u21{
        0x1a17,
        0x1a18,
    }, // Buginese Vowel Sign I   ..Buginese Vowel Sign U
    [2]u21{
        0x1a1b,
        0x1a1b,
    }, // Buginese Vowel Sign Ae  ..Buginese Vowel Sign Ae
    [2]u21{
        0x1a56,
        0x1a56,
    }, // Tai Tham Consonant Sign ..Tai Tham Consonant Sign
    [2]u21{
        0x1a58,
        0x1a5e,
    }, // Tai Tham Sign Mai Kang L..Tai Tham Consonant Sign
    [2]u21{
        0x1a60,
        0x1a60,
    }, // Tai Tham Sign Sakot     ..Tai Tham Sign Sakot
    [2]u21{
        0x1a62,
        0x1a62,
    }, // Tai Tham Vowel Sign Mai ..Tai Tham Vowel Sign Mai
    [2]u21{
        0x1a65,
        0x1a6c,
    }, // Tai Tham Vowel Sign I   ..Tai Tham Vowel Sign Oa B
    [2]u21{
        0x1a73,
        0x1a7c,
    }, // Tai Tham Vowel Sign Oa A..Tai Tham Sign Khuen-lue
    [2]u21{
        0x1a7f,
        0x1a7f,
    }, // Tai Tham Combining Crypt..Tai Tham Combining Crypt
    [2]u21{
        0x1ab0,
        0x1ac0,
    }, // Combining Doubled Circum..
    [2]u21{
        0x1b00,
        0x1b03,
    }, // Balinese Sign Ulu Ricem ..Balinese Sign Surang
    [2]u21{
        0x1b34,
        0x1b34,
    }, // Balinese Sign Rerekan   ..Balinese Sign Rerekan
    [2]u21{
        0x1b36,
        0x1b3a,
    }, // Balinese Vowel Sign Ulu ..Balinese Vowel Sign Ra R
    [2]u21{
        0x1b3c,
        0x1b3c,
    }, // Balinese Vowel Sign La L..Balinese Vowel Sign La L
    [2]u21{
        0x1b42,
        0x1b42,
    }, // Balinese Vowel Sign Pepe..Balinese Vowel Sign Pepe
    [2]u21{
        0x1b6b,
        0x1b73,
    }, // Balinese Musical Symbol ..Balinese Musical Symbol
    [2]u21{
        0x1b80,
        0x1b81,
    }, // Sundanese Sign Panyecek ..Sundanese Sign Panglayar
    [2]u21{
        0x1ba2,
        0x1ba5,
    }, // Sundanese Consonant Sign..Sundanese Vowel Sign Pan
    [2]u21{
        0x1ba8,
        0x1ba9,
    }, // Sundanese Vowel Sign Pam..Sundanese Vowel Sign Pan
    [2]u21{
        0x1bab,
        0x1bad,
    }, // Sundanese Sign Virama   ..Sundanese Consonant Sign
    [2]u21{
        0x1be6,
        0x1be6,
    }, // Batak Sign Tompi        ..Batak Sign Tompi
    [2]u21{
        0x1be8,
        0x1be9,
    }, // Batak Vowel Sign Pakpak ..Batak Vowel Sign Ee
    [2]u21{
        0x1bed,
        0x1bed,
    }, // Batak Vowel Sign Karo O ..Batak Vowel Sign Karo O
    [2]u21{
        0x1bef,
        0x1bf1,
    }, // Batak Vowel Sign U For S..Batak Consonant Sign H
    [2]u21{
        0x1c2c,
        0x1c33,
    }, // Lepcha Vowel Sign E     ..Lepcha Consonant Sign T
    [2]u21{
        0x1c36,
        0x1c37,
    }, // Lepcha Sign Ran         ..Lepcha Sign Nukta
    [2]u21{
        0x1cd0,
        0x1cd2,
    }, // Vedic Tone Karshana     ..Vedic Tone Prenkha
    [2]u21{
        0x1cd4,
        0x1ce0,
    }, // Vedic Sign Yajurvedic Mi..Vedic Tone Rigvedic Kash
    [2]u21{
        0x1ce2,
        0x1ce8,
    }, // Vedic Sign Visarga Svari..Vedic Sign Visarga Anuda
    [2]u21{
        0x1ced,
        0x1ced,
    }, // Vedic Sign Tiryak       ..Vedic Sign Tiryak
    [2]u21{
        0x1cf4,
        0x1cf4,
    }, // Vedic Tone Candra Above ..Vedic Tone Candra Above
    [2]u21{
        0x1cf8,
        0x1cf9,
    }, // Vedic Tone Ring Above   ..Vedic Tone Double Ring A
    [2]u21{
        0x1dc0,
        0x1df9,
    }, // Combining Dotted Grave A..Combining Wide Inverted
    [2]u21{
        0x1dfb,
        0x1dff,
    }, // Combining Deletion Mark ..Combining Right Arrowhea
    [2]u21{
        0x20d0,
        0x20f0,
    }, // Combining Left Harpoon A..Combining Asterisk Above
    [2]u21{
        0x2cef,
        0x2cf1,
    }, // Coptic Combining Ni Abov..Coptic Combining Spiritu
    [2]u21{
        0x2d7f,
        0x2d7f,
    }, // Tifinagh Consonant Joine..Tifinagh Consonant Joine
    [2]u21{
        0x2de0,
        0x2dff,
    }, // Combining Cyrillic Lette..Combining Cyrillic Lette
    [2]u21{
        0x302a,
        0x302d,
    }, // Ideographic Level Tone M..Ideographic Entering Ton
    [2]u21{
        0x3099,
        0x309a,
    }, // Combining Katakana-hirag..Combining Katakana-hirag
    [2]u21{
        0xa66f,
        0xa672,
    }, // Combining Cyrillic Vzmet..Combining Cyrillic Thous
    [2]u21{
        0xa674,
        0xa67d,
    }, // Combining Cyrillic Lette..Combining Cyrillic Payer
    [2]u21{
        0xa69e,
        0xa69f,
    }, // Combining Cyrillic Lette..Combining Cyrillic Lette
    [2]u21{
        0xa6f0,
        0xa6f1,
    }, // Bamum Combining Mark Koq..Bamum Combining Mark Tuk
    [2]u21{
        0xa802,
        0xa802,
    }, // Syloti Nagri Sign Dvisva..Syloti Nagri Sign Dvisva
    [2]u21{
        0xa806,
        0xa806,
    }, // Syloti Nagri Sign Hasant..Syloti Nagri Sign Hasant
    [2]u21{
        0xa80b,
        0xa80b,
    }, // Syloti Nagri Sign Anusva..Syloti Nagri Sign Anusva
    [2]u21{
        0xa825,
        0xa826,
    }, // Syloti Nagri Vowel Sign ..Syloti Nagri Vowel Sign
    [2]u21{
        0xa82c,
        0xa82c,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0xa8c4,
        0xa8c5,
    }, // Saurashtra Sign Virama  ..Saurashtra Sign Candrabi
    [2]u21{
        0xa8e0,
        0xa8f1,
    }, // Combining Devanagari Dig..Combining Devanagari Sig
    [2]u21{
        0xa8ff,
        0xa8ff,
    }, // Devanagari Vowel Sign Ay..Devanagari Vowel Sign Ay
    [2]u21{
        0xa926,
        0xa92d,
    }, // Kayah Li Vowel Ue       ..Kayah Li Tone Calya Plop
    [2]u21{
        0xa947,
        0xa951,
    }, // Rejang Vowel Sign I     ..Rejang Consonant Sign R
    [2]u21{
        0xa980,
        0xa982,
    }, // Javanese Sign Panyangga ..Javanese Sign Layar
    [2]u21{
        0xa9b3,
        0xa9b3,
    }, // Javanese Sign Cecak Telu..Javanese Sign Cecak Telu
    [2]u21{
        0xa9b6,
        0xa9b9,
    }, // Javanese Vowel Sign Wulu..Javanese Vowel Sign Suku
    [2]u21{
        0xa9bc,
        0xa9bd,
    }, // Javanese Vowel Sign Pepe..Javanese Consonant Sign
    [2]u21{
        0xa9e5,
        0xa9e5,
    }, // Myanmar Sign Shan Saw   ..Myanmar Sign Shan Saw
    [2]u21{
        0xaa29,
        0xaa2e,
    }, // Cham Vowel Sign Aa      ..Cham Vowel Sign Oe
    [2]u21{
        0xaa31,
        0xaa32,
    }, // Cham Vowel Sign Au      ..Cham Vowel Sign Ue
    [2]u21{
        0xaa35,
        0xaa36,
    }, // Cham Consonant Sign La  ..Cham Consonant Sign Wa
    [2]u21{
        0xaa43,
        0xaa43,
    }, // Cham Consonant Sign Fina..Cham Consonant Sign Fina
    [2]u21{
        0xaa4c,
        0xaa4c,
    }, // Cham Consonant Sign Fina..Cham Consonant Sign Fina
    [2]u21{
        0xaa7c,
        0xaa7c,
    }, // Myanmar Sign Tai Laing T..Myanmar Sign Tai Laing T
    [2]u21{
        0xaab0,
        0xaab0,
    }, // Tai Viet Mai Kang       ..Tai Viet Mai Kang
    [2]u21{
        0xaab2,
        0xaab4,
    }, // Tai Viet Vowel I        ..Tai Viet Vowel U
    [2]u21{
        0xaab7,
        0xaab8,
    }, // Tai Viet Mai Khit       ..Tai Viet Vowel Ia
    [2]u21{
        0xaabe,
        0xaabf,
    }, // Tai Viet Vowel Am       ..Tai Viet Tone Mai Ek
    [2]u21{
        0xaac1,
        0xaac1,
    }, // Tai Viet Tone Mai Tho   ..Tai Viet Tone Mai Tho
    [2]u21{
        0xaaec,
        0xaaed,
    }, // Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
    [2]u21{
        0xaaf6,
        0xaaf6,
    }, // Meetei Mayek Virama     ..Meetei Mayek Virama
    [2]u21{
        0xabe5,
        0xabe5,
    }, // Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
    [2]u21{
        0xabe8,
        0xabe8,
    }, // Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign
    [2]u21{
        0xabed,
        0xabed,
    }, // Meetei Mayek Apun Iyek  ..Meetei Mayek Apun Iyek
    [2]u21{
        0xfb1e,
        0xfb1e,
    }, // Hebrew Point Judeo-spani..Hebrew Point Judeo-spani
    [2]u21{
        0xfe00,
        0xfe0f,
    }, // Variation Selector-1    ..Variation Selector-16
    [2]u21{
        0xfe20,
        0xfe2f,
    }, // Combining Ligature Left ..Combining Cyrillic Titlo
    [2]u21{
        0x101fd,
        0x101fd,
    }, // Phaistos Disc Sign Combi..Phaistos Disc Sign Combi
    [2]u21{
        0x102e0,
        0x102e0,
    }, // Coptic Epact Thousands M..Coptic Epact Thousands M
    [2]u21{
        0x10376,
        0x1037a,
    }, // Combining Old Permic Let..Combining Old Permic Let
    [2]u21{
        0x10a01,
        0x10a03,
    }, // Kharoshthi Vowel Sign I ..Kharoshthi Vowel Sign Vo
    [2]u21{
        0x10a05,
        0x10a06,
    }, // Kharoshthi Vowel Sign E ..Kharoshthi Vowel Sign O
    [2]u21{
        0x10a0c,
        0x10a0f,
    }, // Kharoshthi Vowel Length ..Kharoshthi Sign Visarga
    [2]u21{
        0x10a38,
        0x10a3a,
    }, // Kharoshthi Sign Bar Abov..Kharoshthi Sign Dot Belo
    [2]u21{
        0x10a3f,
        0x10a3f,
    }, // Kharoshthi Virama       ..Kharoshthi Virama
    [2]u21{
        0x10ae5,
        0x10ae6,
    }, // Manichaean Abbreviation ..Manichaean Abbreviation
    [2]u21{
        0x10d24,
        0x10d27,
    }, // Hanifi Rohingya Sign Har..Hanifi Rohingya Sign Tas
    [2]u21{
        0x10eab,
        0x10eac,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x10f46,
        0x10f50,
    }, // Sogdian Combining Dot Be..Sogdian Combining Stroke
    [2]u21{
        0x11001,
        0x11001,
    }, // Brahmi Sign Anusvara    ..Brahmi Sign Anusvara
    [2]u21{
        0x11038,
        0x11046,
    }, // Brahmi Vowel Sign Aa    ..Brahmi Virama
    [2]u21{
        0x1107f,
        0x11081,
    }, // Brahmi Number Joiner    ..Kaithi Sign Anusvara
    [2]u21{
        0x110b3,
        0x110b6,
    }, // Kaithi Vowel Sign U     ..Kaithi Vowel Sign Ai
    [2]u21{
        0x110b9,
        0x110ba,
    }, // Kaithi Sign Virama      ..Kaithi Sign Nukta
    [2]u21{
        0x11100,
        0x11102,
    }, // Chakma Sign Candrabindu ..Chakma Sign Visarga
    [2]u21{
        0x11127,
        0x1112b,
    }, // Chakma Vowel Sign A     ..Chakma Vowel Sign Uu
    [2]u21{
        0x1112d,
        0x11134,
    }, // Chakma Vowel Sign Ai    ..Chakma Maayyaa
    [2]u21{
        0x11173,
        0x11173,
    }, // Mahajani Sign Nukta     ..Mahajani Sign Nukta
    [2]u21{
        0x11180,
        0x11181,
    }, // Sharada Sign Candrabindu..Sharada Sign Anusvara
    [2]u21{
        0x111b6,
        0x111be,
    }, // Sharada Vowel Sign U    ..Sharada Vowel Sign O
    [2]u21{
        0x111c9,
        0x111cc,
    }, // Sharada Sandhi Mark     ..Sharada Extra Short Vowe
    [2]u21{
        0x111cf,
        0x111cf,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x1122f,
        0x11231,
    }, // Khojki Vowel Sign U     ..Khojki Vowel Sign Ai
    [2]u21{
        0x11234,
        0x11234,
    }, // Khojki Sign Anusvara    ..Khojki Sign Anusvara
    [2]u21{
        0x11236,
        0x11237,
    }, // Khojki Sign Nukta       ..Khojki Sign Shadda
    [2]u21{
        0x1123e,
        0x1123e,
    }, // Khojki Sign Sukun       ..Khojki Sign Sukun
    [2]u21{
        0x112df,
        0x112df,
    }, // Khudawadi Sign Anusvara ..Khudawadi Sign Anusvara
    [2]u21{
        0x112e3,
        0x112ea,
    }, // Khudawadi Vowel Sign U  ..Khudawadi Sign Virama
    [2]u21{
        0x11300,
        0x11301,
    }, // Grantha Sign Combining A..Grantha Sign Candrabindu
    [2]u21{
        0x1133b,
        0x1133c,
    }, // Combining Bindu Below   ..Grantha Sign Nukta
    [2]u21{
        0x11340,
        0x11340,
    }, // Grantha Vowel Sign Ii   ..Grantha Vowel Sign Ii
    [2]u21{
        0x11366,
        0x1136c,
    }, // Combining Grantha Digit ..Combining Grantha Digit
    [2]u21{
        0x11370,
        0x11374,
    }, // Combining Grantha Letter..Combining Grantha Letter
    [2]u21{
        0x11438,
        0x1143f,
    }, // Newa Vowel Sign U       ..Newa Vowel Sign Ai
    [2]u21{
        0x11442,
        0x11444,
    }, // Newa Sign Virama        ..Newa Sign Anusvara
    [2]u21{
        0x11446,
        0x11446,
    }, // Newa Sign Nukta         ..Newa Sign Nukta
    [2]u21{
        0x1145e,
        0x1145e,
    }, // Newa Sandhi Mark        ..Newa Sandhi Mark
    [2]u21{
        0x114b3,
        0x114b8,
    }, // Tirhuta Vowel Sign U    ..Tirhuta Vowel Sign Vocal
    [2]u21{
        0x114ba,
        0x114ba,
    }, // Tirhuta Vowel Sign Short..Tirhuta Vowel Sign Short
    [2]u21{
        0x114bf,
        0x114c0,
    }, // Tirhuta Sign Candrabindu..Tirhuta Sign Anusvara
    [2]u21{
        0x114c2,
        0x114c3,
    }, // Tirhuta Sign Virama     ..Tirhuta Sign Nukta
    [2]u21{
        0x115b2,
        0x115b5,
    }, // Siddham Vowel Sign U    ..Siddham Vowel Sign Vocal
    [2]u21{
        0x115bc,
        0x115bd,
    }, // Siddham Sign Candrabindu..Siddham Sign Anusvara
    [2]u21{
        0x115bf,
        0x115c0,
    }, // Siddham Sign Virama     ..Siddham Sign Nukta
    [2]u21{
        0x115dc,
        0x115dd,
    }, // Siddham Vowel Sign Alter..Siddham Vowel Sign Alter
    [2]u21{
        0x11633,
        0x1163a,
    }, // Modi Vowel Sign U       ..Modi Vowel Sign Ai
    [2]u21{
        0x1163d,
        0x1163d,
    }, // Modi Sign Anusvara      ..Modi Sign Anusvara
    [2]u21{
        0x1163f,
        0x11640,
    }, // Modi Sign Virama        ..Modi Sign Ardhacandra
    [2]u21{
        0x116ab,
        0x116ab,
    }, // Takri Sign Anusvara     ..Takri Sign Anusvara
    [2]u21{
        0x116ad,
        0x116ad,
    }, // Takri Vowel Sign Aa     ..Takri Vowel Sign Aa
    [2]u21{
        0x116b0,
        0x116b5,
    }, // Takri Vowel Sign U      ..Takri Vowel Sign Au
    [2]u21{
        0x116b7,
        0x116b7,
    }, // Takri Sign Nukta        ..Takri Sign Nukta
    [2]u21{
        0x1171d,
        0x1171f,
    }, // Ahom Consonant Sign Medi..Ahom Consonant Sign Medi
    [2]u21{
        0x11722,
        0x11725,
    }, // Ahom Vowel Sign I       ..Ahom Vowel Sign Uu
    [2]u21{
        0x11727,
        0x1172b,
    }, // Ahom Vowel Sign Aw      ..Ahom Sign Killer
    [2]u21{
        0x1182f,
        0x11837,
    }, // Dogra Vowel Sign U      ..Dogra Sign Anusvara
    [2]u21{
        0x11839,
        0x1183a,
    }, // Dogra Sign Virama       ..Dogra Sign Nukta
    [2]u21{
        0x1193b,
        0x1193c,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x1193e,
        0x1193e,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x11943,
        0x11943,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x119d4,
        0x119d7,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x119da,
        0x119db,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x119e0,
        0x119e0,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x11a01,
        0x11a0a,
    }, // Zanabazar Square Vowel S..Zanabazar Square Vowel L
    [2]u21{
        0x11a33,
        0x11a38,
    }, // Zanabazar Square Final C..Zanabazar Square Sign An
    [2]u21{
        0x11a3b,
        0x11a3e,
    }, // Zanabazar Square Cluster..Zanabazar Square Cluster
    [2]u21{
        0x11a47,
        0x11a47,
    }, // Zanabazar Square Subjoin..Zanabazar Square Subjoin
    [2]u21{
        0x11a51,
        0x11a56,
    }, // Soyombo Vowel Sign I    ..Soyombo Vowel Sign Oe
    [2]u21{
        0x11a59,
        0x11a5b,
    }, // Soyombo Vowel Sign Vocal..Soyombo Vowel Length Mar
    [2]u21{
        0x11a8a,
        0x11a96,
    }, // Soyombo Final Consonant ..Soyombo Sign Anusvara
    [2]u21{
        0x11a98,
        0x11a99,
    }, // Soyombo Gemination Mark ..Soyombo Subjoiner
    [2]u21{
        0x11c30,
        0x11c36,
    }, // Bhaiksuki Vowel Sign I  ..Bhaiksuki Vowel Sign Voc
    [2]u21{
        0x11c38,
        0x11c3d,
    }, // Bhaiksuki Vowel Sign E  ..Bhaiksuki Sign Anusvara
    [2]u21{
        0x11c3f,
        0x11c3f,
    }, // Bhaiksuki Sign Virama   ..Bhaiksuki Sign Virama
    [2]u21{
        0x11c92,
        0x11ca7,
    }, // Marchen Subjoined Letter..Marchen Subjoined Letter
    [2]u21{
        0x11caa,
        0x11cb0,
    }, // Marchen Subjoined Letter..Marchen Vowel Sign Aa
    [2]u21{
        0x11cb2,
        0x11cb3,
    }, // Marchen Vowel Sign U    ..Marchen Vowel Sign E
    [2]u21{
        0x11cb5,
        0x11cb6,
    }, // Marchen Sign Anusvara   ..Marchen Sign Candrabindu
    [2]u21{
        0x11d31,
        0x11d36,
    }, // Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
    [2]u21{
        0x11d3a,
        0x11d3a,
    }, // Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
    [2]u21{
        0x11d3c,
        0x11d3d,
    }, // Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign
    [2]u21{
        0x11d3f,
        0x11d45,
    }, // Masaram Gondi Vowel Sign..Masaram Gondi Virama
    [2]u21{
        0x11d47,
        0x11d47,
    }, // Masaram Gondi Ra-kara   ..Masaram Gondi Ra-kara
    [2]u21{
        0x11d90,
        0x11d91,
    }, // Gunjala Gondi Vowel Sign..Gunjala Gondi Vowel Sign
    [2]u21{
        0x11d95,
        0x11d95,
    }, // Gunjala Gondi Sign Anusv..Gunjala Gondi Sign Anusv
    [2]u21{
        0x11d97,
        0x11d97,
    }, // Gunjala Gondi Virama    ..Gunjala Gondi Virama
    [2]u21{
        0x11ef3,
        0x11ef4,
    }, // Makasar Vowel Sign I    ..Makasar Vowel Sign U
    [2]u21{
        0x16af0,
        0x16af4,
    }, // Bassa Vah Combining High..Bassa Vah Combining High
    [2]u21{
        0x16b30,
        0x16b36,
    }, // Pahawh Hmong Mark Cim Tu..Pahawh Hmong Mark Cim Ta
    [2]u21{
        0x16f4f,
        0x16f4f,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x16f8f,
        0x16f92,
    }, // Miao Tone Right         ..Miao Tone Below
    [2]u21{
        0x16fe4,
        0x16fe4,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x1bc9d,
        0x1bc9e,
    }, // Duployan Thick Letter Se..Duployan Double Mark
    [2]u21{
        0x1d167,
        0x1d169,
    }, // Musical Symbol Combining..Musical Symbol Combining
    [2]u21{
        0x1d17b,
        0x1d182,
    }, // Musical Symbol Combining..Musical Symbol Combining
    [2]u21{
        0x1d185,
        0x1d18b,
    }, // Musical Symbol Combining..Musical Symbol Combining
    [2]u21{
        0x1d1aa,
        0x1d1ad,
    }, // Musical Symbol Combining..Musical Symbol Combining
    [2]u21{
        0x1d242,
        0x1d244,
    }, // Combining Greek Musical ..Combining Greek Musical
    [2]u21{
        0x1da00,
        0x1da36,
    }, // Signwriting Head Rim    ..Signwriting Air Sucking
    [2]u21{
        0x1da3b,
        0x1da6c,
    }, // Signwriting Mouth Closed..Signwriting Excitement
    [2]u21{
        0x1da75,
        0x1da75,
    }, // Signwriting Upper Body T..Signwriting Upper Body T
    [2]u21{
        0x1da84,
        0x1da84,
    }, // Signwriting Location Hea..Signwriting Location Hea
    [2]u21{
        0x1da9b,
        0x1da9f,
    }, // Signwriting Fill Modifie..Signwriting Fill Modifie
    [2]u21{
        0x1daa1,
        0x1daaf,
    }, // Signwriting Rotation Mod..Signwriting Rotation Mod
    [2]u21{
        0x1e000,
        0x1e006,
    }, // Combining Glagolitic Let..Combining Glagolitic Let
    [2]u21{
        0x1e008,
        0x1e018,
    }, // Combining Glagolitic Let..Combining Glagolitic Let
    [2]u21{
        0x1e01b,
        0x1e021,
    }, // Combining Glagolitic Let..Combining Glagolitic Let
    [2]u21{
        0x1e023,
        0x1e024,
    }, // Combining Glagolitic Let..Combining Glagolitic Let
    [2]u21{
        0x1e026,
        0x1e02a,
    }, // Combining Glagolitic Let..Combining Glagolitic Let
    [2]u21{
        0x1e130,
        0x1e136,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x1e2ec,
        0x1e2ef,
    }, // [2]u21{ nil }                   ..
    [2]u21{
        0x1e8d0,
        0x1e8d6,
    }, // Mende Kikakui Combining ..Mende Kikakui Combining
    [2]u21{
        0x1e944,
        0x1e94a,
    }, // Adlam Alif Lengthener   ..Adlam Nukta
    [2]u21{
        0xe0100,
        0xe01ef,
    }, // Variation Selector-17   ..Variation Selector-256
};

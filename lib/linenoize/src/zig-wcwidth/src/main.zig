const std = @import("std");
const testing = std.testing;

const zero_width_cf = @import("manual.zig").zero_width_cf;
const wide_eastasian = @import("table_wide.zig").wide_eastasian;
const zero_width = @import("table_zero.zig").zero_width;

/// A simple binary search in a list containing lower and upper bounds
fn tableBisearch(ucs: u21, table: []const [2]u21) bool {
    var lbound: usize = 0;
    var ubound: usize = table.len - 1;

    // Out of entire table bounds
    if (ucs < table[lbound][0] or ucs > table[ubound][1]) {
        return false;
    }

    // Search table
    while (ubound >= lbound) {
        const mid = (lbound + ubound) / 2;
        if (ucs > table[mid][1]) {
            lbound = mid + 1;
        } else if (ucs < table[mid][0]) {
            ubound = mid - 1;
        } else {
            return true;
        }
    }

    return false;
}

/// A simple binary search for a simple ordered list
fn listBisearch(ucs: u21, list: []const u21) bool {
    var lbound: usize = 0;
    var ubound: usize = list.len - 1;

    // Out of entire table bounds
    if (ucs < list[lbound] or ucs > list[ubound]) {
        return false;
    }

    // Search table
    while (ubound >= lbound) {
        const mid = (lbound + ubound) / 2;
        if (ucs > list[mid]) {
            lbound = mid + 1;
        } else if (ucs < list[mid]) {
            ubound = mid - 1;
        } else {
            return true;
        }
    }

    return false;
}

/// Given one unicode character, return its printable length on a terminal.
///
/// The wcwidth() function returns 0 if the wc argument has no printable effect
/// on a terminal (such as NUL '\0'), -1 if wc is not printable, or has an
/// indeterminate effect on the terminal, such as a control character.
/// Otherwise, the number of column positions the character occupies on a
/// graphic terminal (1 or 2) is returned.
///
/// The following have a column width of -1:
///
///     - C0 control characters (U+001 through U+01F).
///
///     - C1 control characters and DEL (U+07F through U+0A0).
///
/// The following have a column width of 0:
///
///     - Non-spacing and enclosing combining characters (general
///       category code Mn or Me in the Unicode database).
///
///     - NULL (U+0000, 0).
///
///     - COMBINING GRAPHEME JOINER (U+034F).
///
///     - ZERO WIDTH SPACE (U+200B) through
///       RIGHT-TO-LEFT MARK (U+200F).
///
///     - LINE SEPERATOR (U+2028) and
///       PARAGRAPH SEPERATOR (U+2029).
///
///     - LEFT-TO-RIGHT EMBEDDING (U+202A) through
///       RIGHT-TO-LEFT OVERRIDE (U+202E).
///
///     - WORD JOINER (U+2060) through
///       INVISIBLE SEPARATOR (U+2063).
///
/// The following have a column width of 1:
///
///     - SOFT HYPHEN (U+00AD) has a column width of 1.
///
///     - All remaining characters (including all printable
///       ISO 8859-1 and WGL4 characters, Unicode control characters,
///       etc.) have a column width of 1.
///
/// The following have a column width of 2:
///
///     - Spacing characters in the East Asian Wide (W) or East Asian
///       Full-width (F) category as defined in Unicode Technical
///       Report #11 have a column width of 2.
pub fn wcwidth(wc: u21) isize {
    // Manual list
    if (listBisearch(wc, &zero_width_cf)) {
        return 0;
    }

    // C0/C1 control characters
    if (wc < 32 or 0x07F <= wc and wc < 0x0A0) {
        return -1;
    }

    // combining characters with zero width
    if (tableBisearch(wc, &zero_width)) {
        return 0;
    }

    // double width
    if (tableBisearch(wc, &wide_eastasian)) {
        return 2;
    } else {
        return 1;
    }
}

/// Given a unicode string, return its printable length on a terminal.
///
/// Returns ``-1`` if a non-printable character is encountered.
pub fn wcswidth(wcs: []const u21) isize {
    var width: isize = 0;

    for (wcs) |char| {
        const wcw = wcwidth(char);
        if (wcw < 0) return -1;
        width += wcw;
    }

    return width;
}

/// Given a byte slice, return its printable length on a terminal. Returns
/// error.InvalidUtf8 when the byte slice does not contain valid UTF-8.
///
/// Returns ``-1`` if a non-printable character is encountered.
pub fn sliceWidth(s: []const u8) !isize {
    var width: isize = 0;

    var utf8 = (try std.unicode.Utf8View.init(s)).iterator();
    while (utf8.nextCodepoint()) |codepoint| {
        const wcw = wcwidth(codepoint);
        if (wcw < 0) return -1;
        width += wcw;
    }

    return width;
}

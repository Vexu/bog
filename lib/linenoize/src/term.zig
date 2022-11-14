const std = @import("std");
const builtin = @import("builtin");
const File = std.fs.File;

const tcflag_t = std.os.tcflag_t;

const unsupported_term = [_][]const u8{ "dumb", "cons25", "emacs" };

pub fn isUnsupportedTerm() bool {
    const env_var = std.os.getenv("TERM") orelse return false;

    return for (unsupported_term) |t| {
        if (std.ascii.eqlIgnoreCase(env_var, t))
            break true;
    } else false;
}

pub fn enableRawMode(fd: File) !std.os.termios {
    const orig = try std.os.tcgetattr(fd.handle);
    var raw = orig;

    // TODO fix hardcoding of linux
    raw.iflag &= ~(@intCast(tcflag_t, std.os.linux.BRKINT) |
        @intCast(tcflag_t, std.os.linux.ICRNL) |
        @intCast(tcflag_t, std.os.linux.INPCK) |
        @intCast(tcflag_t, std.os.linux.ISTRIP) |
        @intCast(tcflag_t, std.os.linux.IXON));

    raw.oflag &= ~(@intCast(tcflag_t, std.os.linux.OPOST));

    raw.cflag |= (@intCast(tcflag_t, std.os.linux.CS8));

    raw.lflag &= ~(@intCast(tcflag_t, std.os.linux.ECHO) |
        @intCast(tcflag_t, std.os.linux.ICANON) |
        @intCast(tcflag_t, std.os.linux.IEXTEN) |
        @intCast(tcflag_t, std.os.linux.ISIG));

    // FIXME
    // raw.cc[std.os.VMIN] = 1;
    // raw.cc[std.os.VTIME] = 0;

    try std.os.tcsetattr(fd.handle, std.os.TCSA.FLUSH, raw);

    return orig;
}

pub fn disableRawMode(fd: File, orig: std.os.termios) void {
    std.os.tcsetattr(fd.handle, std.os.TCSA.FLUSH, orig) catch {};
}

fn getCursorPosition(in: File, out: File) !usize {
    var buf: [32]u8 = undefined;
    var reader = in.reader();

    // Tell terminal to report cursor to in
    try out.writeAll("\x1B[6n");

    // Read answer
    const answer = (try reader.readUntilDelimiterOrEof(&buf, 'R')) orelse
        return error.CursorPos;

    // Parse answer
    if (!std.mem.startsWith(u8, "\x1B[", answer))
        return error.CursorPos;

    var iter = std.mem.split(u8, answer[2..], ";");
    _ = iter.next() orelse return error.CursorPos;
    const x = iter.next() orelse return error.CursorPos;

    return try std.fmt.parseInt(usize, x, 10);
}

fn getColumnsFallback(in: File, out: File) !usize {
    var writer = out.writer();
    const orig_cursor_pos = try getCursorPosition(in, out);

    try writer.print("\x1B[999C", .{});
    const cols = try getCursorPosition(in, out);

    try writer.print("\x1B[{}D", .{orig_cursor_pos});

    return cols;
}

pub fn getColumns(in: File, out: File) !usize {
    switch (builtin.os.tag) {
        .linux => {
            var wsz: std.os.linux.winsize = undefined;
            if (std.os.linux.ioctl(in.handle, std.os.linux.T.IOCGWINSZ, @ptrToInt(&wsz)) == 0) {
                return wsz.ws_col;
            } else {
                return try getColumnsFallback(in, out);
            }
        },
        else => return try getColumnsFallback(in, out),
    }
}

pub fn clearScreen() !void {
    const stdout = std.io.getStdErr();
    try stdout.writeAll("\x1b[H\x1b[2J");
}

pub fn beep() !void {
    const stderr = std.io.getStdErr();
    try stderr.writeAll("\x07");
}

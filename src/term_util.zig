const std = @import("std");
const is_windows = @import("builtin").os.tag == .windows;

pub const Color = enum {
    reset,
    red,
    green,
    blue,
    cyan,
    purple,
    yellow,
    white,
};

pub fn setColor(color: Color, w: anytype) !void {
    if (is_windows) {
        const stderr_file = std.io.getStdErr();
        if (!stderr_file.isTty()) return;
        const windows = std.os.windows;
        const S = struct {
            var attrs: windows.WORD = undefined;
            var init_attrs = false;
        };
        if (!S.init_attrs) {
            S.init_attrs = true;
            var info: windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
            _ = windows.kernel32.GetConsoleScreenBufferInfo(stderr_file.handle, &info);
            S.attrs = info.wAttributes;
            _ = windows.kernel32.SetConsoleOutputCP(65001);
        }

        // need to flush bufferedWriter
        const T = if (@typeInfo(@TypeOf(w.context)) == .Pointer) @TypeOf(w.context.*) else @TypeOf(w.context);
        if (T != void and @hasDecl(T, "flush")) w.context.flush() catch {};

        switch (color) {
            .reset => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, S.attrs),
            .red => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_INTENSITY),
            .green => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY),
            .blue => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY),
            .cyan => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY),
            .purple => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY),
            .yellow => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY),
            .white => _ = try windows.SetConsoleTextAttribute(stderr_file.handle, windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY),
        }
    } else switch (color) {
        .reset => try w.writeAll("\x1b[0m"),
        .red => try w.writeAll("\x1b[31;1m"),
        .green => try w.writeAll("\x1b[32;1m"),
        .blue => try w.writeAll("\x1b[34;1m"),
        .cyan => try w.writeAll("\x1b[36;1m"),
        .purple => try w.writeAll("\x1b[35;1m"),
        .yellow => try w.writeAll("\x1b[93;1m"),
        .white => try w.writeAll("\x1b[0m\x1b[1m"),
    }
}

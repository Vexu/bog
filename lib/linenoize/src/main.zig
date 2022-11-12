const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const File = std.fs.File;

const LinenoiseState = @import("state.zig").LinenoiseState;
pub const History = @import("history.zig").History;
const term = @import("term.zig");
const isUnsupportedTerm = term.isUnsupportedTerm;
const enableRawMode = term.enableRawMode;
const disableRawMode = term.disableRawMode;
const getColumns = term.getColumns;

pub const HintsCallback = *fn (Allocator, []const u8) Allocator.Error!?[]const u8;
pub const CompletionsCallback = *fn (Allocator, []const u8) Allocator.Error![]const []const u8;

const key_null = 0;
const key_ctrl_a = 1;
const key_ctrl_b = 2;
const key_ctrl_c = 3;
const key_ctrl_d = 4;
const key_ctrl_e = 5;
const key_ctrl_f = 6;
const key_ctrl_h = 8;
const key_tab = 9;
const key_ctrl_k = 11;
const key_ctrl_l = 12;
const key_enter = 13;
const key_ctrl_n = 14;
const key_ctrl_p = 16;
const key_ctrl_t = 20;
const key_ctrl_u = 21;
const key_ctrl_w = 23;
const key_esc = 27;
const key_backspace = 127;

pub fn linenoiseEdit(ln: *Linenoise, in: File, out: File, prompt: []const u8) !?[]const u8 {
    var state = LinenoiseState.init(ln, in, out, prompt);
    defer state.buf.deinit(state.allocator);

    try state.ln.history.add("");
    state.ln.history.current = state.ln.history.hist.items.len - 1;
    try state.refreshLine();

    while (true) {
        var input_buf: [1]u8 = undefined;
        if ((try in.read(&input_buf)) < 1) return null;
        var c = input_buf[0];

        // Browse completions before editing
        if (c == key_tab) {
            if (try state.browseCompletions()) |new_c| {
                c = new_c;
            }
        }

        switch (c) {
            key_null, key_tab => {},
            key_ctrl_a => try state.editMoveHome(),
            key_ctrl_b => try state.editMoveLeft(),
            key_ctrl_c => return error.CtrlC,
            key_ctrl_d => {
                if (state.buf.items.len > 0) {
                    try state.editDelete();
                } else {
                    state.ln.history.pop();
                    return null;
                }
            },
            key_ctrl_e => try state.editMoveEnd(),
            key_ctrl_f => try state.editMoveRight(),
            key_ctrl_k => try state.editKillLineForward(),
            key_ctrl_l => {
                try term.clearScreen();
                try state.refreshLine();
            },
            key_enter => {
                state.ln.history.pop();
                return try ln.allocator.dupe(u8, state.buf.items);
            },
            key_ctrl_n => try state.editHistoryNext(.next),
            key_ctrl_p => try state.editHistoryNext(.prev),
            key_ctrl_t => try state.editSwapPrev(),
            key_ctrl_u => try state.editKillLineBackward(),
            key_ctrl_w => try state.editDeletePrevWord(),
            key_esc => {
                if ((try in.read(&input_buf)) < 1) return null;
                switch (input_buf[0]) {
                    'b' => try state.editMoveWordStart(),
                    'f' => try state.editMoveWordEnd(),
                    '[' => {
                        if ((try in.read(&input_buf)) < 1) return null;
                        switch (input_buf[0]) {
                            '0'...'9' => {
                                const num = input_buf[0];
                                if ((try in.read(&input_buf)) < 1) return null;
                                if (num == '3' and input_buf[0] == '~')
                                    try state.editDelete();
                            },
                            'A' => try state.editHistoryNext(.prev),
                            'B' => try state.editHistoryNext(.next),
                            'C' => try state.editMoveRight(),
                            'D' => try state.editMoveLeft(),
                            'H' => try state.editMoveHome(),
                            'F' => try state.editMoveEnd(),
                            else => {},
                        }
                    },
                    '0' => {
                        if ((try in.read(&input_buf)) < 1) return null;
                        switch (input_buf[0]) {
                            'H' => try state.editMoveHome(),
                            'F' => try state.editMoveEnd(),
                            else => {},
                        }
                    },
                    else => {},
                }
            },
            key_backspace, key_ctrl_h => try state.editBackspace(),
            else => {
                var utf8_buf: [4]u8 = undefined;
                const utf8_len = std.unicode.utf8CodepointSequenceLength(c) catch continue;

                utf8_buf[0] = c;
                if ((try in.read(utf8_buf[1..utf8_len])) < utf8_len - 1) return null;

                try state.editInsert(utf8_buf[0..utf8_len]);
            },
        }
    }
}

/// Read a line with custom line editing mechanics. This includes hints,
/// completions and history
pub fn linenoiseRaw(ln: *Linenoise, in: File, out: File, prompt: []const u8) !?[]const u8 {
    defer out.writeAll("\n") catch {};

    const orig = try enableRawMode(in);
    defer disableRawMode(in, orig);

    return try linenoiseEdit(ln, in, out, prompt);
}

/// Read a line with no special features (no hints, no completions, no history)
pub fn linenoiseNoTTY(allocator: Allocator, stdin: File) !?[]const u8 {
    var reader = stdin.reader();
    const max_line_len = std.math.maxInt(usize);
    return reader.readUntilDelimiterAlloc(allocator, '\n', max_line_len) catch |e| switch (e) {
        error.EndOfStream => return null,
        else => return e,
    };
}

pub const Linenoise = struct {
    allocator: Allocator,
    history: History,
    multiline_mode: bool = false,
    mask_mode: bool = false,
    is_tty: bool = false,
    term_supported: bool = false,
    hints_callback: ?HintsCallback = null,
    completions_callback: ?CompletionsCallback = null,

    const Self = @This();

    /// Initialize a linenoise struct
    pub fn init(allocator: Allocator) Self {
        var self = Self{
            .allocator = allocator,
            .history = History.empty(allocator),
        };
        self.examineStdIo();
        return self;
    }

    /// Free all resources occupied by this struct
    pub fn deinit(self: *Self) void {
        self.history.deinit();
    }

    /// Re-examine (currently) stdin and environment variables to
    /// check if line editing and prompt printing should be
    /// enabled or not.
    pub fn examineStdIo(self: *Self) void {
        const stdin_file = std.io.getStdIn();
        self.is_tty = stdin_file.isTty();
        self.term_supported = !isUnsupportedTerm();
    }

    /// Reads a line from the terminal. Caller owns returned memory
    pub fn linenoise(self: *Self, prompt: []const u8) !?[]const u8 {
        const stdin_file = std.io.getStdIn();
        const stdout_file = std.io.getStdOut();

        if (self.is_tty and !self.term_supported) {
            try stdout_file.writeAll(prompt);
        }

        return if (self.is_tty and self.term_supported)
            try linenoiseRaw(self, stdin_file, stdout_file, prompt)
        else
            try linenoiseNoTTY(self.allocator, stdin_file);
    }
};

test "all" {
    _ = @import("history.zig");
}

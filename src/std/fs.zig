const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

const handle_str = Value.string("handle");

const File = struct {
    /// The OS-specific file descriptor or file handle.
    handle: std.os.fd_t,

    pub fn intoBog(file: File, vm: *Vm) Vm.Error!*Value {
        const map = try Value.zigToBog(vm, File);
        try map.set(vm, &handle_str, &Value{ .int = file.handle });
        return map;
    }

    pub fn fromBog(val: *Value, vm: *Vm) Vm.Error!File {
        if (val.* != .map)
            return vm.fatal("expected map");
        const handle = val.map.get(&handle_str);
        if (handle == null)
            return vm.fatal("expected file");
        if (handle.?.* != .int)
            return vm.fatal("expected file");
        return File{
            .handle = @truncate(std.os.fd_t, handle.?.int),
        };
    }

    pub fn read(vm: *Vm) ![]u8 {
        const file = try fromBog(vm.last_get, vm);
        const zig_file = std.fs.File{
            .handle = file.handle,
        };
        return zig_file.readToEndAlloc(vm.gc.gpa, 1024*1024);
    }

    pub fn close(vm: *Vm) !void {
        const file = try fromBog(vm.last_get, vm);
        const zig_file = std.fs.File{
            .handle = file.handle,
        };
        return zig_file.close();
    }
};

/// mode:
///  empty: default to r
///  a: open for writing, appending to the end of the file if it exists
///  d: open directory
///  r: open for reading
///  w: open for writing
pub fn open(path: []const u8, mode: []const u8) !?File {
    if (mode.len == 0) {
        return File{ .handle = (try std.fs.cwd().openFile(path, .{})).handle };
    } else if (mode.len == 1) {
        switch (mode[0]) {
            'a' => return File{ .handle = (try std.fs.cwd().createFile(path, .{ .truncate = false })).handle },
            'd' => return File{ .handle = (try std.fs.cwd().openDir(path, .{ .iterate = true })).fd },
            'w' => return File{ .handle = (try std.fs.cwd().createFile(path, .{})).handle },
            'r' => return File{ .handle = (try std.fs.cwd().openFile(path, .{})).handle },
            else => {},
        }
    }
    return error.InvalidMode;
}

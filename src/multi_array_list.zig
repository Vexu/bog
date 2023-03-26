const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const meta = std.meta;
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;

/// A MultiArrayList stores a list of a struct type.
/// Instead of storing a single list of items, MultiArrayList
/// stores separate lists for each field of the struct.
/// This allows for memory savings if the struct has padding,
/// and also improves cache usage if only some fields are needed
/// for a computation.  The primary API for accessing fields is
/// the `slice()` function, which computes the start pointers
/// for the array of each field.  From the slice you can call
/// `.items(.<field_name>)` to obtain a slice of field values.
pub fn MultiArrayList(comptime S: type) type {
    return struct {
        bytes: [*]align(@alignOf(S)) u8 = undefined,
        len: u32 = 0,
        capacity: u32 = 0,

        pub const Elem = S;

        pub const Field = meta.FieldEnum(S);

        /// A MultiArrayList.Slice contains cached start pointers for each field in the list.
        /// These pointers are not normally stored to reduce the size of the list in memory.
        /// If you are accessing multiple fields, call slice() first to compute the pointers,
        /// and then get the field arrays from the slice.
        pub const Slice = struct {
            /// This array is indexed by the field index which can be obtained
            /// by using @enumToInt() on the Field enum
            ptrs: [fields.len][*]u8,
            len: u32,
            capacity: u32,

            pub fn items(self: Slice, comptime field: Field) []FieldType(field) {
                const F = FieldType(field);
                if (self.capacity == 0) {
                    return &[_]F{};
                }
                const byte_ptr = self.ptrs[@enumToInt(field)];
                const casted_ptr: [*]F = if (@sizeOf(F) == 0)
                    undefined
                else
                    @ptrCast([*]F, @alignCast(@alignOf(F), byte_ptr));
                return casted_ptr[0..self.len];
            }

            pub fn toMultiArrayList(self: Slice) Self {
                if (self.ptrs.len == 0) {
                    return .{};
                }
                const unaligned_ptr = self.ptrs[sizes.fields[0]];
                const aligned_ptr = @alignCast(@alignOf(S), unaligned_ptr);
                const casted_ptr = @ptrCast([*]align(@alignOf(S)) u8, aligned_ptr);
                return .{
                    .bytes = casted_ptr,
                    .len = self.len,
                    .capacity = self.capacity,
                };
            }

            pub fn deinit(self: *Slice, gpa: Allocator) void {
                var other = self.toMultiArrayList();
                other.deinit(gpa);
                self.* = undefined;
            }
        };

        const Self = @This();

        const fields = meta.fields(S);
        /// `sizes.bytes` is an array of @sizeOf each S field. Sorted by alignment, descending.
        /// `sizes.fields` is an array mapping from `sizes.bytes` array index to field index.
        const sizes = blk: {
            const Data = struct {
                size: u32,
                size_index: u32,
                alignment: u32,
            };
            var data: [fields.len]Data = undefined;
            for (fields, 0..) |field_info, i| {
                data[i] = .{
                    .size = @sizeOf(field_info.type),
                    .size_index = i,
                    .alignment = if (@sizeOf(field_info.type) == 0) 1 else field_info.alignment,
                };
            }
            const Sort = struct {
                fn lessThan(trash: *i32, lhs: Data, rhs: Data) bool {
                    _ = trash;
                    return lhs.alignment > rhs.alignment;
                }
            };
            var trash: i32 = undefined; // workaround for stage1 compiler bug
            std.sort.sort(Data, &data, &trash, Sort.lessThan);
            var sizes_bytes: [fields.len]u32 = undefined;
            var field_indexes: [fields.len]u32 = undefined;
            for (data, 0..) |elem, i| {
                sizes_bytes[i] = elem.size;
                field_indexes[i] = elem.size_index;
            }
            break :blk .{
                .bytes = sizes_bytes,
                .fields = field_indexes,
            };
        };

        /// Release all allocated memory.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            gpa.free(self.allocatedBytes());
            self.* = undefined;
        }

        /// The caller owns the returned memory. Empties this MultiArrayList.
        pub fn toOwnedSlice(self: *Self) Slice {
            const result = self.slice();
            self.* = .{};
            return result;
        }

        /// Compute pointers to the start of each field of the array.
        /// If you need to access multiple fields, calling this may
        /// be more efficient than calling `items()` multiple times.
        pub fn slice(self: Self) Slice {
            var result: Slice = .{
                .ptrs = undefined,
                .len = self.len,
                .capacity = self.capacity,
            };
            var ptr: [*]u8 = self.bytes;
            for (sizes.bytes, 0..) |field_size, i| {
                result.ptrs[sizes.fields[i]] = ptr;
                ptr += field_size * self.capacity;
            }
            return result;
        }

        /// Get the slice of values for a specified field.
        /// If you need multiple fields, consider calling slice()
        /// instead.
        pub fn items(self: Self, comptime field: Field) []FieldType(field) {
            return self.slice().items(field);
        }

        /// Overwrite one array element with new data.
        pub fn set(self: *Self, index: u32, elem: S) void {
            const slices = self.slice();
            inline for (fields, 0..) |field_info, i| {
                slices.items(@intToEnum(Field, i))[index] = @field(elem, field_info.name);
            }
        }

        /// Obtain all the data for one array element.
        pub fn get(self: Self, index: u32) S {
            const slices = self.slice();
            var result: S = undefined;
            inline for (fields, 0..) |field_info, i| {
                @field(result, field_info.name) = slices.items(@intToEnum(Field, i))[index];
            }
            return result;
        }

        /// Extend the list by 1 element. Allocates more memory as necessary.
        pub fn append(self: *Self, gpa: Allocator, elem: S) !void {
            try self.ensureUnusedCapacity(gpa, 1);
            self.appendAssumeCapacity(elem);
        }

        /// Extend the list by 1 element, but asserting `self.capacity`
        /// is sufficient to hold an additional item.
        pub fn appendAssumeCapacity(self: *Self, elem: S) void {
            assert(self.len < self.capacity);
            self.len += 1;
            self.set(self.len - 1, elem);
        }

        /// Extend the list by 1 element, asserting `self.capacity`
        /// is sufficient to hold an additional item.  Returns the
        /// newly reserved index with uninitialized data.
        pub fn addOneAssumeCapacity(self: *Self) u32 {
            assert(self.len < self.capacity);
            const index = self.len;
            self.len += 1;
            return index;
        }

        /// Inserts an item into an ordered list.  Shifts all elements
        /// after and including the specified index back by one and
        /// sets the given index to the specified element.  May reallocate
        /// and invalidate iterators.
        pub fn insert(self: *Self, gpa: Allocator, index: u32, elem: S) !void {
            try self.ensureUnusedCapacity(gpa, 1);
            self.insertAssumeCapacity(index, elem);
        }

        /// Inserts an item into an ordered list which has room for it.
        /// Shifts all elements after and including the specified index
        /// back by one and sets the given index to the specified element.
        /// Will not reallocate the array, does not invalidate iterators.
        pub fn insertAssumeCapacity(self: *Self, index: u32, elem: S) void {
            assert(self.len < self.capacity);
            assert(index <= self.len);
            self.len += 1;
            const slices = self.slice();
            inline for (fields, 0..) |field_info, field_index| {
                const field_slice = slices.items(@intToEnum(Field, field_index));
                var i: u32 = self.len - 1;
                while (i > index) : (i -= 1) {
                    field_slice[i] = field_slice[i - 1];
                }
                field_slice[index] = @field(elem, field_info.name);
            }
        }

        /// Remove the specified item from the list, swapping the last
        /// item in the list into its position.  Fast, but does not
        /// retain list ordering.
        pub fn swapRemove(self: *Self, index: u32) void {
            const slices = self.slice();
            inline for (fields, 0..) |_, i| {
                const field_slice = slices.items(@intToEnum(Field, i));
                field_slice[index] = field_slice[self.len - 1];
                field_slice[self.len - 1] = undefined;
            }
            self.len -= 1;
        }

        /// Remove the specified item from the list, shifting items
        /// after it to preserve order.
        pub fn orderedRemove(self: *Self, index: u32) void {
            const slices = self.slice();
            inline for (fields, 0..) |_, field_index| {
                const field_slice = slices.items(@intToEnum(Field, field_index));
                var i = index;
                while (i < self.len - 1) : (i += 1) {
                    field_slice[i] = field_slice[i + 1];
                }
                field_slice[i] = undefined;
            }
            self.len -= 1;
        }

        /// Adjust the list's length to `new_len`.
        /// Does not initialize added items, if any.
        pub fn resize(self: *Self, gpa: Allocator, new_len: u32) !void {
            try self.ensureTotalCapacity(gpa, new_len);
            self.len = new_len;
        }

        /// Attempt to reduce allocated capacity to `new_len`.
        /// If `new_len` is greater than zero, this may fail to reduce the capacity,
        /// but the data remains intact and the length is updated to new_len.
        pub fn shrinkAndFree(self: *Self, gpa: Allocator, new_len: u32) void {
            if (new_len == 0) {
                gpa.free(self.allocatedBytes());
                self.* = .{};
                return;
            }
            assert(new_len <= self.capacity);
            assert(new_len <= self.len);

            const other_bytes = gpa.allocAdvanced(
                u8,
                @alignOf(S),
                capacityInBytes(new_len),
                .exact,
            ) catch {
                const self_slice = self.slice();
                inline for (fields, 0..) |field_info, i| {
                    if (@sizeOf(field_info.type) != 0) {
                        const field = @intToEnum(Field, i);
                        const dest_slice = self_slice.items(field)[new_len..];
                        const byte_count = dest_slice.len * @sizeOf(field_info.type);
                        // We use memset here for more efficient codegen in safety-checked,
                        // valgrind-enabled builds. Otherwise the valgrind client request
                        // will be repeated for every element.
                        @memset(@ptrCast([*]u8, dest_slice.ptr), undefined, byte_count);
                    }
                }
                self.len = new_len;
                return;
            };
            var other = Self{
                .bytes = other_bytes.ptr,
                .capacity = new_len,
                .len = new_len,
            };
            self.len = new_len;
            const self_slice = self.slice();
            const other_slice = other.slice();
            inline for (fields, 0..) |field_info, i| {
                if (@sizeOf(field_info.type) != 0) {
                    const field = @intToEnum(Field, i);
                    // TODO we should be able to use std.mem.copy here but it causes a
                    // test failure on aarch64 with -OReleaseFast
                    const src_slice = mem.sliceAsBytes(self_slice.items(field));
                    const dst_slice = mem.sliceAsBytes(other_slice.items(field));
                    @memcpy(dst_slice.ptr, src_slice.ptr, src_slice.len);
                }
            }
            gpa.free(self.allocatedBytes());
            self.* = other;
        }

        /// Reduce length to `new_len`.
        /// Invalidates pointers to elements `items[new_len..]`.
        /// Keeps capacity the same.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: u32) void {
            self.len = new_len;
        }

        /// Modify the array so that it can hold at least `new_capacity` items.
        /// Implements super-linear growth to achieve amortized O(1) append operations.
        /// Invalidates pointers if additional memory is needed.
        pub fn ensureTotalCapacity(self: *Self, gpa: Allocator, new_capacity: u32) !void {
            var better_capacity = self.capacity;
            if (better_capacity >= new_capacity) return;

            while (true) {
                better_capacity += better_capacity / 2 + 8;
                if (better_capacity >= new_capacity) break;
            }

            return self.setCapacity(gpa, better_capacity);
        }

        /// Modify the array so that it can hold at least `additional_count` **more** items.
        /// Invalidates pointers if additional memory is needed.
        pub fn ensureUnusedCapacity(self: *Self, gpa: Allocator, additional_count: u32) !void {
            return self.ensureTotalCapacity(gpa, self.len + additional_count);
        }

        /// Modify the array so that it can hold exactly `new_capacity` items.
        /// Invalidates pointers if additional memory is needed.
        /// `new_capacity` must be greater or equal to `len`.
        pub fn setCapacity(self: *Self, gpa: Allocator, new_capacity: u32) !void {
            assert(new_capacity >= self.len);
            const new_bytes = try gpa.alignedAlloc(
                u8,
                @alignOf(S),
                capacityInBytes(new_capacity),
            );
            if (self.len == 0) {
                gpa.free(self.allocatedBytes());
                self.bytes = new_bytes.ptr;
                self.capacity = new_capacity;
                return;
            }
            var other = Self{
                .bytes = new_bytes.ptr,
                .capacity = new_capacity,
                .len = self.len,
            };
            const self_slice = self.slice();
            const other_slice = other.slice();
            inline for (fields, 0..) |field_info, i| {
                if (@sizeOf(field_info.type) != 0) {
                    const field = @intToEnum(Field, i);
                    mem.copy(field_info.type, other_slice.items(field), self_slice.items(field));
                }
            }
            gpa.free(self.allocatedBytes());
            self.* = other;
        }

        /// Create a copy of this list with a new backing store,
        /// using the specified allocator.
        pub fn clone(self: Self, gpa: Allocator) !Self {
            var result = Self{};
            errdefer result.deinit(gpa);
            try result.ensureTotalCapacity(gpa, self.len);
            result.len = self.len;
            const self_slice = self.slice();
            const result_slice = result.slice();
            inline for (fields, 0..) |field_info, i| {
                if (@sizeOf(field_info.type) != 0) {
                    const field = @intToEnum(Field, i);
                    // TODO we should be able to use std.mem.copy here but it causes a
                    // test failure on aarch64 with -OReleaseFast
                    const src_slice = mem.sliceAsBytes(self_slice.items(field));
                    const dst_slice = mem.sliceAsBytes(result_slice.items(field));
                    @memcpy(dst_slice.ptr, src_slice.ptr, src_slice.len);
                }
            }
            return result;
        }

        /// `ctx` has the following method:
        /// `fn lessThan(ctx: @TypeOf(ctx), a_index: u32, b_index: u32) bool`
        pub fn sort(self: Self, ctx: anytype) void {
            const SortContext = struct {
                sub_ctx: @TypeOf(ctx),
                slice: Slice,

                pub fn swap(sc: @This(), a_index: u32, b_index: u32) void {
                    inline for (fields, 0..) |field_info, i| {
                        if (@sizeOf(field_info.type) != 0) {
                            const field = @intToEnum(Field, i);
                            const ptr = sc.slice.items(field);
                            mem.swap(field_info.type, &ptr[a_index], &ptr[b_index]);
                        }
                    }
                }

                pub fn lessThan(sc: @This(), a_index: u32, b_index: u32) bool {
                    return sc.sub_ctx.lessThan(a_index, b_index);
                }
            };

            std.sort.sortContext(self.len, SortContext{
                .sub_ctx = ctx,
                .slice = self.slice(),
            });
        }

        fn capacityInBytes(capacity: u32) u32 {
            if (builtin.zig_backend == .stage2_c) {
                var bytes: u32 = 0;
                for (sizes.bytes) |size| bytes += size * capacity;
                return bytes;
            } else {
                const sizes_vector: @Vector(sizes.bytes.len, u32) = sizes.bytes;
                const capacity_vector = @splat(sizes.bytes.len, capacity);
                return @reduce(.Add, capacity_vector * sizes_vector);
            }
        }

        fn allocatedBytes(self: Self) []align(@alignOf(S)) u8 {
            return self.bytes[0..capacityInBytes(self.capacity)];
        }

        fn FieldType(comptime field: Field) type {
            return meta.fieldInfo(S, field).type;
        }
    };
}

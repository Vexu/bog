pub fn assert(val: bool) !void {
    if (!val) return error.AssertionFailed;
}

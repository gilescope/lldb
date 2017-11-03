"""Test DWARF type parsing for Rust."""

import lldb
import os

from lldbsuite.test.decorators import *
from lldbsuite.test.lldbtest import *
from lldbsuite.test import lldbutil


class TestRustASTContext(TestBase):
    mydir = TestBase.compute_mydir(__file__)

    @add_test_categories(['pyapi'])
    @no_debug_info_test
    @skipUnlessRustInstalled
    def test_with_dsym_and_python_api(self):
        """Test RustASTContext DWARF parsing."""
        self.buildRust()
        self.launchProcess()
        self.init_typelist()
        self.check_types()
        self.check_main_vars()

    def setUp(self):
        # Call super's setUp().
        TestBase.setUp(self)
        # Find the line numbers to break inside main().
        self.main_source = "main.rs"
        self.break_line = line_number(self.main_source, '// breakpoint')

    def launchProcess(self):
        exe = os.path.join(os.getcwd(), "main")

        target = self.dbg.CreateTarget(exe)
        self.assertTrue(target, VALID_TARGET)

        bpt = target.BreakpointCreateByLocation(self.main_source, self.break_line)
        self.assertTrue(bpt, VALID_BREAKPOINT)

        # Now launch the process, and do not stop at entry point.
        process = target.LaunchSimple(None, None, self.get_process_working_directory())

        self.assertTrue(process, PROCESS_IS_VALID)

        # The stop reason of the thread should be breakpoint.
        thread_list = lldbutil.get_threads_stopped_at_breakpoint(process, bpt)

        # Make sure we stopped at the first breakpoint.
        self.assertTrue(
            len(thread_list) != 0,
            "No thread stopped at our breakpoint.")
        self.assertTrue(len(thread_list) == 1,
                        "More than one thread stopped at our breakpoint.")

        frame = thread_list[0].GetFrameAtIndex(0)
        self.assertTrue(frame, "Got a valid frame 0 frame.")

    def init_typelist(self):
        address_size = self.target().GetAddressByteSize()
        self._typelist = [
            ('bool', 1, 'true'),
            ('i8', 1, '-23'),
            ('u8', 1, '23'),
            ('i16', 2, '-2323'),
            ('u16', 2, '2323'),
            ('i32', 4, '-232323'),
            ('u32', 4, '232323'),
            ('i64', 8, '-23232323'),
            ('u64', 8, '23232323'),
            ('isize', address_size, '-23232323'),
            ('usize', address_size, '23232323'),
            ('f32', 4, '5.25'),
            ('f64', 8, '7.5'),
        ]

    def check_type(self, name, size, typeclass):
        tl = self.target().FindTypes(name)
        self.assertTrue(len(tl) > 0)
        t = list(tl)[0]
        self.assertEqual(name, t.name)
        self.assertEqual(typeclass, t.type)
        self.assertEqual(size, t.size)

    def check_types(self):
        for (name, size, value) in self._typelist:
            self.check_type(name, size, lldb.eTypeClassBuiltin)

    def var(self, name):
        var = self.frame().FindVariable(name)
        self.assertTrue(var.IsValid(), "%s %s" % (VALID_VARIABLE, name))
        return var

    def check_main_vars(self):
        for (name, size, value) in self._typelist:
            v = self.var('v' + name)
            self.assertEqual(name, v.GetType().name)
            self.assertEqual(value, v.value)

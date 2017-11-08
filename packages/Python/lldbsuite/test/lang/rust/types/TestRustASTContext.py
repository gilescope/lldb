"""Test DWARF type parsing for Rust."""

from __future__ import print_function

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
        self.check_main_function()
        self.check_structs()

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
        self._typelist = []
        for (name, size, value) in [
                ('bool', 1, 'true'),
                # FIXME - should be "'q'", not '81' here; but that hasn't
                # been implemented yet.
                ('char', 4, '81'),
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
        ]:
            self._typelist.append((name, 'v' + name, size, value))

    def check_type(self, name, size, typeclass):
        tl = self.target().FindTypes(name)
        self.assertTrue(len(tl) > 0)
        t = list(tl)[0]
        self.assertEqual(name, t.name)
        self.assertEqual(typeclass, t.type)
        self.assertEqual(size, t.size)

    def check_types(self):
        for (name, vname, size, value) in self._typelist:
            self.check_type(name, size, lldb.eTypeClassBuiltin)

    def var(self, name):
        var = self.frame().FindVariable(name)
        self.assertTrue(var.IsValid(), "%s %s" % (VALID_VARIABLE, name))
        return var

    def check_main_vars(self):
        mytypelist = self._typelist[:]
        # FIXME value should not be None here
        # Not in _typelist because it isn't eTypeClassBuiltin.
        self._typelist.append(('()', 'empty', 0, None))
        # FIXME Not in _typelist because we can't currently look up
        # this type by name; but also not eTypeClassBuiltin.
        # FIXME the value here as well
        mytypelist.append(('[i8; 4]', 'vi8array', 4, None))
        address_size = self.target().GetAddressByteSize()
        mytypelist.append(('*const bool', 'vboolpointer', address_size, None))
        mytypelist.append(('*mut char', 'vcharpointer', address_size, None))
        mytypelist.append(('&i8', 'vi8ref', address_size, None))
        mytypelist.append(('&mut u8', 'vu8ref', address_size, None))

        for (name, vname, size, value) in mytypelist:
            v = self.var(vname)
            self.assertEqual(name, v.GetType().name)
            # FIXME the None check is a temporary hack.
            if value is not None:
                self.assertEqual(value, v.value)

    def check_main_function(self):
        fn_type = self.frame().GetFunction().GetType()
        self.assertTrue(fn_type.IsFunctionType())
        self.assertEqual(len(fn_type.GetFunctionArgumentTypes()), 0)
        self.assertEqual(fn_type.GetFunctionReturnType().name, '()')

    def check_structs(self):
        for (vname, typename, m0name, m1name, xdesc) in [
                ('vstruct', 'Struct', 'field1', 'field2',
                 'struct Struct { field1: u8, field2: char }'),
                ('vtuplestruct', 'TupleStruct', None, None,
                 'struct TupleStruct ( u8, char )'),
                ('vtuple', '(u8, char)', None, None,
                 # The spaces occur here because we emit newlines and
                 # then those are turned to spaces by the join() below.
                 '( u8, char )'),
                ('vunion', 'Union', 'field1', 'field2',
                 'union Union { field1: u8, field2: char }'),
        ]:
            v = self.var(vname)
            vtype = v.GetType()
            # Get the description but filter out the whitespace.
            desc = ' '.join(str(vtype).split())
            self.assertEqual(desc, xdesc)
            self.assertEqual(typename, vtype.name)
            self.assertTrue(vtype.IsTypeComplete())
            self.assertEqual(vtype.GetNumberOfFields(), 2)
            m0 = vtype.GetFieldAtIndex(0)
            self.assertEqual(m0.GetType().name, 'u8')
            self.assertEqual(m0.GetName(), m0name)
            m1 = vtype.GetFieldAtIndex(1)
            self.assertEqual(m1.GetType().name, 'char')
            self.assertEqual(m1.GetName(), m1name)

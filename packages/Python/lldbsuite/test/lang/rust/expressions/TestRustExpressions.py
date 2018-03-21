"""Test the Rust expression parser and evaluator."""

import os
import time
import unittest2
import lldb
from lldbsuite.test.decorators import *
from lldbsuite.test.lldbtest import *
from lldbsuite.test import lldbutil

class TestRustExpressions(TestBase):
    mydir = TestBase.compute_mydir(__file__)

    @add_test_categories(['pyapi'])
    @no_debug_info_test
    @skipUnlessRustInstalled
    def test_with_dsym_and_python_api(self):
        """Test Rust expression parser and evaluator."""
        self.buildRust()
        self.launchProcess()
        self.rust_expressions()

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

    def rust_expressions(self):
        frame = self.frame()
        v = frame.EvaluateExpression("1")
        self.assertEqual("(i64)  = 1", str(v))
        v = frame.EvaluateExpression("1.25")
        self.assertEqual("(f64)  = 1.25", str(v))
        v = frame.EvaluateExpression("1 + 2")
        self.assertEqual("(i64)  = 3", str(v))
        v = frame.EvaluateExpression("+ 2 + - 1")
        self.assertEqual("(i64)  = 1", str(v))
        v = frame.EvaluateExpression("3 + 1 / 4")
        self.assertEqual("(i64)  = 3", str(v))
        v = frame.EvaluateExpression("(3 + 1) / 4")
        self.assertEqual("(i64)  = 1", str(v))
        v = frame.EvaluateExpression("5 % 4")
        self.assertEqual("(i64)  = 1", str(v))
        v = frame.EvaluateExpression("sizeof(4u8)")
        self.assertEqual("(usize)  = 1", str(v))
        v = frame.EvaluateExpression("!0xffu16")
        self.assertEqual("(u16)  = 65280", str(v))
        v = frame.EvaluateExpression("1 << 3")
        self.assertEqual("(i64)  = 8", str(v))
        v = frame.EvaluateExpression("-1 < 3")
        self.assertEqual("(bool)  = true", str(v))
        v = frame.EvaluateExpression("3 <= 3")
        self.assertEqual("(bool)  = true", str(v))
        v = frame.EvaluateExpression("3 >= 3")
        self.assertEqual("(bool)  = true", str(v))
        v = frame.EvaluateExpression("-9 > -11")
        self.assertEqual("(bool)  = true", str(v))
        v = frame.EvaluateExpression("-27 != -17")
        self.assertEqual("(bool)  = true", str(v))
        v = frame.EvaluateExpression("-27 == -17")
        self.assertEqual("(bool)  = false", str(v))
        v = frame.EvaluateExpression("5.0 / 4")
        self.assertEqual("(f64)  = 1.25", str(v))
        v = frame.EvaluateExpression("'c'")
        self.assertEqual("(char)  = 'c'", str(v))
        v = frame.EvaluateExpression("true")
        self.assertEqual("(bool)  = true", str(v))
        v = frame.EvaluateExpression("false")
        self.assertEqual("(bool)  = false", str(v))
        v = frame.EvaluateExpression("!true")
        self.assertEqual("(bool)  = false", str(v))
        v = frame.EvaluateExpression("vstruct.field1")
        self.assertEqual("(u8) field1 = 23", str(v))
        v = frame.EvaluateExpression("vtuplestruct.0")
        self.assertEqual("(u8)  = 23", str(v))
        v = frame.EvaluateExpression("vtuple.0")
        self.assertEqual("(u8)  = 23", str(v))
        v = frame.EvaluateExpression("vunion.field2")
        self.assertEqual("(char) field2 = 'Q'", str(v))
        v = frame.EvaluateExpression("vi8array[2]")
        self.assertEqual("(i8) [2] = 3", str(v))
        v = frame.EvaluateExpression("*vboolpointer")
        self.assertEqual("(bool) *vboolpointer = true", str(v))
        v = frame.EvaluateExpression("*vcharpointer")
        self.assertEqual("(char) *vcharpointer = 'Q'", str(v))
        v = frame.EvaluateExpression("*vi8ref")
        self.assertEqual("(i8) *vi8ref = -23", str(v))
        v = frame.EvaluateExpression("*vu8ref")
        self.assertEqual("(u8) *vu8ref = 23", str(v))
        # FIXME
        # v = frame.EvaluateExpression("vsimpleenum.1")
        # self.assertEqual("(u8)  = 92", str(v))

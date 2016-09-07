from __future__ import print_function

import unittest2

from lldbsuite.test.decorators import *
from lldbsuite.test.concurrent_base import ConcurrentEventsBase
from lldbsuite.test.lldbtest import TestBase


@skipIfWindows
class ConcurrentTwoBreakpointThreads(ConcurrentEventsBase):

    mydir = ConcurrentEventsBase.compute_mydir(__file__)

    @skipIfFreeBSD  # timing out on buildbot
    # Atomic sequences are not supported yet for MIPS in LLDB.
    @expectedFailureAll(triple='^mips')
    def test_two_breakpoint_threads(self):
        """Test two threads that trigger a breakpoint. """
        self.build(dictionary=self.getBuildFlags())
        self.do_thread_actions(num_breakpoint_threads=2)
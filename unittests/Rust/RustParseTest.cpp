//===-- RustParseTest.cpp ------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "gtest/gtest.h"
#include <cstdarg>
#include "Plugins/ExpressionParser/Rust/RustParse.h"
#include "lldb/Utility/Status.h"
#include "lldb/Utility/StreamString.h"

using namespace lldb_private;
using namespace lldb_private::rust;

static void
TestParse(const char *input, const char *expected) {
  Parser parser(nullptr, input);

  Status error;
  RustExpressionUP result = parser.Expr(error);

  if (expected == nullptr) {
    EXPECT_EQ(result.get(), nullptr) << "expected failure for "
                                     << input << ": " << error.AsCString();
    EXPECT_NE(error.AsCString(nullptr), nullptr) << "expected error message for " << input;
  } else {
    ASSERT_NE(result.get(), nullptr) << "unexpected parse failure for "
                                     << input << ": " << error.AsCString();

    StreamString str;
    str << result;

    EXPECT_STREQ(str.GetData(), expected) << "incorrect parse for "
                                          << input << ": " << error.AsCString();
  }
}

TEST(RustParseTest, Literals) {
  TestParse("1", "1");

  TestParse("[1,2,3]", "[1, 2, 3]");
  TestParse("[72*3; 8]", "[(72 * 3); 8]");

  TestParse("()", "()");
  TestParse("(1,)", "(1, )");
  TestParse("(1,2,3)", "(1, 2, 3, )");
  TestParse("true || false", "(true || false)");

  TestParse("\"hi\"", "\"hi\"");
  TestParse("b\"hi\"", "b\"hi\"");
  TestParse("r##\"hi\"##", "\"hi\"");
  TestParse("br##\"hi\"##", "b\"hi\"");

  TestParse("'c'", "'c'");
  TestParse("b'c'", "b'c'");
}

TEST(RustParseTest, Simple) {
  TestParse("1 + 2", "(1 + 2)");
  TestParse("1+2*3", "(1 + (2 * 3))");
  TestParse("(1+2)*3", "((1 + 2) * 3)");
  TestParse("1++2", "(1 + + (2))");
  TestParse("1--2", "(1 - - (2))");
  TestParse("1-+-2", "(1 - + (- (2)))");
  TestParse("[1,2,3][5]", "([1, 2, 3] @ 5)");
  // We don't serialize sizeof all that nicely.
  TestParse("sizeof(())", "@ (())");
}

TEST(RustParseTest, Members) {
  TestParse("something.57", "something.57");
  TestParse("something.field", "something.field");
}

TEST(RustParseTest, Calls) {
  TestParse("func()", "func ()");
  TestParse("func(1,2,'b')", "func (1, 2, 'b')");
  TestParse("s.f(7)", "s.f (7)");
  TestParse("23.mumble(8)", "23.mumble (8)");
}

TEST(RustParseTest, Paths) {
  TestParse("self", "self");
  TestParse("self::super::hi", "super::hi");
  TestParse("self::hi::there", "hi::there");
  TestParse("self::super::super::super::hi", "super::super::super::hi");
  TestParse("::hi::there", "::hi::there");
  TestParse("hi::there", "hi::there");
  TestParse("hi::there::<bool>", "hi::there::<bool>");
  TestParse("hi::there::<something<bool>>", "hi::there::<something<bool>>");
}

TEST(RustParseTest, Types) {
  TestParse("32 as usize", "(32 as usize)");
  TestParse("x as (u32, u32, u32)", "(x as (u32, u32, u32))");
  TestParse("x as [bool;7]", "(x as [bool; 7])");
  TestParse("x as &[f64]", "(x as &[f64])");
  TestParse("x as fn(u32, u32)->()", "(x as fn (u32, u32) -> ())");
  TestParse("x as *const [mod::whatever<bool>; 8]", "(x as *const [mod::whatever<bool>; 8])");
}

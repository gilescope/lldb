//===-- RustLex.cpp ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "RustLex.h"
#include "llvm/Support/ConvertUTF.h"

using namespace lldb_private::rust;
using namespace lldb_private;
using namespace lldb;
using namespace llvm;

Token Lexer::Next() {
  // Skip whitespace.
  while (m_iter != m_end &&
         // FIXME is it possible to see newlines here?
         (*m_iter == ' ' || *m_iter == '\t'))
    ++m_iter;
  if (m_iter == m_end) {
    return Token(THATSALLFOLKS);
  }

  char c = *m_iter;
  if (c >= '0' && c <= '9') {
    return Number();
  } else if (c == 'b') {
    return MaybeByteLiteral();
  } else if (c == 'r') {
    return MaybeRawString();
  } else if (c == '"') {
    return String();
  } else if (c == '\'') {
    return Character();
  } else {
    return Operator();
  }
}

bool Lexer::Lookup(const ::llvm::StringRef &str, int *result) {
  ::llvm::StringMap<TokenKind> *map = Keywords();
  const auto &iter = map->find(str);
  if (iter == map->end()) {
    return false;
  }
  *result = iter->second;
  return true;
}

Token Lexer::Operator() {
  int result;

  if (Remaining() >= 3 && Lookup(::llvm::StringRef(m_iter, 3), &result)) {
    m_iter += 3;
    return Token(result);
  }

  if (Remaining() >= 2 && Lookup(::llvm::StringRef(m_iter, 2), &result)) {
    m_iter += 2;
    return Token(result);
  }

  if (strchr(".|&=!<>+-*/%:[](){}", *m_iter) != nullptr) {
    return Token(*m_iter++);
  }

  return Identifier();
}

Token Lexer::Number() {
  // FIXME
  return Token(INVALID);
}

Token Lexer::Identifier() {
  assert(m_iter != m_end);

  ::llvm::StringRef::iterator start = m_iter;
  char c = *m_iter;
  if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_')) {
    return Token(INVALID);
  }

  for (++m_iter; m_iter != m_end; ++m_iter) {
    char c = *m_iter;
    if (! ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' ||
           (c >= '0' && c <= '9'))) {
      break;
    }
  }

  ::llvm::StringRef text(start, m_iter - start);
  int result;
  if (Lookup(text, &result)) {
    return Token(result);
  }

  return Token(IDENTIFIER, text.str());
}

Token Lexer::MaybeByteLiteral() {
  assert(*m_iter == 'b');

  if (Remaining() < 2) {
    return Identifier();
  }
  if (m_iter[1] == 'r') {
    return MaybeRawString(true);
  } else if (m_iter[1] == '"') {
    ++m_iter;
    return String(true);
  } else if (m_iter[1] == '\'') {
    ++m_iter;
    return Character(true);
  }
  return Identifier();
}

bool Lexer::ParseHex(uint64_t *result, int min_digits, int max_digits) {
  *result = 0;
  int i;
  for (i = 0; m_iter != m_end && i <= max_digits; ++i, ++m_iter) {
    uint64_t digit;
    if (*m_iter >= 'a' && *m_iter <= 'f') {
      digit = *m_iter - 'a' + 10;
    } else if (*m_iter >= 'A' && *m_iter <= 'F') {
      digit = *m_iter - 'A' + 10;
    } else if (*m_iter >= '0' && *m_iter <= '9') {
      digit = *m_iter - '0';
    } else {
      return false;
    }
    *result = *result * 16 + digit;
  }

  return i >= min_digits;
}

bool Lexer::ParseEscape(uint64_t *result, bool is_byte) {
  assert(*m_iter == '\\');
  ++m_iter;

  if (m_iter == m_end) {
    return false;
  }
  switch (*m_iter++) {
  case 'x':
    return ParseHex(result, 2, 2);

  case 'u': {
    if (is_byte) {
      return false;
    }
    if (m_iter == m_end || *m_iter++ != '{') {
      return false;
    }
    if (!ParseHex(result, 1, 6)) {
      return false;
    }
    if (m_iter == m_end || *m_iter++ != '}') {
      return false;
    }
    break;
  }

  case 'n':
    *result = '\n';
    break;
  case 'r':
    *result = '\r';
    break;
  case 't':
    *result = '\t';
    break;
  case '\\':
    *result = '\\';
    break;
  case '0':
    *result = 0;
    break;
  case '\'':
    *result = '\'';
    break;
  case '"':
    *result = '"';
    break;

  default:
    return false;
  }

  return true;
}

bool Lexer::AppendEscape(std::string *result, bool is_byte) {
  uint64_t value;
  if (!ParseEscape(&value, is_byte)) {
    return false;
  }

  char utf8[10];
  char *out = utf8;
  if (!ConvertCodePointToUTF8(value, out)) {
    return false;
  }

  result->append(utf8, out);
  return true;
}

Token Lexer::Character(bool is_byte) {
  assert(*m_iter == '\'');

  if (++m_iter == m_end) {
    return Token(INVALID);
  }

  uint64_t result;
  if (*m_iter == '\\') {
    if (!ParseEscape(&result, is_byte)) {
      return Token(INVALID);
    }
  } else {
    result = *m_iter++;
  }

  if (m_iter == m_end || *m_iter++ != '\'') {
    return Token(INVALID);
  }

  return Token(BYTE, result);
}

Token Lexer::MaybeRawString(bool is_byte) {
  // Use a local copy so we can backtrack if need be.
  ::llvm::StringRef::iterator iter = m_iter;

  if (is_byte) {
    assert(*iter == 'b');
    ++iter;
  }

  assert(*iter == 'r');
  ++iter;

  ::llvm::StringRef::iterator before_hashes = iter;
  while (iter != m_end && *iter == '#') {
    ++iter;
  }
  if (iter == m_end || *iter != '"') {
    return Identifier();
  }

  auto n_hashes = iter - before_hashes;
  ::llvm::StringRef::iterator string_start = ++iter;

  for (; iter != m_end; ++iter) {
    if (*iter == '"' &&
        (n_hashes == 0 ||
         (m_end - iter + 1 > n_hashes &&
          strncmp(iter + 1, before_hashes, n_hashes) == 0))) {
      break;
    }
  }

  m_iter = iter;
  if (iter == m_end) {
    return Token(INVALID);
  }

  return Token(is_byte ? BYTESTRING : STRING, std::string(string_start, iter));
}

Token Lexer::String(bool is_byte) {
  assert(*m_iter == '"');
  ++m_iter;

  std::string text;
  while (m_iter != m_end && *m_iter != '"') {
    if (*m_iter == '\\') {
      if (!AppendEscape(&text, is_byte)) {
        return Token(INVALID);
      }
    } else {
      text += *m_iter++;
    }
  }

  if (m_iter == m_end) {
    return Token(INVALID);
  }
  assert(*m_iter == '"');
  ++m_iter;

  return Token(is_byte ? BYTESTRING : STRING, std::move(text));
}

::llvm::StringMap<TokenKind> *Lexer::Keywords() {
  if (m_keywords == nullptr) {
    m_keywords = new ::llvm::StringMap<TokenKind>;
    ::llvm::StringMap<TokenKind> &m = *m_keywords;

    m["as"] = AS;
    m["true"] = TRUE;
    m["false"] = FALSE;
    m["super"] = SUPER;
    m["self"] = SELF;
    m["mut"] = MUT;
    m["const"] = CONST;
    m["fn"] = FN;
    m["sizeof"] = SIZEOF;
    m[".."] = DOTDOT;
    m["||"] = OROR;
    m["&&"] = ANDAND;
    m["=="] = EQEQ;
    m["!="] = NOTEQ;
    m["<="] = LTEQ;
    m[">="] = GTEQ;
    m["<<"] = LSH;
    m[">>"] = RSH;
    m["+="] = PLUS_EQ;
    m["-="] = MINUS_EQ;
    m["*="] = STAR_EQ;
    m["/="] = SLASH_EQ;
    m["%="] = PERCENT_EQ;
    m["<<="] = LSH_EQ;
    m[">>="] = RSH_EQ;
    m["::"] = COLONCOLON;
    m["->"] = ARROW;
  }

  return m_keywords;
}

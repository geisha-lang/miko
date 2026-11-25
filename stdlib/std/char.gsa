// ASCII / UTF-8 byte & character classification (`std.char`)

pub def isDigitByte(b: Int): Bool = b >= 48 && b <= 57

pub def isUpperByte(b: Int): Bool = b >= 65 && b <= 90

pub def isLowerByte(b: Int): Bool = b >= 97 && b <= 122

pub def isAlphaByte(b: Int): Bool = isUpperByte(b) || isLowerByte(b)

pub def isAlphaNumByte(b: Int): Bool = isAlphaByte(b) || isDigitByte(b)

pub def isIdentStartByte(b: Int): Bool = isAlphaByte(b) || b == 95

pub def isIdentContByte(b: Int): Bool = isAlphaNumByte(b) || b == 95

pub def isSpaceByte(b: Int): Bool =
    b == 32 || b == 9 || b == 10 || b == 13

pub def isDigit(c: Char): Bool = isDigitByte(charToInt(c))

pub def isAlpha(c: Char): Bool = isAlphaByte(charToInt(c))

pub def isSpace(c: Char): Bool = isSpaceByte(charToInt(c))

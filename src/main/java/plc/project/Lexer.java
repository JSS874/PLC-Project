package plc.project;

import java.util.List;
import java.util.ArrayList;

/**
 * The lexer works through three main functions:
 * <p>
 * - {@link #lex()}, which repeatedly calls lexToken() and skips whitespace
 * - {@link #lexToken()}, which lexes the next token
 * - {@link CharStream}, which manages the state of the lexer and literals
 * <p>
 * If the lexer fails to parse something (such as an unterminated string) you
 * should throw a {@link ParseException} with an index at the invalid character.
 * <p>
 * The {@link #peek(String...)} and {@link #match(String...)} functions are
 * helpers you need to use, they will make the implementation easier.
 */
public final class Lexer {

    private final CharStream chars;

    public Lexer(String input) {
        chars = new CharStream(input);
    }

    /**
     * Repeatedly lexes the input using {@link #lexToken()}, also skipping over
     * whitespace where appropriate.
     */
    public List<Token> lex() {
        List<Token> tokens = new ArrayList<>();
        while (chars.has(0)) {
            if (Character.isWhitespace(chars.get(0))) {
                chars.advance();
            } else {
                tokens.add(lexToken());
            }
        }
        return tokens;
    }

    /**
     * This method determines the type of the next token, delegating to the
     * appropriate lex method. As such, it is best for this method to not change
     * the state of the char stream (thus, use peek not match).
     * <p>
     * The next character should start a valid token since whitespace is handled
     * by {@link #lex()}
     */
    public Token lexToken() {
        if (match("[A-Za-z_]")) {
            return lexIdentifier();
        } else if (peek("[+-]")) {
            // Potential sign; check if it's followed by a digit
            if (chars.has(1) && String.valueOf(chars.get(1)).matches("[0-9]")) {
                return lexNumber();
            } else {
                // '+' or '-' by itself is an operator
                return lexOperator();
            }
        } else if (peek("[0-9]")) {
            return lexNumber();
        } else if (match("'")) {
            return lexCharacter();
        } else if (match("\"")) {
            return lexString();
        } else {
            return lexOperator();
        }
    }

    public Token lexIdentifier() {
        if (!match("[a-zA-Z_]")) {
            throw new ParseException("Invalid identifier start", chars.index);
        }
        while (chars.has(0) && match("[a-zA-Z0-9_-]*")) {
            System.out.println("lexIdentifier Ran");
            // No need to call advance here, match already advances the stream
        }
        return chars.emit(Token.Type.IDENTIFIER);
    }

    public Token lexNumber() {
        // Optional sign
        match("[+-]");

        boolean isZero = false;

        if (match("0")) {
            isZero = true;
        } else if (match("[1-9]")) {
            while (match("[0-9]")) {
                // Consume digits
            }
        } else {
            throw new ParseException("Invalid number format", chars.index);
        }

        // Check for leading zeros
        if (isZero && peek("[0-9]")) {
            throw new ParseException("Leading zeros are not permitted in integers", chars.index);
        }

        // Check for decimal point
        if (match("\\.")) {
            if (!match("[0-9]")) {
                throw new ParseException("Decimal point must be followed by at least one digit", chars.index);
            }
            while (match("[0-9]")) {
                // Consume digits after decimal point
            }
            return chars.emit(Token.Type.DECIMAL);
        } else {
            return chars.emit(Token.Type.INTEGER);
        }
    }


    public Token lexCharacter() {
        if (match("\\\\")) {
            lexEscape();
        } else if (match("[^'\\\\\\n\\r]")) {
            // Valid single character
        } else {
            throw new ParseException("Invalid character in character literal", chars.index);
        }

        if (!match("'")) {
            throw new ParseException("Character literal must end with a single quote", chars.index);
        }

        return chars.emit(Token.Type.CHARACTER);
    }

    public Token lexString() {
        while (chars.has(0) && !peek("\"")) {
            if (match("\\\\")) {
                lexEscape();
            } else if (match("[^\"\n\r\\\\]")) {
                // No need to call advance here, match already advances the stream
            } else {
                throw new ParseException("Invalid character in string literal", chars.index);
            }
        }
        if (!match("\"")) {
            throw new ParseException("String literal must end with a double quote", chars.index);
        }
        return chars.emit(Token.Type.STRING);
    }

    public void lexEscape() {
        if (!match("[bnrt'\"\\\\]")) {
            throw new ParseException("Invalid escape sequence", chars.index);
        }
    }

    public Token lexOperator() {
        // Handle two-character operators
        System.out.println("RAN OPERATOR");
        if (peek("[<>!=]", "=")) {
            match("[<>!=]", "=");
            return chars.emit(Token.Type.OPERATOR);
        } else if (peek("&", "&")) {
            match("&", "&");
            return chars.emit(Token.Type.OPERATOR);
        } else if (peek("\\|", "\\|")) {
            match("\\|", "\\|");
            return chars.emit(Token.Type.OPERATOR);
        } else if (peek("[<>!=]")) {
            match("[<>!=]");
            return chars.emit(Token.Type.OPERATOR);
        } else if (peek("[^\\s]")) {
            match("[^\\s]");
            return chars.emit(Token.Type.OPERATOR);
        } else {
            throw new ParseException("Invalid operator", chars.index);
        }
    }


    /**
     * Returns true if the next sequence of characters match the given patterns,
     * which should be a regex. For example, {@code peek("a", "b", "c")} would
     * return true if the next characters are {@code 'a', 'b', 'c'}.
     */
    public boolean peek(String... patterns) {
        for (int i = 0; i < patterns.length; i++) {
            if (!chars.has(i) || !String.valueOf(chars.get(i)).matches(patterns[i])) {
                return false;
            }
        }
        return true;
//        throw new UnsupportedOperationException(); //TODO (in Lecture)
    }

    /**
     * Returns true in the same way as {@link #peek(String...)}, but also
     * advances the character stream past all matched characters if peek returns
     * true. Hint - it's easiest to have this method simply call peek.
     */
    public boolean match(String... patterns) {
        boolean peek = peek(patterns);
        if (peek) {
            for (int i = 0; i < patterns.length; i++) {
                chars.advance();
            }
        }
        return peek;
//        throw new UnsupportedOperationException(); //TODO (in Lecture)
    }

    /**
     * A helper class maintaining the input string, current index of the char
     * stream, and the current length of the token being matched.
     * <p>
     * You should rely on peek/match for state management in nearly all cases.
     * The only field you need to access is {@link #index} for any {@link
     * ParseException} which is thrown.
     */
    public static final class CharStream {

        private final String input;
        private int index = 0;
        private int length = 0;

        public CharStream(String input) {
            this.input = input;
        }

        public boolean has(int offset) {
            return index + offset < input.length();
        }

        public char get(int offset) {
            return input.charAt(index + offset);
        }

        public void advance() {
            index++;
            length++;
        }

        public void skip() {
            length = 0;
        }

        public Token emit(Token.Type type) {
            int start = index - length;
            skip();
            return new Token(type, input.substring(start, index), start);
        }

    }

}

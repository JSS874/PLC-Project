package plc.project;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The parser takes the sequence of tokens emitted by the lexer and turns that
 * into a structured representation of the program, called the Abstract Syntax
 * Tree (AST).
 * <p>
 * The parser has a similar architecture to the lexer, just with {@link Token}s
 * instead of characters. As before, {@link #peek(Object...)} and {@link
 * #match(Object...)} are helpers to make the implementation easier.
 * <p>
 * This type of parser is called <em>recursive descent</em>. Each rule in our
 * grammar will have it's own function, and reference to other rules correspond
 * to calling those functions.
 */
public final class Parser {

    private final TokenStream tokens;

    public Parser(List<Token> tokens) {
        this.tokens = new TokenStream(tokens);
    }

    /**
     * Parses the {@code source} rule.
     */
    public Ast.Source parseSource() throws ParseException {

        List<Ast.Field> fields = new ArrayList<>();
        List<Ast.Method> methods = new ArrayList<>();

        while (peek("LET") || peek("DEF")) {
            if (peek("LET")) {
                fields.add(parseField());
            } else if (peek("DEF")) {
                methods.add(parseMethod());
            }
        }

        return new Ast.Source(fields, methods);
    }

    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {

        match("LET");

        boolean isConst = false;

        if (peek("CONST")) {
            match("CONST");
            isConst = true;
        }

        if (!peek(Token.Type.IDENTIFIER)) {
            throw new ParseException("Expected an identifier", tokens.get(0).getIndex());
        }

        String name = tokens.get(0).getLiteral();
        match(Token.Type.IDENTIFIER);

        Optional<Ast.Expression> value = Optional.empty();
        if (peek("=")) {
            match("=");
            value = Optional.of(parseExpression());
        }

        match(";");

        return new Ast.Field(name, isConst, value);
    }

    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {

        match("DEF");

        if (!peek(Token.Type.IDENTIFIER)) {
            throw new ParseException("Expected an identifier", tokens.get(0).getIndex());
        }

        String methodName = tokens.get(0).getLiteral();
        match(Token.Type.IDENTIFIER);

        if (!peek("(")) {
            throw new ParseException("Expected an open parenthesis", tokens.get(0).getIndex());
        }
        match("(");
        List<String> parameters = new ArrayList<>();

        if (peek(Token.Type.INTEGER)) {
            parameters.add(tokens.get(0).getLiteral());
            match(Token.Type.INTEGER);

            while (peek(",")) {
                match(",");
                if (!peek(Token.Type.INTEGER)) {
                    throw new ParseException("Expected an integer", tokens.get(0).getIndex());
                }
                parameters.add(tokens.get(0).getLiteral());
                match(Token.Type.INTEGER);
            }
        }

        match(")");

        match("DO)");
        List<Ast.Statement> statements = new ArrayList<>();

        while (!peek("END")) {
            statements.add(parseStatement());
        }

        match("END");

        return new Ast.Method(methodName, parameters, statements);
    }

    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {

        if (peek("LET")) {
            return parseDeclarationStatement();
        }

        if (peek("IF")) {
            return parseIfStatement();
        }

        if (peek("FOR")) {
            return parseForStatement();
        }

        if (peek("WHILE")) {
            return parseWhileStatement();
        }

        if (peek("RETURN")) {
            return parseReturnStatement();
        }

        Ast.Expression expression = parseExpression();

        if (peek("=")) {
            match("=");
            Ast.Expression value = parseExpression();
            match(";");
            return new Ast.Statement.Assignment(expression, value);
        } else {
            match(";");
            return new Ast.Statement.Expression(expression);
        }
    }

    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {

        match("LET");

        if (!peek(Token.Type.IDENTIFIER)) {
            throw new ParseException("Expected an identifier", tokens.get(0).getIndex());
        }

        String name = tokens.get(0).getLiteral();
        match(Token.Type.IDENTIFIER);

        Optional<Ast.Expression> value = Optional.empty();
        if (peek("=")) {
            match("=");
            value = Optional.of(parseExpression());
        }

        match(";");

        return new Ast.Statement.Declaration(name, value);
    }

    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {

        match("IF");

        Ast.Expression condition = parseExpression();

        match("DO");

        List<Ast.Statement> thenStatement = new ArrayList<>();

        while (!peek("ELSE") && !peek("END")) {
            thenStatement.add(parseStatement());
        }

        List<Ast.Statement> elseStatement = new ArrayList<>();

        if (peek("ELSE")) {
            match("ELSE");
            while (!peek("END")) {
                elseStatement.add(parseStatement());
            }
        }

        match("END");

        return new Ast.Statement.If(condition, thenStatement, elseStatement);
    }

    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {

        match("FOR");
        match("(");

        String initializerName = tokens.get(0).getLiteral();
        match("=");
        Ast.Expression initializerValue = parseExpression();
        Ast.Statement.Declaration initializer = new Ast.Statement.Declaration(initializerName, Optional.of(initializerValue));

        match(";");

        Ast.Expression condition = parseExpression();

        match(";");

        Ast.Expression incrementReceiver = parseExpression();
        match("=");
        Ast.Expression incrementValue = parseExpression();
        Ast.Statement.Assignment increment = new Ast.Statement.Assignment(incrementReceiver, incrementValue);

        match(")");

        List<Ast.Statement> body = new ArrayList<>();
        while (!peek("END")) {
            body.add(parseStatement());
        }

        match("END");

        return new Ast.Statement.For(initializer, condition, increment, body);
    }

    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {

        match("WHILE");
        Ast.Expression condition = parseExpression();
        match("DO");
        List<Ast.Statement> body = new ArrayList<>();

        while (!peek("END")) {
            body.add(parseStatement());
        }

        match("END");

        return new Ast.Statement.While(condition, body);
    }

    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {

        match("RETURN");
        Ast.Expression value = parseExpression();
        match(";");

        return new Ast.Statement.Return(value);
    }

    /**
     * Parses the {@code expression} rule.
     */
    public Ast.Expression parseExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code logical-expression} rule.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code equality-expression} rule.
     */
    public Ast.Expression parseEqualityExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code additive-expression} rule.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code multiplicative-expression} rule.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code primary-expression} rule. This is the top-level rule
     * for expressions and includes literal values, grouping, variables, and
     * functions. It may be helpful to break these up into other methods but is
     * not strictly necessary.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {

        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * As in the lexer, returns {@code true} if the current sequence of tokens
     * matches the given patterns. Unlike the lexer, the pattern is not a regex;
     * instead it is either a {@link Token.Type}, which matches if the token's
     * type is the same, or a {@link String}, which matches if the token's
     * literal is the same.
     * <p>
     * In other words, {@code Token(IDENTIFIER, "literal")} is matched by both
     * {@code peek(Token.Type.IDENTIFIER)} and {@code peek("literal")}.
     */
    private boolean peek(Object... patterns) {
        for (int i = 0; i < patterns.length; i++) {
            if (!tokens.has(i)) {
                return false;
            } else if (patterns[i] instanceof Token.Type) {
                if (patterns[i] != tokens.get(i).getType()) {
                    return false;
                }
            } else if (patterns[i] instanceof String) {
                if (!patterns[i].equals(tokens.get(i).getLiteral())) {
                    return false;
                }
            } else {
                throw new AssertionError("Invalid pattern object: " + patterns[i].getClass());
            }
        }
        return true;
    }

    /**
     * As in the lexer, returns {@code true} if {@link #peek(Object...)} is true
     * and advances the token stream.
     */
    private boolean match(Object... patterns) {
        boolean peek = peek(patterns);
        if (peek) {
            for (int i = 0; i < patterns.length; i++) {
                tokens.advance();
            }
        }
        return peek;
    }

    private static final class TokenStream {

        private final List<Token> tokens;
        private int index = 0;

        private TokenStream(List<Token> tokens) {
            this.tokens = tokens;
        }

        /**
         * Returns true if there is a token at index + offset.
         */
        public boolean has(int offset) {
            return index + offset < tokens.size();
        }

        /**
         * Gets the token at index + offset.
         */
        public Token get(int offset) {
            return tokens.get(index + offset);
        }

        /**
         * Advances to the next token, incrementing the index.
         */
        public void advance() {
            index++;
        }

    }

}

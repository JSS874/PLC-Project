package plc.project;
import java.math.BigDecimal;
import java.math.BigInteger;
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
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
        throw new UnsupportedOperationException(); //TODO
    }

    /**
     * Parses the {@code expression} rule.
     */
    public Ast.Expression parseExpression() throws ParseException {
        // The top-level rule for expressions is the logical expression.
        return parseLogicalExpression();
    }

    /**
     * Parses the {@code logical-expression} rule.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
        // First, parse the comparison part of the expression.
        Ast.Expression left = parseEqualityExpression(); // Parsing comparison_expression.

        // Now, check for logical operators ('&&' or '||').
        while (peek("&&") || peek("||")) {
            // Match the operator and get its literal value.
            String operator = tokens.get(0).getLiteral(); // Get the operator's literal (&& or ||).
            match(operator); // Match the operator token.

            // Parse the right-hand side (another comparison_expression).
            Ast.Expression right = parseEqualityExpression();

            // Combine left and right expressions with the operator into a Binary expression.
            left = new Ast.Expression.Binary(operator, left, right);
        }

        // Return the resulting expression.
        return left;
    }


    /**
     * Parses the {@code equality-expression} rule.
     */
    public Ast.Expression parseEqualityExpression() throws ParseException {
        // First, parse the additive expression (which is the core of the comparison expression).
        Ast.Expression left = parseAdditiveExpression();

        // Now, check for comparison operators ('<' | '<=' | '>' | '>=' | '==' | '!=').
        while (peek("==") || peek("!=") || peek("<") || peek("<=") || peek(">") || peek(">=")) {
            // Match the operator and get its literal value.
            String operator = tokens.get(0).getLiteral(); // Get the operator's literal.
            match(operator); // Match the operator token.

            // Parse the right-hand side (additive expression).
            Ast.Expression right = parseAdditiveExpression();

            // Combine left and right expressions with the operator into a Binary expression.
            left = new Ast.Expression.Binary(operator, left, right);
        }

        // Return the resulting expression.
        return left;
    }



    /**
     * Parses the {@code additive-expression} rule.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
        // Parse the multiplicative expression on the left-hand side.
        Ast.Expression left = parseMultiplicativeExpression();

        // Continue parsing while we encounter '+' or '-' operators.
        while (peek("+") || peek("-")) {
            // Match and get the operator (either '+' or '-').
            String operator = tokens.get(0).getLiteral();
            match(operator); // Match the operator token.

            // Parse the right-hand side, which is another multiplicative expression.
            Ast.Expression right = parseMultiplicativeExpression();

            // Combine left and right expressions into a binary expression.
            left = new Ast.Expression.Binary(operator, left, right);
        }

        // Return the resulting expression.
        return left;
    }

    /**
     * Parses the {@code multiplicative-expression} rule.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
        // First, parse the primary part of the expression, which is a secondary_expression.
        Ast.Expression left = parsePrimaryExpression();

        // Now, check for multiplicative operators ('*' or '/').
        while (peek("*") || peek("/")) {
            // Match the operator and get its literal value.
            String operator = tokens.get(0).getLiteral(); // Get the current token's literal before matching.
            match(operator); // Match the operator token (either "*" or "/").

            // Parse the right-hand side (secondary expression).
            Ast.Expression right = parsePrimaryExpression();

            // Combine left and right expressions with the operator into a Binary expression.
            left = new Ast.Expression.Binary(operator, left, right);
        }

        // Return the resulting expression.
        return left;
    }

    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        // Start by parsing the primary expression.
        Ast.Expression expression = parsePrimaryExpression();

        // Continue parsing if there's a '.' (indicating field access or method call).
        while (match(".")) {
            // The next token must be an identifier for the field or method name.
            if (!peek(Token.Type.IDENTIFIER)) {
                throw new ParseException("Expected identifier after '.'", tokens.get(0).getIndex());
            }

            String name = tokens.get(0).getLiteral(); // Get the current token's literal.
            match(Token.Type.IDENTIFIER); // Consume the identifier.

            // Check if it's a method call (followed by '(').
            if (match("(")) {
                // Parse the arguments for the method call.
                List<Ast.Expression> arguments = new ArrayList<>();
                // Check if there are any arguments.
                if (!peek(")")) {
                    arguments.add(parseExpression());
                    while (match(",")) {
                        arguments.add(parseExpression());
                    }
                }
                // Match the closing ')'.
                if (!match(")")) {
                    throw new ParseException("Expected ')'", tokens.get(0).getIndex());
                }

                // Create a method call expression, using the current expression as the receiver.
                expression = new Ast.Expression.Function(Optional.of(expression), name, arguments);
            } else {
                // It's a field access, so create an Access expression.
                expression = new Ast.Expression.Access(Optional.of(expression), name);
            }
        }

        // Return the final expression.
        return expression;
    }


    /**
     * Parses the {@code primary-expression} rule. This is the top-level rule
     * for expressions and includes literal values, grouping, variables, and
     * functions. It may be helpful to break these up into other methods but is
     * not strictly necessary.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {
        // Check if it's a literal (NIL, TRUE, FALSE, integer, decimal, character, string)
        if (match("NIL")) {
            return new Ast.Expression.Literal(null);
        } else if (match("TRUE")) {
            return new Ast.Expression.Literal(Boolean.TRUE);
        } else if (match("FALSE")) {
            return new Ast.Expression.Literal(Boolean.FALSE);
        } else if (peek(Token.Type.INTEGER)) {
            String literal = tokens.get(0).getLiteral();
            match(Token.Type.INTEGER);
            return new Ast.Expression.Literal(new BigInteger(literal));
        } else if (peek(Token.Type.DECIMAL)) {
            String literal = tokens.get(0).getLiteral();
            match(Token.Type.DECIMAL);
            return new Ast.Expression.Literal(new BigDecimal(literal));
        } else if (peek(Token.Type.CHARACTER)) {
            String literal = tokens.get(0).getLiteral();
            match(Token.Type.CHARACTER);
            literal = literal.substring(1, literal.length() - 1); // Remove surrounding quotes
            return new Ast.Expression.Literal(literal.charAt(0));
        } else if (peek(Token.Type.STRING)) {
            String literal = tokens.get(0).getLiteral();
            match(Token.Type.STRING);
            literal = literal.substring(1, literal.length() - 1); // Remove surrounding quotes
            return new Ast.Expression.Literal(literal);
        }

        // Check if it's a group expression
        if (match("(")) {
            // Parse the expression inside the parentheses
            Ast.Expression expression = parseExpression();
            if (!match(")")) {
                throw new ParseException("Expected closing ')'", tokens.get(0).getIndex());
            }
            return new Ast.Expression.Group(expression);
        }

        // Check if it's an identifier (either variable or function call)
        if (peek(Token.Type.IDENTIFIER)) {
            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);

            // Check if it's a function call
            if (match("(")) {
                List<Ast.Expression> arguments = new ArrayList<>();
                // Parse arguments if any
                if (!peek(")")) {
                    arguments.add(parseExpression());
                    while (match(",")) {
                        arguments.add(parseExpression());
                    }
                }
                if (!match(")")) {
                    throw new ParseException("Expected closing ')'", tokens.get(0).getIndex());
                }
                return new Ast.Expression.Function(Optional.empty(), name, arguments);
            }

            // It's a variable access if not a function call
            return new Ast.Expression.Access(Optional.empty(), name);
        }

        // If none of the above matched, throw a ParseException
        throw new ParseException("Invalid primary expression", tokens.get(0).getIndex());
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

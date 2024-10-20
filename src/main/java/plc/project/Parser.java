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
        System.out.println("Current Token: " + tokens.get(0).getLiteral());

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

        if (peek(Token.Type.IDENTIFIER)) {
            parameters.add(tokens.get(0).getLiteral());
            match(Token.Type.IDENTIFIER);

            while (peek(",")) {
                match(",");
                if (!peek(Token.Type.IDENTIFIER)) {
                    throw new ParseException("Expected an integer", tokens.get(0).getIndex());
                }
                parameters.add(tokens.get(0).getLiteral());
                match(Token.Type.IDENTIFIER);
            }
        }

        match(")");

        if (!peek("DO")) {
            throw new ParseException("Expected DO", tokens.get(0).getIndex());
        }

        match("DO");

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
        System.out.println("Current Token: " + tokens.get(0).getLiteral());

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
            if (!peek(";") || !match(";")) {
                throw new ParseException("Expected ';' after assignment", tokens.get(0).getIndex());
            }
            return new Ast.Statement.Assignment(expression, value);
        } else {
            if (!peek(";") || !match(";")) {
                throw new ParseException("Expected ';' after expression", tokens.get(0).getIndex());
            }
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

        if (!match(";")) {
            throw new ParseException("Expected ';'", tokens.get(0).getIndex());
        }

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
        if (!peek("DO")) {
            throw new ParseException("Expected DO", tokens.get(0).getIndex());
        }
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

        if (!peek(Token.Type.IDENTIFIER)) {
            throw new ParseException("Expected an identifier for initializer", tokens.get(0).getIndex());
        }
        String initializerName = tokens.get(0).getLiteral();
        match(Token.Type.IDENTIFIER);
        match("=");
        Ast.Expression initializerValue = parseExpression();
        Ast.Statement initializer = new Ast.Statement.Assignment(
                new Ast.Expression.Access(Optional.empty(), initializerName),
                initializerValue
        );

        match(";");

        Ast.Expression condition = parseExpression();

        match(";");

        if (!peek(Token.Type.IDENTIFIER)) {
            throw new ParseException("Expected an identifier for increment", tokens.get(0).getIndex());
        }
        Ast.Expression incrementReceiver = parseExpression();
        match("=");
        Ast.Expression incrementValue = parseExpression();
        Ast.Statement increment = new Ast.Statement.Assignment(incrementReceiver, incrementValue);

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

            if (!peek(Token.Type.INTEGER) && !peek(Token.Type.DECIMAL) && !peek(Token.Type.IDENTIFIER) && !peek("(")) {
                throw new ParseException("Expected operand after operator", tokens.get(0).getIndex());
            }

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
        Ast.Expression left = parseSecondaryExpression();

        // Now, check for multiplicative operators ('*' or '/').
        while (peek("*") || peek("/")) {
            // Match the operator and get its literal value.
            String operator = tokens.get(0).getLiteral(); // Get the current token's literal before matching.
            match(operator); // Match the operator token (either "*" or "/").

            // Parse the right-hand side (secondary expression).
            Ast.Expression right = parseSecondaryExpression();

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
            literal = literal.substring(1, literal.length() - 1);
            return new Ast.Expression.Literal(literal.charAt(0));
        } else if (peek(Token.Type.STRING)) {
            String rawString = tokens.get(0).getLiteral();
            String processedString = rawString.substring(1, rawString.length() - 1)
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\\"", "\"")
                    .replace("\\'", "'")
                    .replace("\\\\", "\\");
            match(Token.Type.STRING);
            return new Ast.Expression.Literal(processedString);
        }

        // Check if it's a group expression
        if (match("(")) {
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
            if (!has(offset)) {
                throw new ParseException("Unexpected end of input", index + offset);
            }
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
package com.craftinginterpreters.lox;

import java.util.List;
import static com.craftinginterpreters.lox.TokenType.*;

/// Recursive-descent parser for Lox.
class Parser {
    /// Simple sentinel class for unwinding the parser.
    private static class ParseError extends RuntimeException {}

    private final List<Token> tokens;
    private int current = 0;

    /* Constructors and entry point */

    /// Constructor for parser.
    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    /// Parse the given list of tokens stored within parser.
    /// Returns an abstract syntax tree on success, and `null` on error.
    /// TODO: Revisit this when adding statements.
    Expr parse() {
        try {
            return expression();
        } catch(ParseError error) {
            return null;
        }
    }

    /* Helper functions */
    /// Matches a set of tokens. If any of them is found,
    /// it is consumed and `true` is returned.
    private Boolean match(TokenType... types) {
        for(TokenType type : types) {
            if(check(type)) {
                advance();
                return true;
            }
        }
        return false;
    }

    /// Checks for a current token. Never consumes.
    private Boolean check(TokenType type) {
        if(isAtEnd()) return false;
        return peek().type == type;
    }

    /// Consumes the current token and returns it.
    private Token advance() {
        if(!isAtEnd()) current++;
        return previous();
    }

    /// Checks if we ran out of tokens to parse.
    private Boolean isAtEnd() {
        return peek().type == EOF;
    }

    /// Returns the current token that we have yet to consume.
    private Token peek() {
        return tokens.get(current);
    }

    /// Returns the most recently consumed token.
    private Token previous() {
        return tokens.get(current - 1);
    }

    /// Checks if the next token is of the expected type.
    /// If so, consumes. If not, report an error.
    private Token consume(TokenType type, String message) {
        if(check(type)) return advance();
        throw error(peek(), message);
    }

    /// Returns a parse error when a wrong token is found.
    private ParseError error(Token token, String message) {
        Lox.error(token, message);
        return new ParseError();
    }

    /// Synchronizes the parser state by advancing to the next statement.
    /// Used after a syntax error is encountered to help unwinding the state.
    private void synchronize() {
        advance();
        while(!isAtEnd()) {
            if(previous().type == SEMICOLON) return;
            switch(peek().type) {
                case CLASS:
                case FOR:
                case FUN:
                case IF:
                case PRINT:
                case RETURN:
                case VAR:
                case WHILE:
                    return;
            }
            advance();
        }
    }

    /* Grammar rules */

    /// Grammar rule for expressions in general.
    /// ```
    /// expression -> equality ;
    /// ```
    private Expr expression() {
        return equality();
    }

    /// Grammar rule for equalities or higher precedence.
    /// ```
    /// equality   -> comparison ( ( "!=" | "==" ) comparison )* ;
    /// ```
    private Expr equality() {
        Expr expr = comparison();
        while(match(BANG_EQUAL, EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    /// Grammar rule for comparisons or higher precedence.
    /// ```
    /// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    /// ```
    private Expr comparison() {
        Expr expr = term();
        while(match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            Token operator = previous();
            Expr right = term();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    /// Grammar rule for addition and subtraction, or higher precedence.
    /// ```
    /// term       -> factor ( ( "-" | "+" ) factor )* ;
    /// ```
    private Expr term() {
        Expr expr = factor();
        while(match(MINUS, PLUS)) {
            Token operator = previous();
            Expr right = factor();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    /// Grammar rule for multiplication and division, or higher precedence.
    /// ```
    /// factor     -> unary ( ( "/" | "*" ) unary )* ;
    /// ```
    private Expr factor() {
        Expr expr = unary();
        while(match(SLASH, STAR)) {
            Token operator = previous();
            Expr right = unary();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    /// Grammar rule for unary operations, or higher precedence.
    /// ```
    /// unary      -> ( "!" | "-" ) unary
    ///             | primary ;
    /// ```
    private Expr unary() {
        if(match(BANG, MINUS)) {
            Token operator = previous();
            Expr right = unary();
            return new Expr.Unary(operator, right);
        }
        return primary();
    }

    /// Grammar rule for primary expressions, the highest level
    /// of precedence.
    /// ```
    /// primary    -> NUMBER | STRING | "true" | "false" | "nil"
    ///             | "(" expression ")" ;
    /// ```
    private Expr primary() {
        if(match(FALSE)) return new Expr.Literal(false);
        if(match(TRUE)) return new Expr.Literal(true);
        if(match(NIL)) return new Expr.Literal(null);

        if(match(NUMBER, STRING)) {
            return new Expr.Literal(previous().literal);
        }

        if(match(LEFT_PAREN)) {
            Expr expr = expression();
            consume(RIGHT_PAREN, "Expected ')' after expression.");
            return new Expr.Grouping(expr);
        }

        throw error(peek(), "Expected expression.");
    }
}

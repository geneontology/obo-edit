/*
 * Copyright 2002-2006 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bbop.expression;

import java.io.StringReader;

import org.bbop.expression.parser.ASTExpressionExpression;
import org.bbop.expression.parser.ASTForeachStatement;
import org.bbop.expression.parser.ASTIfStatement;
import org.bbop.expression.parser.ASTReferenceExpression;
import org.bbop.expression.parser.ASTStatementExpression;
import org.bbop.expression.parser.ASTWhileStatement;
import org.bbop.expression.parser.ParseException;
import org.bbop.expression.parser.Parser;
import org.bbop.expression.parser.SimpleNode;
import org.bbop.expression.parser.TokenMgrError;

/**
 * <p>
 * Creates Expression objects.  To create a JEXL Expression object, pass
 * valid JEXL syntax to the static createExpression() method:
 * </p>
 *
 * <pre>
 * String jexl = "array[1]";
 * Expression expression = ExpressionFactory.createExpression( jexl );
 * </pre>
 *
 * <p>
 * When an {@link Expression} object is created, the JEXL syntax is
 * parsed and verified.  If the supplied expression is neither an
 * expression nor a reference, an exception is thrown from createException().
 * </p>
 * @since 1.0
 * @author <a href="mailto:geirm@apache.org">Geir Magnusson Jr.</a>
 * @version $Id: ExpressionFactory.java,v 1.3 2007/09/27 01:02:12 jmr39 Exp $
 */
public class ExpressionFactory {

    /**
     * The singleton ExpressionFactory also holds a single instance of
     * {@link Parser}.
     * When parsing expressions, ExpressionFactory synchronizes on Parser.
     */
    protected static Parser parser =
            new Parser(new StringReader(";")); //$NON-NLS-1$

    /**
     * ExpressionFactory is a singleton and this is the private
     * instance fufilling that pattern.
     */
    protected static ExpressionFactory ef = new ExpressionFactory();

    /**
     * Private constructor, the single instance is always obtained
     * with a call to getInstance().
     */
    private ExpressionFactory() {
    }

    /**
     * Returns the single instance of ExpressionFactory.
     * @return the instance of ExpressionFactory.
     */
    protected static  ExpressionFactory getInstance() {
        return ef;
    }

    /**
     * Creates an Expression from a String containing valid
     * JEXL syntax.  This method parses the expression which
     * must contain either a reference or an expression.
     * @param expression A String containing valid JEXL syntax
     * @return An Expression object which can be evaluated with a JexlContext
     * @throws Exception An exception can be thrown if there is a problem
     *      parsing this expression, or if the expression is neither an
     *      expression or a reference.
     */
    public static Expression createExpression(String expression)
        throws Exception {
        return getInstance().createNewExpression(expression);
    }


    /**
     *  Creates a new Expression based on the expression string.
     *
     *  @param expression valid Jexl expression
     *  @return Expression
     *  @throws Exception for a variety of reasons - mostly malformed
     *          Jexl expression
     */
    protected Expression createNewExpression(final String expression)
        throws ExpressionException {

        String expr = cleanExpression(expression);

        // Parse the Expression
        SimpleNode tree;
        synchronized (parser) {
            try {
                tree = parser.parse(new StringReader(expr));
            } catch (TokenMgrError tme) {
                throw new ParseException(tme.getMessage());
            } catch (Exception e) {
				// TODO Auto-generated catch block
				throw new ExpressionException(e);
			}
        }

        // Must be a simple reference, expression, statement or if, otherwise
        // throw an exception.
        SimpleNode node = (SimpleNode) tree.jjtGetChild(0);

        // TODO: Can we get rid of these checks?
        if (node instanceof ASTReferenceExpression
            || node instanceof ASTExpressionExpression
            || node instanceof ASTStatementExpression
            || node instanceof ASTIfStatement
            || node instanceof ASTWhileStatement
            || node instanceof ASTForeachStatement
            ) {
            return new ExpressionImpl(expression, node);
        }

        throw new ExpressionException("Invalid Expression: not a Reference, Expression, "
            + "Statement or If");
    }

    /**
     * Trims the expression and adds a semi-colon if missing.
     * @param expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    private String cleanExpression(String expression) {
        String expr = expression.trim();
        if (!expr.endsWith(";")) {
            expr += ";";
        }
        return expr;
    }
}

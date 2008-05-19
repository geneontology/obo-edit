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

/**
 * This code has been modified by John Day-Richter for use in the org.bbop
 * package.
 */

package org.bbop.expression.parser;

import java.util.Iterator;

import org.bbop.expression.JexlContext;
import org.bbop.expression.util.Introspector;
import org.bbop.expression.util.introspection.Info;

/**
 * ForEach statement. Syntax: foreach (var in iterable) Statement()
 * 
 * @author Dion Gillard
 * @since 1.1
 */
import org.apache.log4j.*;

public class ASTForeachStatement extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTForeachStatement.class);
    /** dummy velocity info. */
    private static final Info DUMMY = new Info("", 1, 1);
    /** index of the loop variable. */
    private static final int VAR_INDEX = 0;
    /** index of the items. */
    private static final int ITEMS_INDEX = 1;
    /** index of the code to execute. */
    private static final int STATEMENT_INDEX = 2;


    /**
     * Create the node given an id.
     * 
     * @param id node id.
     */
    public ASTForeachStatement(int id) {
        super(id);
    }

    /**
     * Create a node with the given parser and id.
     * 
     * @param p a parser.
     * @param id node id.
     */
    public ASTForeachStatement(Parser p, int id) {
        super(p, id);
    }

    /** {@inheritDoc} */
    public Object jjtAccept(ParserVisitor visitor, Object data) {
        return visitor.visit(this, data);
    }

    /** {@inheritDoc} */
    public Object value(JexlContext jc) throws Exception {
        Object result = null;
        /* first child is the loop variable */
        ASTReference loopVariable = (ASTReference) jjtGetChild(VAR_INDEX);
        /* second child is the variable to iterate */
        SimpleNode iterable = (SimpleNode) jjtGetChild(ITEMS_INDEX);
        Object iterableValue = iterable.value(jc);
        // make sure there is a value to iterate on and a statement to execute
        if (iterableValue != null && jjtGetNumChildren() >= (STATEMENT_INDEX + 1)) {
            /* third child is the statement to execute */
            SimpleNode statement = (SimpleNode) jjtGetChild(2);
            // get an iterator for the collection/array etc via the
            // introspector.
            Iterator itemsIterator = Introspector.getUberspect().getIterator(
                    iterableValue, DUMMY);
            while (itemsIterator.hasNext()) {
                // set loopVariable to value of iterator
                Object value = itemsIterator.next();
                try {
                	jc.setLocalVariable(loopVariable.getRootString(), value, true);
                } catch (ParseException ex) {
                	ex.decorateException(this);
                }
                // execute statement
            	try {
            		result = statement.value(jc);
            	} catch (BreakLoopException ex) {
            		return null;
            	}
            }
        }
        return result;
    }
}

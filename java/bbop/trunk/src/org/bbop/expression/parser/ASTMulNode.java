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

package org.bbop.expression.parser;

import org.bbop.expression.JexlContext;
import org.bbop.expression.util.Coercion;

/**
 * Multiplication.
 * 
 * @author <a href="mailto:geirm@apache.org">Geir Magnusson Jr.</a>
 * @version $Id: ASTMulNode.java,v 1.2 2007/09/27 01:02:10 jmr39 Exp $
 */
import org.apache.log4j.*;

public class ASTMulNode extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTMulNode.class);
    /**
     * Create the node given an id.
     * 
     * @param id node id.
     */
    public ASTMulNode(int id) {
        super(id);
    }

    /**
     * Create a node with the given parser and id.
     * 
     * @param p a parser.
     * @param id node id.
     */
    public ASTMulNode(Parser p, int id) {
        super(p, id);
    }

    /** {@inheritDoc} */
    public Object jjtAccept(ParserVisitor visitor, Object data) {
        return visitor.visit(this, data);
    }

    /** {@inheritDoc} */
    public Object value(JexlContext context) throws Exception {
        Object left = ((SimpleNode) jjtGetChild(0)).value(context);
        Object right = ((SimpleNode) jjtGetChild(1)).value(context);

        /*
         * the spec says 'and', I think 'or'
         */
        if (left == null && right == null) {
            return new Byte((byte) 0);
        }

        /*
         * if anything is float, double or string with ( "." | "E" | "e") coerce
         * all to doubles and do it
         */
        if (left instanceof Float
            || left instanceof Double
            || right instanceof Float
            || right instanceof Double
            || (left instanceof String 
                && (((String) left).indexOf(".") != -1 
                    || ((String) left).indexOf("e") != -1 
                    || ((String) left).indexOf("E") != -1))
            || (right instanceof String 
                && (((String) right).indexOf(".") != -1 
                    || ((String) right).indexOf("e") != -1 
                    || ((String) right).indexOf("E") != -1))) {
            Double l = Coercion.coerceDouble(left);
            Double r = Coercion.coerceDouble(right);

            return new Double(l.doubleValue() * r.doubleValue());
        }

        /*
         * otherwise to longs with thee!
         */

        Long l = Coercion.coerceLong(left);
        Long r = Coercion.coerceLong(right);

        return new Long(l.longValue() * r.longValue());
    }

}

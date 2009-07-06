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

import java.math.BigDecimal;
import java.math.BigInteger;

import org.bbop.expression.JexlContext;

/**
 * - (unary minus).
 * 
 * @author <a href="mailto:mhw@kremvax.net">Mark H. Wilkinson</a>
 * @version $Id: ASTUnaryMinusNode.java,v 1.3 2007/09/27 01:02:06 jmr39 Exp $
 */
import org.apache.log4j.*;

public class ASTUnaryMinusNode extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTUnaryMinusNode.class);
    /**
     * Create the node given an id.
     * 
     * @param id node id.
     */
    public ASTUnaryMinusNode(int id) {
        super(id);
    }

    /**
     * Create a node with the given parser and id.
     * 
     * @param p a parser.
     * @param id node id.
     */
    public ASTUnaryMinusNode(Parser p, int id) {
        super(p, id);
    }

    /** {@inheritDoc} */
    @Override
	public Object jjtAccept(ParserVisitor visitor, Object data) {
        return visitor.visit(this, data);
    }

    /** {@inheritDoc} */
    @Override
	public Object value(JexlContext jc) throws Exception {
        Object val = ((SimpleNode) jjtGetChild(0)).value(jc);

        if (val instanceof Byte) {
            byte valueAsByte = ((Byte) val).byteValue();
            return new Byte((byte) -valueAsByte);
        } else if (val instanceof Short) {
            short valueAsShort = ((Short) val).shortValue();
            return new Short((short) -valueAsShort);
        } else if (val instanceof Integer) {
            int valueAsInt = ((Integer) val).intValue();
            return new Integer(-valueAsInt);
        } else if (val instanceof Long) {
            long valueAsLong = ((Long) val).longValue();
            return new Long(-valueAsLong);
        } else if (val instanceof Float) {
            float valueAsFloat = ((Float) val).floatValue();
            return new Float(-valueAsFloat);
        } else if (val instanceof Double) {
            double valueAsDouble = ((Double) val).doubleValue();
            return new Double(-valueAsDouble);
        } else if (val instanceof BigDecimal) {
            BigDecimal valueAsBigD = (BigDecimal) val;
            return valueAsBigD.negate();
        } else if (val instanceof BigInteger) {
            BigInteger valueAsBigI = (BigInteger) val;
            return valueAsBigI.negate();
        }
        throwExpressionException("expression not a number");
        return null;
    }
}

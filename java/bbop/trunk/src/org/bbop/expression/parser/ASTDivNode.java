/*
 * Copyright 2003-2006 The Apache Software Foundation.
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
 * a / b, mathematical divide.
 * 
 * @author <a href="mailto:geirm@apache.org">Geir Magnusson Jr.</a>
 * @version $Id: ASTDivNode.java,v 1.2 2007/09/27 01:02:07 jmr39 Exp $
 */
import org.apache.log4j.*;

public class ASTDivNode extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTDivNode.class);
    /**
     * Create the node given an id.
     * 
     * @param id node id.
     */
    public ASTDivNode(int id) {
        super(id);
    }

    /**
     * Create a node with the given parser and id.
     * 
     * @param p a parser.
     * @param id node id.
     */
    public ASTDivNode(Parser p, int id) {
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
        Object left = ((SimpleNode) jjtGetChild(0)).value(jc);
        Object right = ((SimpleNode) jjtGetChild(1)).value(jc);

        /*
         * the spec says 'and', I think 'or'
         */
        if (left == null && right == null) {
            return new Byte((byte) 0);
        }

        Double l = Coercion.coerceDouble(left);
        Double r = Coercion.coerceDouble(right);

        /*
         * catch div/0
         */
        if (r.doubleValue() == 0.0) {
            return new Double(0.0);
        }

        return new Double(l.doubleValue() / r.doubleValue());

    }
}

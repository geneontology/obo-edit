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

import java.util.HashMap;
import java.util.Map;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.FunctionDef;
import org.bbop.expression.JexlContext;

/**
 * Top of the syntax tree - parsed Jexl code.
 * 
 * @since 1.1
 */
import org.apache.log4j.*;

public class ASTJexlScript extends SimpleNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ASTJexlScript.class);
	
	protected Map localFunctions = new HashMap();
	
    /**
     * Create the node given an id.
     * 
     * @param id node id.
     */
    public ASTJexlScript(int id) {
        super(id);
    }

    public void defineLocalFunction(JexlContext context, String name, FunctionDef def)
    throws Exception {
    	localFunctions.put(name, def);
    	context.setLocalVariable(name, def, false);
    }
    
    public FunctionDef getFunction(String name, JexlContext context) {
    	FunctionDef out = (FunctionDef) localFunctions.get(name);
    	if (out == null)
    		out = context.getFunction(name);
    	return out;
    }
        
    /**
     * Create a node with the given parser and id.
     * 
     * @param p a parser.
     * @param id node id.
     */
    public ASTJexlScript(Parser p, int id) {
        super(p, id);
    }

    /** {@inheritDoc} */
    public Object jjtAccept(ParserVisitor visitor, Object data) {
        return visitor.visit(this, data);
    }

    /** {@inheritDoc} */
    public Object value(JexlContext jc) throws ExpressionException {
        int numChildren = jjtGetNumChildren();
        Object result = null;
        for (int i = 0; i < numChildren; i++) {
            SimpleNode child = (SimpleNode) jjtGetChild(i);
            try {
            	result = child.value(jc);
            } catch (ReturnValueException ex) {
            	return ex.getValue();
            } catch (BreakLoopException ex) {
            	throw new ParseException("Cannot call break outside of a loop");
            } catch (ExpressionException ex) {
            	throw ex;
            } catch (Exception ex) {
            	throw new ExpressionException(ex);
            }
        }
        return result;
    }
    
    public void jjtClose() {
    	
    }
}

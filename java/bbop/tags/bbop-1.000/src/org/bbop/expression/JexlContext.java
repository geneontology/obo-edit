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

import java.util.Map;

/**
 * Holds a Map of variables which are referenced in a JEXL expression.
 *
 *  @since 1.0
 *  @author <a href="mailto:geirm@apache.org">Geir Magnusson Jr.</a>
 *  @version $Id: JexlContext.java,v 1.3 2007/09/27 01:02:12 jmr39 Exp $
 */
public interface JexlContext {
    
    public Object getVariableValue(String name);
    public FunctionDef getFunction(String name);
    public void setVariable(String name, Object value) throws ExpressionException;
    public void setLocalVariable(String name, Object value, boolean modifiable) throws ExpressionException;
    public void setGlobalVariable(String name, Object value, boolean modifiable) throws ExpressionException;
    public void exportLocalVariable(String name) throws ExpressionException;
    public void defineLocalFunction(String name, FunctionDef def) throws ExpressionException;
    public void defineGlobalFunction(String name, FunctionDef def) throws ExpressionException;

    public JexlContext createNewFrame();

}

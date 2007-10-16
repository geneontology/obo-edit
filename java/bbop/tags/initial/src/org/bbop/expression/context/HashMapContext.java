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

package org.bbop.expression.context;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.FunctionDef;
import org.bbop.expression.JexlContext;
import org.bbop.expression.parser.ParseException;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Implementation of JexlContext based on a HashMap.
 * 
 * @since 1.0
 * @author <a href="mailto:geirm@apache.org">Geir Magnusson Jr.</a>
 * @version $Id: HashMapContext.java,v 1.3 2007/09/27 01:02:16 jmr39 Exp $
 */
public class HashMapContext implements JexlContext {
	/** serialization version id jdk13 generated. */
	static final long serialVersionUID = 5715964743204418854L;

	protected Map globalVars;
	protected Map globalFunctions;
	protected Set modifiableGlobals;
	protected Map localVars;
	protected Map localFunctions;

	protected Set modifiableLocals;

	public HashMapContext() {
		globalVars = new HashMap();
		localVars = new HashMap();
		localFunctions = new HashMap();
		globalFunctions = new HashMap();
		modifiableGlobals = new HashSet();
		modifiableLocals = new HashSet();
	}

	private HashMapContext(boolean noInit) {
		localVars = new HashMap();
		localFunctions = new HashMap();
		modifiableLocals = new HashSet();
	}

	public JexlContext createNewFrame() {
		HashMapContext out = new HashMapContext(true);
		out.globalFunctions = globalFunctions;
		out.globalVars = globalVars;
		out.modifiableGlobals = modifiableGlobals;
		return out;
	}

	public void defineLocalFunction(String name, FunctionDef def) throws ExpressionException {
		if (localFunctions.containsKey(name))
			throw new ParseException("Cannot redefine function "+name);
		localFunctions.put(name, def);
	}

	public void exportLocalVariable(String name) throws ExpressionException {
		Object val = localVars.remove(name);
		setGlobalVariable(name, val, true);		
	}

	public FunctionDef getFunction(String name) {
		FunctionDef out = (FunctionDef) localFunctions.get(name);
		if (out != null)
			return out;
		
		Object o = localVars.get(name);
		if (o instanceof FunctionDef)
			return (FunctionDef) o;
		out = (FunctionDef) globalFunctions.get(name);
		if (out != null)
			return out;
		o = globalVars.get(name);
		if (o instanceof FunctionDef)
			return (FunctionDef) o;
		return null;
	}

	public Object getVariableValue(String name) {
		if (localVars.containsKey(name))
			return localVars.get(name);
		else
			return globalVars.get(name);
	}

	/**
	 * Sets the value of a variable. If the variable is defined locally, set the
	 * value of the local variable. If it's not defined locally, but it is
	 * defined globally, set the global variable. If it's not defined anywhere,
	 * create a new local variable.
	 * 
	 * @param name
	 *            the variable name
	 * @param value
	 *            the new value
	 */
	public void setVariable(String name, Object value) throws ExpressionException {
		if (localVars.containsKey(name) || !globalVars.containsKey(name))
			setLocalVariable(name, value, true);
		else {
			setGlobalVariable(name, value, true);
		}
	}

	public void setGlobalVariable(String name, Object value, boolean modifiable)
			throws ExpressionException {
		if (globalVars.containsKey(name) && !modifiableGlobals.contains(name)) {
			throw new ExpressionException("Cannot reset value of unmodifiable global variable "+name);
		}
		globalVars.put(name, value);
		if (modifiable)
			modifiableGlobals.add(name);
	}

	public void setLocalVariable(String name, Object value, boolean modifiable) throws ExpressionException {
		if (localVars.containsKey(name) && !modifiableLocals.contains(name)) {
			throw new ExpressionException("Cannot reset value of unmodifiable local variable "+name);
		}
		localVars.put(name, value);
		if (modifiable)
			modifiableLocals.add(name);
	}

	public void defineGlobalFunction(String name, FunctionDef def) throws ExpressionException {
		if (globalFunctions.containsKey(name))
			throw new ParseException("Cannot redefine function "+name);
		globalFunctions.put(name, def);
	}
}

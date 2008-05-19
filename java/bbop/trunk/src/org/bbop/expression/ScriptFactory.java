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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.URL;
import java.net.URLConnection;

import org.bbop.expression.parser.ASTJexlScript;
import org.bbop.expression.parser.ParseException;
import org.bbop.expression.parser.Parser;
import org.bbop.expression.parser.SimpleNode;
import org.bbop.expression.parser.TokenMgrError;

/**
 * <p> 
 * Creates {@link Script}s.  To create a JEXL Script, pass
 * valid JEXL syntax to the static createScript() method:
 * </p>
 * 
 * <pre>
 * String jexl = "y = x * 12 + 44; y = y * 4;";
 * Script script = ScriptFactory.createScript( jexl );
 * </pre>
 * 
 * <p>
 * When an {@link Script} is created, the JEXL syntax is
 * parsed and verified.
 * </p>
 * @since 1.1
 * @version $Id: ScriptFactory.java,v 1.3 2007/09/27 01:02:12 jmr39 Exp $
 */
import org.apache.log4j.*;

public class ScriptFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ScriptFactory.class);

    /**
     * The singleton ScriptFactory also holds a single instance of 
     * {@link Parser}. When parsing expressions, ScriptFactory 
     * synchronizes on Parser.
     */
    protected static Parser parser = new Parser(new StringReader(";"));

    /**
     * ScriptFactory is a singleton and this is the private
     * instance fufilling that pattern.
     */
    protected static ScriptFactory factory = new ScriptFactory();

    /**
     * Private constructor, the single instance is always obtained
     * with a call to getInstance().
     */
    private ScriptFactory() {

    }

    /**
     * Returns the single instance of ScriptFactory.
     * @return the instance of ScriptFactory.
     */
    protected static  ScriptFactory getInstance() {
        return factory;
    }

    /**
     * Creates a Script from a String containing valid JEXL syntax. 
     * This method parses the script which validates the syntax.
     * 
     * @param scriptText A String containing valid JEXL syntax
     * @return A {@link Script} which can be executed with a 
     *      {@link JexlContext}.
     * @throws ExpressionException An exception can be thrown if there is a 
     *      problem parsing the script.
     */
    public static Script createScript(String scriptText) throws ExpressionException {
        return getInstance().createNewScript(scriptText);
    }

    /**
     * Creates a Script from a {@link File} containing valid JEXL syntax. 
     * This method parses the script and validates the syntax.
     * 
     * @param scriptFile A {@link File} containing valid JEXL syntax.
     *      Must not be null. Must be a readable file.
     * @return A {@link Script} which can be executed with a 
     *      {@link JexlContext}.
     * @throws ExpressionException An exception can be thrown if there is a problem 
     *      parsing the script.
     */
    public static Script createScript(File scriptFile) throws ExpressionException, IOException {
        if (scriptFile == null) {
            throw new NullPointerException("scriptFile is null");
        }
        if (!scriptFile.canRead()) {
            throw new IOException("Can't read scriptFile (" 
                + scriptFile.getCanonicalPath() + ")");
        }
        BufferedReader reader = new BufferedReader(new FileReader(scriptFile));
        return createScript(readerToString(reader));
            
    }

    /**
     * Creates a Script from a {@link URL} containing valid JEXL syntax. 
     * This method parses the script and validates the syntax.
     * 
     * @param scriptUrl A {@link URL} containing valid JEXL syntax.
     *      Must not be null. Must be a readable file.
     * @return A {@link Script} which can be executed with a 
     *      {@link JexlContext}.
     * @throws ExpressionException An exception can be thrown if there is a problem
     *      parsing the script.
     * @throws IOException 
     */
    public static Script createScript(URL scriptUrl) throws ExpressionException, IOException {
        if (scriptUrl == null) {
            throw new NullPointerException("scriptUrl is null");
        }
        URLConnection connection = scriptUrl.openConnection();
        
        BufferedReader reader = new BufferedReader(
            new InputStreamReader(connection.getInputStream()));
        return createScript(readerToString(reader));
    }

    /**
     *  Creates a new Script based on the string.
     *
     *  @param scriptText valid Jexl script
     *  @return Script a new script
     *  @throws ExpressionException for a variety of reasons - mostly malformed scripts
     */
    protected Script createNewScript(String scriptText) throws ExpressionException {
        String cleanText = cleanScript(scriptText);
        SimpleNode script;
        // Parse the Expression
        synchronized (parser) {
            try {
                script = parser.parse(new StringReader(cleanText));
            } catch (TokenMgrError tme) {
                throw new ParseException(tme.getMessage());
            } catch (Exception ex) {
            	throw new ExpressionException(ex);
            }
        }
        if (script instanceof ASTJexlScript) {
            return new ScriptImpl(cleanText, (ASTJexlScript) script);
        } else {
            throw new IllegalStateException("Parsed script is not "
                + "an ASTJexlScript");
        }
    }

    /**
     * @todo move to ParseUtils?
     * Trims the expression and adds a semi-colon if missing.
     * @param script to clean
     * @return trimmed expression ending in a semi-colon
     */
    private String cleanScript(String script) {
        String expr = script.trim();
        if (!expr.endsWith(";")) {
            expr += ";";
        }
        return expr;
    }
    
    /**
     * Read a buffered reader into a StringBuffer and return a String with
     * the contents of the reader.
     * @param reader to be read.
     * @return the contents of the reader as a String.
     * @throws IOException on any error reading the reader.
     */
    private static String readerToString(BufferedReader reader)
        throws IOException {
        StringBuffer buffer = new StringBuffer();
        try {
            String line = null;
            while ((line = reader.readLine()) != null) {
                buffer.append(line).append('\n');
            }
            return buffer.toString();
        } finally {
            reader.close();
        }

    }

}

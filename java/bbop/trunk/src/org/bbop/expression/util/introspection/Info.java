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
package org.bbop.expression.util.introspection;

/**
 * Little class to carry in info such as template name, line and column for
 * information error reporting from the uberspector implementations
 * 
 * Taken from velocity for self-sufficiency.
 * 
 * @since 1.0
 * @author <a href="mailto:geirm@optonline.net">Geir Magnusson Jr.</a>
 * @version $Id: Info.java,v 1.2 2007/09/27 01:02:13 jmr39 Exp $
 */
import org.apache.log4j.*;

public class Info {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Info.class);
    /** line number. */
    private int line;
    /** column number. */
    private int column;
    /** name. */
    private String templateName;
    /** 
     * Create info.
     * @param tn template name
     * @param l line number
     * @param c column
     */
    public Info(String tn, int l, int c) {
        templateName = tn;
        line = l;
        column = c;
    }

    /**
     * Gets the template name.
     * @return template name
     */
    public String getTemplateName() {
        return templateName;
    }

    /**
     * Gets the line number.
     * @return line number.
     */
    public int getLine() {
        return line;
    }

    /**
     * Gets the column number.
     * @return the column.
     */
    public int getColumn() {
        return column;
    }
}

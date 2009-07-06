/*
 * Copyright 2000-2001,2004 The Apache Software Foundation.
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

package org.bbop.expression.util;

import java.lang.reflect.InvocationTargetException;

/**
 * Executor that simply tries to execute a get(key)
 * operation. This will try to find a get(key) method
 * for any type of object, not just objects that
 * implement the Map interface as was previously
 * the case.
 *
 * @since 1.0
 * @author <a href="mailto:jvanzyl@apache.org">Jason van Zyl</a>
 * @version $Id: GetExecutor.java,v 1.2 2007/09/27 01:02:12 jmr39 Exp $
 */
import org.apache.log4j.*;

public class GetExecutor extends AbstractExecutor {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GetExecutor.class);
    /**
     * Container to hold the 'key' part of 
     * get(key).
     */
    private final Object[] args = new Object[1];
    
    /**
     * Default constructor.
     *
     * @param r The instance log.
     * @param ispect The JEXL introspector.
     * @param c The class being examined.
     * @param key The key for the get(key) operation.
     * @throws Exception Failure while trying to obtain the pertinent method.
     */
    public GetExecutor(org.bbop.expression.util.introspection.Introspector ispect,
            Class c, String key) throws Exception {
        args[0] = key;
        method = ispect.getMethod(c, "get", args);
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public Object execute(Object o)
    throws IllegalAccessException, InvocationTargetException {
        if (method == null) {
            return null;
        }

        return method.invoke(o, args);
    }

}


/*
 * Copyright 2002,2004 The Apache Software Foundation.
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
 * Interface defining a 'getter'. For uses when looking for resolution of
 * property references
 * 
 * $foo.bar
 * 
 * @since 1.0
 * @author <a href="mailto:geirm@optonline.net">Geir Magnusson Jr.</a>
 * @version $Id: VelPropertyGet.java,v 1.2 2007/09/27 01:02:15 jmr39 Exp $
 */
public interface VelPropertyGet {
    /**
     * invocation method - called when the 'get action' should be performed and
     * a value returned.
     * @param o the object to get the property from.
     * @return the property value.
     * @throws Exception on any error.
     */
    Object invoke(Object o) throws Exception;

    /**
     * specifies if this VelPropertyGet is cacheable and able to be reused for
     * this class of object it was returned for.
     * 
     * @return true if can be reused for this class, false if not
     */
    boolean isCacheable();

    /**
     * returns the method name used to return this 'property'.
     * @return the method name.
     */
    String getMethodName();
}

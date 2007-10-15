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
 * Interface used for setting values that appear to be properties in Velocity.
 * Ex.
 * 
 * #set($foo.bar = "hello")
 * 
 * @since 1.0
 * @author <a href="mailto:geirm@optonline.net">Geir Magnusson Jr.</a>
 * @version $Id: VelPropertySet.java,v 1.2 2007/09/27 01:02:16 jmr39 Exp $
 */
public interface VelPropertySet {
    /**
     * method used to set the value in the object.
     * 
     * @param o Object on which the method will be called with the arg
     * @param arg value to be set
     * @return the value returned from the set operation (impl specific)
     * @throws Exception on any error.
     */
    Object invoke(Object o, Object arg) throws Exception;

    /**
     * specifies if this VelPropertySet is cacheable and able to be reused for
     * this class of object it was returned for.
     * 
     * @return true if can be reused for this class, false if not
     */
    boolean isCacheable();

    /**
     * returns the method name used to set this 'property'.
     * @return the method name.
     */
    String getMethodName();
}

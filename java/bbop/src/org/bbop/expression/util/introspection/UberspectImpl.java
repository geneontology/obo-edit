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

import org.bbop.expression.util.AbstractExecutor;
import org.bbop.expression.util.ArrayIterator;
import org.bbop.expression.util.BooleanPropertyExecutor;
import org.bbop.expression.util.EnumerationIterator;
import org.bbop.expression.util.GetExecutor;
import org.bbop.expression.util.PropertyExecutor;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.Collection;
import java.util.Map;
import java.util.Enumeration;
import java.util.ArrayList;

/**
 * Implementation of Uberspect to provide the default introspective
 * functionality of Velocity.
 * 
 * @since 1.0
 * @author <a href="mailto:geirm@optonline.net">Geir Magnusson Jr.</a>
 * @version $Id: UberspectImpl.java,v 1.3 2007/09/27 01:02:15 jmr39 Exp $
 */
public class UberspectImpl implements Uberspect {
    /** index of the first character of the property. */
    private static final int PROPERTY_START_INDEX = 3;

    /**
     * the default Velocity introspector.
     */
    private static Introspector introspector;

    /**
     * init - does nothing - we need to have setRuntimeLogger called before
     * getting our introspector, as the default vel introspector depends upon
     * it.
     * @throws Exception on any error.
     */
    public void init() {
    	introspector = new Introspector();
    }
    
    public UberspectImpl() {
    	init();
    }

    /**
     * {@inheritDoc}
     */
    public Iterator getIterator(Object obj, Info i) throws Exception {
        if (obj.getClass().isArray()) {
            return new ArrayIterator(obj);
        } else if (obj instanceof Collection) {
            return ((Collection) obj).iterator();
        } else if (obj instanceof Map) {
            return ((Map) obj).values().iterator();
        } else if (obj instanceof Iterator) {
            return ((Iterator) obj);
        } else if (obj instanceof Enumeration) {
            return new EnumerationIterator((Enumeration) obj);
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    public VelMethod getMethod(Object obj, String methodName, Object[] args, Info i) throws Exception {
        if (obj == null) {
            return null;
        }

        Method m = introspector.getMethod(obj.getClass(), methodName, args);
        if (m == null && obj instanceof Class) {
            m = introspector.getMethod((Class) obj, methodName, args);
        }

        return (m == null) ? null : new VelMethodImpl(m);
    }

    /**
     * {@inheritDoc}
     */
    public VelPropertyGet getPropertyGet(Object obj, String identifier, Info i) throws Exception {
        AbstractExecutor executor;

        Class claz = obj.getClass();

        /*
         * first try for a getFoo() type of property (also getfoo() )
         */

        executor = new PropertyExecutor(introspector, claz, identifier);

        /*
         * look for boolean isFoo()
         */

        if (!executor.isAlive()) {
            executor = new BooleanPropertyExecutor(introspector, claz, identifier);
        }

        /*
         * if that didn't work, look for get("foo")
         */

        if (!executor.isAlive()) {
            executor = new GetExecutor(introspector, claz, identifier);
        }

        return (executor == null) ? null : new VelGetterImpl(executor);
    }

    /**
     * {@inheritDoc}
     */
    public VelPropertySet getPropertySet(Object obj, String identifier, Object arg, Info i) throws Exception {
        Class claz = obj.getClass();

        VelMethod vm = null;
        try {
            /*
             * first, we introspect for the set<identifier> setter method
             */

            Object[] params = {arg};

            try {
                vm = getMethod(obj, "set" + identifier, params, i);

                if (vm == null) {
                    throw new NoSuchMethodException();
                }
            } catch (NoSuchMethodException nsme2) {
                StringBuffer sb = new StringBuffer("set");
                sb.append(identifier);

                if (Character.isLowerCase(sb.charAt(PROPERTY_START_INDEX))) {
                    sb.setCharAt(PROPERTY_START_INDEX, Character.toUpperCase(sb.charAt(PROPERTY_START_INDEX)));
                } else {
                    sb.setCharAt(PROPERTY_START_INDEX, Character.toLowerCase(sb.charAt(PROPERTY_START_INDEX)));
                }

                vm = getMethod(obj, sb.toString(), params, i);

                if (vm == null) {
                    throw new NoSuchMethodException();
                }
            }
        } catch (NoSuchMethodException nsme) {
            /*
             * right now, we only support the Map interface
             */

            if (Map.class.isAssignableFrom(claz)) {
                Object[] params = {new Object(), new Object()};

                vm = getMethod(obj, "put", params, i);

                if (vm != null) {
                    return new VelSetterImpl(vm, identifier);
                }
            }
        }

        return (vm == null) ? null : new VelSetterImpl(vm);
    }

    /**
     * An implementation of {@link VelMethod}.
     */
    public class VelMethodImpl implements VelMethod {
        /** the method. */
        protected Method method = null;
        /** 
         * Create a new instance.
         * 
         * @param m the method.
         */
        public VelMethodImpl(Method m) {
            method = m;
        }

        /**
         * {@inheritDoc}
         */
        public Object invoke(Object o, Object[] params) throws Exception {
            try {
                return method.invoke(o, params);
            } catch (InvocationTargetException e) {
                final Throwable t = e.getTargetException();

                if (t instanceof Exception) {
                    throw (Exception) t;
                } else if (t instanceof Error) {
                    throw (Error) t;
                } else {
                    throw e;
                }
            }
        }

        /**
         * {@inheritDoc}
         */
        public boolean isCacheable() {
            return true;
        }

        /**
         * {@inheritDoc}
         */
        public String getMethodName() {
            return method.getName();
        }

        /**
         * {@inheritDoc}
         */
        public Class getReturnType() {
            return method.getReturnType();
        }
    }

    /**
     * {@inheritDoc}
     */
    public class VelGetterImpl implements VelPropertyGet {
        /** executor for performing the get. */
        protected AbstractExecutor ae = null;

        /**
         * Create the getter using an {@link AbstractExecutor} to
         * do the work.
         * @param exec the executor.
         */
        public VelGetterImpl(AbstractExecutor exec) {
            ae = exec;
        }

        /**
         * {@inheritDoc}
         */
        public Object invoke(Object o) throws Exception {
            return ae.execute(o);
        }

        /**
         * {@inheritDoc}
         */
        public boolean isCacheable() {
            return true;
        }

        /**
         * {@inheritDoc}
         */
        public String getMethodName() {
            return ae.getMethod().getName();
        }
    }

    /**
     * {@inheritDoc}
     */
    public class VelSetterImpl implements VelPropertySet {
        /** the method to call. */
        protected VelMethod vm = null;
        /** the key for indexed and other properties. */
        protected String putKey = null;

        /**
         * Create an instance.
         * @param velmethod the method to call on set.
         */
        public VelSetterImpl(VelMethod velmethod) {
            this.vm = velmethod;
        }

        /**
         * Create an instance.
         * @param velmethod the method to call on set.
         * @param key the index or other value passed to a
         *      setProperty(xxx, value) method.
         */
        public VelSetterImpl(VelMethod velmethod, String key) {
            this.vm = velmethod;
            putKey = key;
        }

        /** {@inheritDoc} */
        public Object invoke(Object o, Object value) throws Exception {
            ArrayList al = new ArrayList();

            if (putKey == null) {
                al.add(value);
            } else {
                al.add(putKey);
                al.add(value);
            }

            return vm.invoke(o, al.toArray());
        }

        /** {@inheritDoc} */
        public boolean isCacheable() {
            return true;
        }

        /** {@inheritDoc} */
        public String getMethodName() {
            return vm.getMethodName();
        }

    }
}

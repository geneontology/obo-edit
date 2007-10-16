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
package org.bbop.expression.util;

import org.bbop.expression.util.introspection.Uberspect;
import org.bbop.expression.util.introspection.UberspectImpl;

/**
 *  Little class to manage a Velocity uberspector (Vel 1.4+) for instrospective
 *  services.
 *
 *  @since 1.0
 *  @author <a href="mailto:geirm@apache.org">Geir Magnusson Jr.</a>
 *  @version $Id: Introspector.java,v 1.2 2007/09/27 01:02:12 jmr39 Exp $
 */
public class Introspector {
    /**
     *  The uberspector from Velocity that handles all instrospection patterns.
     */
    private static Uberspect uberSpect;

    static {
        uberSpect = new UberspectImpl();
    }

    /**
     *  For now, expose the raw uberspector to the AST.
     *
     *  @return Uberspect The Velocity uberspector.
     */
    public static Uberspect getUberspect() {
        return uberSpect;
    }
}

package org.bbop.swing;

import java.awt.Component;
import java.util.Properties;

public interface ComponentNameResolver {

    public Component resolveName(String id, Properties props, String xml);

    public void startParseNotify();

    public void endParseNotify();
}

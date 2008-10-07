package org.bbop.framework;

/**
 * <p>
 * @author John Day-Richter
 * </p>
 * <p>
 * 
 * Docs by Jennifer Deegan and Nicolas Rodriguez, October 2008.
 * </p>
 * <p>
 * Provides context for the settings classes for all the graphical components.
 * As an example of settings, the Tree Viewer is configurable to show paths to
 * multiple selected terms, and the option is always on or off at any given
 * time. When the application is restarted the details of whether such options
 * are on or off are saved to an XML configuration file for reloading at next
 * restart.
 * </p>
 * <p>
 * Each component, e.g. Graphviz Viewer or Tree Viewer, can have a settings
 * class, and the class must implement ComponentConfiguration. The settings
 * object holds information about the configurable characteristics of the
 * component. (Some settings classes are in separate files from other parts of
 * component, some in the same file.) The settings class is a JavaBean. It has
 * to be serializable, as the config file is written out by the <a
 * href="http://java.sun.com/j2se/1.4.2/docs/api/java/beans/XMLEncoder.html"
 * >XMLEncoder</a> and read in by the <a
 * href="http://java.sun.com/j2se/1.5.0/docs/api/java/beans/XMLDecoder.html"
 * >XMLDecoder</a> .
 * </p>
 * <p>
 * There is a good description of what is required for the JavaBean to work in
 * the <a href="http://en.wikipedia.org/wiki/JavaBeans"a>wikipedia</a>.
 * 
 * 
 * 
 */
public interface ComponentConfiguration {
}

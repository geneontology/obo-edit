package org.oboedit.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.*;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.swing.*;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.gui.event.*;

public class OBOEditComponentNameResolver implements ComponentNameResolver {

	protected Map buttonGroups;

	protected List<ComponentNameResolver> resolverExtensions = new LinkedList<ComponentNameResolver>();

	public OBOEditComponentNameResolver() {

	}

	public void addResolverExtension(ComponentNameResolver resolver) {
		resolverExtensions.add(resolver);
	}

	public void removeResolverExtension(ComponentNameResolver resolver) {
		resolverExtensions.remove(resolver);
	}

	// protected Map getConfigMap() {
	// return Controller.getController().getComponentConfigMap();
	// }

	protected ButtonGroup getButtonGroup(String name) {
		if (name == null)
			return null;
		if (buttonGroups == null)
			buttonGroups = new HashMap();
		ButtonGroup group = (ButtonGroup) buttonGroups.get(name);
		if (group == null) {
			group = new ButtonGroup();
			buttonGroups.put(name, group);
		}
		return group;
	}

	public Component resolveName(String id, Properties props, String xml) {
		GUIComponent out = ComponentManager.getManager().createComponent(
				id, null);
		/*
		 * if (id.equals("DAG") || id.equals("OBODAG")) { out = new
		 * OBOPanelHolder(); } else if (id.equals("GRAPH_EDITOR")) { try { out =
		 * new GraphEditor(new HierarchicalGraphLayout()); } catch (Exception e) {
		 * e.printStackTrace(); } } else if (id.equals("INTERSECTION_EDITOR")) {
		 * out = new IntersectionEditor(); } else if
		 * (id.equals("GRAPH_DAG_VIEW")) { out = new DAGViewCanvas(); } else if
		 * (id.equals("FIND")) { FindPanel panel = new FindPanel();
		 * Controller.getController().setFindPanel(panel); //
		 * panel.setController(Controller.getController()); out = panel; } else
		 * if (id.equals("SEARCH_BUTTON")) { int type; String typeStr =
		 * props.getProperty("method"); if (typeStr == null) { type =
		 * UserFilterButton.SEARCH; } else if (typeStr.equals("MODIFY")) { type =
		 * UserFilterButton.MODIFY; } else if (typeStr.equals("FILTER")) { type =
		 * UserFilterButton.FILTER; } else if (typeStr.equals("RENDER")) { type =
		 * UserFilterButton.RENDER; } else type = UserFilterButton.SEARCH;
		 * String filterPath = props.getProperty("filter"); String label =
		 * props.getProperty("label"); String iconPath =
		 * props.getProperty("icon"); String tooltipText =
		 * props.getProperty("tooltip"); String selectedStr =
		 * props.getProperty("selected"); String buttonGroupStr =
		 * props.getProperty("buttonGroup"); boolean selected = selectedStr !=
		 * null && selectedStr.equals("true"); ButtonGroup group =
		 * getButtonGroup(buttonGroupStr); try { out = new
		 * UserFilterButton(type, filterPath, label, selected, iconPath,
		 * tooltipText, group); } catch (Exception ex) { ex.printStackTrace(); } }
		 * else if (id.startsWith("plugin:")) { String pluginname =
		 * id.substring("plugin:".length()); try { ComponentPlugin
		 * nonfinalPlugin = null; for (ComponentPlugin temp :
		 * Controller.getController() .getPlugins()) { if
		 * (temp.getClass().getName().equals(pluginname)) { nonfinalPlugin =
		 * temp; } } if (nonfinalPlugin == null) return new JLabel("Could not
		 * load plugin " + id); final ComponentPlugin plugin = nonfinalPlugin;
		 * plugin.setIsEmbedded(true); plugin.setBorder(new
		 * TitledBorder(plugin.getName()));
		 * plugin.setBackground(Preferences.defaultBackgroundColor());
		 * 
		 * Controller.getController().firePluginActivate( new PluginEvent(this,
		 * plugin));
		 * 
		 * out = plugin; } catch (Exception e) { e.printStackTrace(); } }
		 */
		if (out == null) {
			for (ComponentNameResolver resolver : resolverExtensions) {
				out = (GUIComponent) resolver.resolveName(id, props, xml);
				if (out != null)
					break;
			}
		}
		if (out != null) {
			if (out instanceof JPanel)
				((JComponent) out).setMinimumSize(new Dimension(0, 0));
			else if (out instanceof JComponent) {
				((JComponent) out).setMinimumSize(((JComponent) out)
						.getPreferredSize());
			}

			out.init();
			out.setXML(xml);
			Map configMap;
			if (out instanceof AbstractGUIComponent) {
				configMap = new HashMap();
				ComponentConfiguration cc = (ComponentConfiguration) configMap
						.get(out.getID());

				if (cc != null)
					out.setConfiguration(cc);
			} else {
				configMap = new HashMap();
				java.util.List configList = (java.util.List) configMap.get(out
						.getID());
				ComponentConfiguration config = null;
				if (configList != null && configList.size() > 0) {
					config = (ComponentConfiguration) configList.remove(0);

					if (config != null)
						out.setConfiguration(config);
				}
			}
			/*
			 * if (out.isSingleton()) { Iterator it =
			 * Controller.getController().getActiveComponents() .iterator();
			 * while (it.hasNext()) { OBOEditComponent comp = (OBOEditComponent)
			 * it.next(); if (comp.getID().equals(out.getID())) { return new
			 * JButton(id); } } }
			 */
			ComponentManager.getManager().addActiveComponent(out);
			return out.getComponent();
		} else
			return new JButton(id);
	}

	public void startParseNotify() {

	}

	public void endParseNotify() {

	}
}

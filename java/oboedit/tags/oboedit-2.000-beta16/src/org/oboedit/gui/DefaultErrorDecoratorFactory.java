package org.oboedit.gui;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import javax.swing.JComponent;

import org.obo.datamodel.FieldPathSpec;

public class DefaultErrorDecoratorFactory implements ErrorDecoratorFactory {

	protected Collection<ErrorDecoratorFactory> factories = new LinkedList<ErrorDecoratorFactory>();

	public DefaultErrorDecoratorFactory(boolean installDefaults) {
		if (installDefaults) {
			addFactory(new TextErrorDecoratorFactory());
			addFactory(new JListErrorDecoratorFactory());
			addFactory(new JTableErrorDecoratorFactory());
		}
	}

	public void addFactory(ErrorDecoratorFactory factory) {
		factories.add(factory);
	}

	public void removeFactory(ErrorDecoratorFactory factory) {
		factories.remove(factory);
	}

	protected Map<ErrorDecorator, ErrorDecoratorFactory> factoryMap = new HashMap<ErrorDecorator, ErrorDecoratorFactory>();

	public ErrorDecorator install(FieldPathSpec spec, OBOTextEditComponent parent,
			JComponent c) {
		for (ErrorDecoratorFactory factory : factories) {
			ErrorDecorator d = factory.install(spec, parent, c);
			if (d != null) {
				factoryMap.put(d, factory);
				return d;
			}
		}

		return null;
	}

	public void uninstall(ErrorDecorator e) {
		ErrorDecoratorFactory factory = factoryMap.remove(e);
		if (factory != null) {
			factory.uninstall(e);
		}
	}

}

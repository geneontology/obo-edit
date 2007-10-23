package org.obo.datamodel;

import java.util.Collection;
import java.util.Collections;

public enum ObjectField {
    DEFINITION {
    	public Collection getValues(Object o) {
    		if (o instanceof DefinedObject)
    			return Collections.singletonList(((DefinedObject) o).getDefinition());
    		else
    			return Collections.emptyList();
    			
    	}
    };

    
    ObjectField() {
    	
    }
    
    public abstract Collection getValues(Object o);
    
    ObjectField(double mass, double radius) {
    }

}

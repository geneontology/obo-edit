package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DefinitionSearchCriterion extends AbstractStringCriterion {

	protected final static Logger logger = Logger.getLogger(DefinitionSearchCriterion.class);

        protected boolean excludeObsoletes = false;

	public static final DefinitionSearchCriterion CRITERION =
		new DefinitionSearchCriterion();

	public DefinitionSearchCriterion() {
          this(false);
	}

       public DefinitionSearchCriterion(boolean excludeObsoletes) {
         this.excludeObsoletes = excludeObsoletes;
       }

	public Collection getValues(Collection scratch, Object obj) {  
                // This is adding a lot of empty definitions to the list, but leaving them out doesn't seem to make much difference later on...
//              if (obj instanceof DefinedObject && !((DefinedObject) obj).getDefinition().equals(""))
          if (obj instanceof DefinedObject) {
            if (!(excludeObsoletes && isObsolete((IdentifiedObject)obj)))
              scratch.add(((DefinedObject) obj).getDefinition());
          }
            return scratch;
	}
	
	@Override
	public int getMaxCardinality() {
		return 1;
	}
	
	@Override
	public int getMinCardinality() {
		return 0;
	}

	public String getID() {
		return "definition";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Definition";
	}
}

public Abstract${pojo.getDeclarationName()} generateClone() {
	Abstract${pojo.getDeclarationName()} cloned = new ${pojo.getDeclarationName()}(); 
	<#foreach field in pojo.getPropertiesForFullConstructor()> 
    	   cloned.${field.name} = this.${field.name};
	</#foreach>
	return cloned;
}

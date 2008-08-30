<#if pojo.needsEqualsHashCode() && !clazz.superclass?exists>   public boolean equals(Object other) {
         if ( (this == other ) ) return true;
		 if ( (other == null ) ) return false;
		 if ( !(other instanceof ${pojo.getDeclarationName()}_g) ) return false;
		 ${pojo.getDeclarationName()}_g castOther = ( ${pojo.getDeclarationName()}_g ) other; 
         
		 return ${pojo.generateEquals("this", "castOther", jdk5)};
   }
   
   public int hashCode() {
         int result = 17;
         
<#foreach property in pojo.getAllPropertiesIterator()>         ${pojo.generateHashCode(property, "result", "this", jdk5)}
</#foreach>         return result;
   }   
</#if>
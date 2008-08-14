package org.geneontology.db.model;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Vector;
import org.hibernate.proxy.HibernateProxy;


public abstract class GOModel{
	
	private Vector<Field> uniqueConstraintFields = new Vector<Field>();

	public Vector<Field> getUniqueConstraintFields() {
		return uniqueConstraintFields;
	}

	public void initUniqueConstraintFields(Class<?extends GOModel> c, String[] fieldNames){
		
		for (String fieldName : fieldNames){
			try{
				this.getUniqueConstraintFields().add(c.getDeclaredField(fieldName));
			} catch (SecurityException e) {
				System.err.println("ERROR: Unable to fetch field " + fieldName + " for uniqueConstraints initialization. Security Exception: " +e.getMessage() );				
				e.printStackTrace();
				System.exit(-1);
			} catch (NoSuchFieldException e) {
				System.err.println("ERROR: Unable to fetch field " + fieldName + " for uniqueConstraints initialization. No field for class " + c.getClass().getName() );
				e.printStackTrace();
				System.exit(-1);
			}
		}
	}

	
	
	/*
	 * Hmmm... This could use some discussion. Currently, equals is based on the 
	 * database unique constraints for an object. But that means that if there are two 
	 * features with identical organisms, types, and uniquenames, this equals function will
	 * return true, even if the features have different names,seqlens,residues, etc....
	 * Hmmmm.........
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object co) {
		
		if (co == null){
			return false;
		}
		
		Class<?> thisClass = this.getClass();
		Class<?> comparaClass = co.getClass();
		
		for (Class<?> c : thisClass.getInterfaces()){
			if (c.equals(HibernateProxy.class)){
				thisClass = ((HibernateProxy)this).getHibernateLazyInitializer().getPersistentClass();
			}
		}
		
		for (Class<?> c : comparaClass.getInterfaces()){
			if (c.equals(HibernateProxy.class)){
				comparaClass = ((HibernateProxy)co).getHibernateLazyInitializer().getPersistentClass();
			}
		}
		
		if (co instanceof GOModel){
			boolean isEqual = true;
			for (Field f : this.getUniqueConstraintFields()){
				try {
					Method m = this.getGetter(f);

					// If we have unlike fields, then we're not comparing objects
					// of the same type. Return false. 
					try {
						Method n = ((GOModel)co).getGetter(f);
						if ( n == null){
							return false;
						} else if (!n.equals(m)){
							return false;
						}
					} catch (NoSuchMethodException e){
						return false;
					}
					
					Object[] args = new Object[0];
					//System.out.println("Trying to invoke: " + m.getName() + " on a " + this.getClass().getName() + " and a " + co.getClass().getName());
					Object thisVal = m.invoke(this, args);
					Object coVal = m.invoke(co, args);
					if (((thisVal==null)&&(coVal!=null))||((thisVal!=null)&&(coVal==null))){
						isEqual = false;
					} else if ((thisVal != null)&&(coVal != null)){
						if (!thisVal.equals(coVal)){
							isEqual = false;
				
						}
					}
					
				} catch (IllegalArgumentException e) {
					System.err.println("ERROR: Can't access field " + f.getName() + " of class " + this.getClass().getName() + " for equals calculation. Bad Argument.");
					e.printStackTrace();
					System.exit(-1);
				} catch (IllegalAccessException e) {
					System.err.println("ERROR: Can't access field " + f.getName() + " of class " + this.getClass().getName() + " for equals calculation. Illegal Access.p");
					e.printStackTrace();
					System.exit(-1);
				} catch (InvocationTargetException e) {
					System.err.println("ERROR: Can't invoke getter for field " + f.getName() + " for equals calulation.");
					e.printStackTrace();
					System.exit(-1);
				} catch (NoSuchMethodException e){
					System.err.println("ERROR: Unable to invoke getter method for  " + f.getName() + " for equals calculation." );
					e.printStackTrace();
					System.exit(-1);
				}
			}
			return isEqual;
		} else {
			return super.equals(co);
		}
	}

	public int hashCode(){
		String hashCodeString="";
		for (Field f : this.getUniqueConstraintFields()){
			try {
				Method m = this.getGetter(f);
				Object[] args = new Object[0];
				Object o = m.invoke(this, args);
				if (o != null){
					hashCodeString += Integer.toString(o.hashCode());
				}
			} catch (IllegalArgumentException e) {
				System.err.println("ERROR: Can't access field " + f.getName() + " of class " + this.getClass().getName() + " for hashcode caluclation.");
				e.printStackTrace();
				System.exit(-1);
			} catch (IllegalAccessException e) {
				System.err.println("ERROR: Can't access field " + f.getName() + " of class " + this.getClass().getName() + " for hashcode caluclation.");
				e.printStackTrace();
				System.exit(-1);
			} catch (InvocationTargetException e) {
				System.err.println("ERROR: Can't invoke getter for field " + f.getName() + " for hashcode calculation.");
				e.printStackTrace();
			} catch (NoSuchMethodException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return hashCodeString.hashCode();
	}
	
	public Method getGetter(Field f) throws NoSuchMethodException {		
		Method returnMethod = null;
		String prefix = "get";
		
		if (f.getType().equals(Boolean.TYPE)){
			prefix = "is";
		}
		
		String methodName = prefix + f.getName().substring(0, 1).toUpperCase() + f.getName().substring(1);

		try {
			Class<?>[] parameters = new Class[0];
			returnMethod = this.getClass().getMethod(methodName, parameters);
		} catch (SecurityException e) {
			System.err.println("WARN: Access denied to method " + methodName);
		} catch (NoSuchMethodException e){
			//System.err.println("WARN: No getter method " + methodName);
		}
		return returnMethod;
	}
	
	public Method getSetter(Field f){
		Method returnMethod = null;
		String methodName = "set" + f.getName().substring(0, 1).toUpperCase() + f.getName().substring(1);
		try {
			Class<?>[] parameters = { f.getType() };
			returnMethod = this.getClass().getDeclaredMethod(methodName, parameters);
		} catch (SecurityException e) {
			System.err.println("WARN: Access denied to method " + methodName);
		} catch (NoSuchMethodException e) {
			System.err.println("WARN: No setter function " + methodName);
		}
		return returnMethod;
	}
	
	/*
	public Boolean proxyClassEquals(Class comparaClass){
	
		System.out.println(this instanceof HibernateProxy);
		
		
		System.out.println(this.getClass().getName() + "\t" + comparaClass.getName());
		
		if (this.getClass().equals(comparaClass)){
			return true;
		}
		
		Class<?> thisClass = this.getClass();
		
		for (Class<?> c : thisClass.getInterfaces()){
			if (c.equals(HibernateProxy.class)){
				thisClass = ((HibernateProxy)this).getHibernateLazyInitializer().getPersistentClass();
			}
		}
		
		return comparaClass.equals(thisClass);
	}
	*/

}
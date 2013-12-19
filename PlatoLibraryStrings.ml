(* TODO casting here *)
let commonClassString = 
	"public class PlatoCommon { 
	  public Object cast(Object object, String fromType, String toType) { 
		  if (fromType.equals(toType)) { 
				return object;
			} else { 
			  return null
			}
		}
	}"

let booleanClassString = 
		"public class Booleans { 
		  public Boolean not(Object bool) {
			  return !((Boolean)bool);
			}
		  public Boolean or(Object bool1, Object bool2) {
			  return ((Boolean)bool1) || ((Boolean)bool2);
			}
			public Boolean and(Object bool1, Object bool2) {
			  return ((Boolean)bool1) && ((Boolean)bool2);
			}
		}"

let integerClassString = 
		"public class Integers { 
		  public Integer negation(Integer integer) {
			  return -integer;
			}
		  public Integer plus(Object integer1, Object integer2) {
			  return ((Integer)integer1) + ((Integer)integer2);
			}
		  public Integer minus(Object integer1, Object integer2) {
			  return ((Integer)integer1) - ((Integer)integer2);
			}
		  public Integer times(Object integer1, Object integer2) {
			  return ((Integer)integer1) * ((Integer)integer2);
			}
		  public Integer divide(Object integer1, Object integer2) {
			  return ((Integer)integer1) / ((Integer)integer2);
			}
		  public Integer mod(Object integer1, Object integer2) {
			  return ((Integer)integer1) % ((Integer)integer2);
			}
		  public Integer raise(Object integer1, Object integer2) {
			  return (int) Math.pow((Integer)integer1, (Integer)integer2);
			}
		  public Boolean lessThan(Object integer1, Object integer2) {
			  return ((Integer)integer1) < ((Integer)integer2);
			}
		  public Boolean lessThanOrEqual(Object integer1, Object integer2) {
			  return ((Integer)integer1) <= ((Integer)integer2);
			}
		  public Boolean greaterThan(Object integer1, Object integer2) {
			  return ((Integer)integer1) > ((Integer)integer2);
			}
		  public Boolean greaterThanOrEqual(Object integer1, Object integer2) {
			  return ((Integer)integer1) >= ((Integer)integer2);
			}
		  public Boolean equal(Object integer1, Object integer2) {
			  return ((Integer)integer1) == ((Integer)integer2);
			}
		}"

let setLiteralsClassString = 
"public class SetLiterals {
  public static PlatoSet<Object> newPlatoSet(Object ... objects) {
  	PlatoSet<Object> returnable = new PlatoSet<Object>();
  	for(Object object : objects) {
    	returnable.add(object);
    }
    return returnable;
  }
  /**
  * Union operator
  */
  public static PlatoSet<Object> plus(PlatoSet<Object> set1, PlatoSet<Object> set2) {
  	PlatoSet<Object> returnable = new PlatoSet<Object>();
  	for(Object object : set1) {
    	returnable.add(object);
    }
    for(Object object : set2) {
    	returnable.add(object);
    }
    return returnable;
  }
  /**
  * Intersection operator
  */
  public static PlatoSet<Object> raise(PlatoSet<Object> set1, PlatoSet<Object> set2) {
  	PlatoSet<Object> returnable = new PlatoSet<Object>();
    for(Object object : set1) {
    	if(set2.contains(object)) {
    		returnable.add(object);
    	}
    }
    return returnable;
  }
  /**
  * Cartesian product operator
  */
  public static PlatoSet<Object> times(PlatoSet<Object> set1, PlatoSet<Object> set2) {
  	// TODO
  	System.out.println(\"---Set intersection feature not yet implemented. Go to PlatoLibraryStrings.ml---\");
  	PlatoSet<Object> returnable = new PlatoSet<Object>();
    return returnable;
  }
  /**
  * Set difference
  */
  public static PlatoSet<Object> setDifference(PlatoSet<Object> set1, PlatoSet<Object> set2) {
  	PlatoSet<Object> returnable = new PlatoSet<Object>();
  	for(Object object : set1) {
    	returnable.add(object);
    }
    for(Object object : set2) {
    	returnable.remove(object);
    }
    return returnable;
  }
}"

let platoSetClassString = 
"import java.util.HashSet;
public class PlatoSet<T> extends HashSet<T> {
  public String toString() {
  	String parentToString = super.toString();
  	return \"{\"+parentToString.substring(1, parentToString.length()-1)+\"}\";
  }
}"

let vectorLiteralsClassString = 
"public class VectorLiterals {
  public static PlatoVector<Object> newPlatoVector(Object ... objects) {
  	PlatoVector<Object> returnable = new PlatoVector<Object>();
  	for(Object object : objects) {
    	returnable.add(object);
    }
    return returnable;
  }
  public static Object vectorAccess(PlatoVector<Object> vec, Integer index) {
  	return vec.get(index-1);
  }
}"

let platoVectorClassString = 
"import java.util.ArrayList;
public class PlatoVector<T> extends ArrayList<T> {
}"

let groupClassString =
	"import java.util.*;
  
	public class Groups {
	  protected Map<String, String> additionTable;
	  protected Map<String, String> additiveInverseList;
	  
	  public Integer plus(Object number1, Object number2) {
	    return Integer.parseInt(additionTable.get(Integer.toString((Integer)number1) + \",\" + Integer.toString((Integer)number2))); 
	  }
	  
	  public Integer minus(Integer number1, Integer number2) {
		  return plus(number1, Integer.parseInt(additiveInverseList.get(Integer.toString(number2))));
	  }
	}"

let ringClassString = 
	"import java.util.*;
	public class Rings extends Groups {
	  protected Map<String, String> multiplicationTable;
	  
	  public Integer times(Object number1, Object number2) {
	    return Integer.parseInt(multiplicationTable.get(Integer.toString((Integer)number1) + \",\" + Integer.toString((Integer)number2))); 
	  }
	}"

let fieldClassString = 
	"import java.util.*;
	public class Fields extends Rings {
	  protected Integer additiveIdentity;
	  protected Map<String, String> multiplicitiveInverseList;
	  
	  public Integer divide(Object number1, Object number2) {
		  if (number2.equals(additiveIdentity)) {
		    throw new ArithmeticException(\"Division by zero\");
	    }
		  return times((Integer)number1, Integer.parseInt(multiplicitiveInverseList.get(Integer.toString((Integer)number2))));
	  }
	}"

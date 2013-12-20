let booleanClassString = 
		"public class Booleans { 
		  public Boolean not(Object bool) {
			  return !((Boolean)bool);
			}
			public PlatoVector<Object> not(PlatoVector<Object> bools) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object bool : bools) {
					returnable.add(!(Boolean)bool);
			  } 
				return returnable;
			}
		  public Boolean or(Object bool1, Object bool2) {
			  return ((Boolean)bool1) || ((Boolean)bool2);
			}
			public PlatoVector<Object> or(PlatoVector<Object> bools, Object bool2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object bool : bools) {
					returnable.add((Boolean)bool || (Boolean)bool2);
			  } 
				return returnable;
			}
			public PlatoVector<Object> or(Object bool2, PlatoVector<Object> bools) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object bool : bools) {
					returnable.add((Boolean)bool || (Boolean)bool2);
			  } 
				return returnable;
			}
			public Boolean and(Object bool1, Object bool2) {
			  return ((Boolean)bool1) && ((Boolean)bool2);
			}
			public PlatoVector<Object> and(PlatoVector<Object> bools, Object bool2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object bool : bools) {
					returnable.add((Boolean)bool && (Boolean)bool2);
			  } 
				return returnable;
			}
			public PlatoVector<Object> and(Object bool2, PlatoVector<Object> bools) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object bool : bools) {
					returnable.add((Boolean)bool && (Boolean)bool2);
			  } 
				return returnable;
			}
		}"

let integerClassString = 
		"public class Integers { 
		  public Integer negation(Integer integer) {
			  return -integer;
			}
			public PlatoVector<Object> negation(PlatoVector<Object> intVals) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add(-(Integer)intVal);
			  } 
				return returnable;
			}
		  public Integer plus(Object integer1, Object integer2) {
			  return ((Integer)integer1) + ((Integer)integer2);
			}
			public PlatoVector<Object> plus(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal + (Integer)intVal2);
			  } 
				return returnable;
			}
			public PlatoVector<Object> plus(Object intVal2, PlatoVector<Object> intVals) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal + (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Integer minus(Object integer1, Object integer2) {
			  return ((Integer)integer1) - ((Integer)integer2);
			}
			public PlatoVector<Object> minus(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal - (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Integer times(Object integer1, Object integer2) {
			  return ((Integer)integer1) * ((Integer)integer2);
			}
			public PlatoVector<Object> times(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal * (Integer)intVal2);
			  } 
				return returnable;
			}
			public PlatoVector<Object> times(Object intVal2, PlatoVector<Object> intVals) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal * (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Integer divide(Object integer1, Object integer2) {
			  return ((Integer)integer1) / ((Integer)integer2);
			}
			public PlatoVector<Object> divide(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal / (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Integer mod(Object integer1, Object integer2) {
			  return ((Integer)integer1) % ((Integer)integer2);
			}
			public PlatoVector<Object> mod(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal % (Integer)intVal2);
			  } 
				return returnable;
			}
			public PlatoVector<Object> mod(Object intVal2, PlatoVector<Object> intVals) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal % (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Integer raise(Object integer1, Object integer2) {
			  return (int) Math.pow((Integer)integer1, (Integer)integer2);
			}
			public PlatoVector<Object> raise(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((int) Math.pow((Integer)intVal, (Integer)intVal2));
			  } 
				return returnable;
			}
		  public Boolean lessThan(Object integer1, Object integer2) {
			  return ((Integer)integer1) < ((Integer)integer2);
			}
			public PlatoVector<Object> lessThan(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal < (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Boolean lessThanOrEqual(Object integer1, Object integer2) {
			  return ((Integer)integer1) <= ((Integer)integer2);
			}
			public PlatoVector<Object> lessThanOrEqual(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal <= (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Boolean greaterThan(Object integer1, Object integer2) {
			  return ((Integer)integer1) > ((Integer)integer2);
			}
			public PlatoVector<Object> greaterThan(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal > (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Boolean greaterThanOrEqual(Object integer1, Object integer2) {
			  return ((Integer)integer1) >= ((Integer)integer2);
			}
			public PlatoVector<Object> greaterThanOrEqual(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add((Integer)intVal >= (Integer)intVal2);
			  } 
				return returnable;
			}
		  public Boolean equal(Object integer1, Object integer2) {
			  return ((Integer)integer1).equals(((Integer)integer2));
			}
			public PlatoVector<Object> equal(PlatoVector<Object> intVals, Object intVal2) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add(((Integer)intVal).equals((Integer)intVal2));
			  } 
				return returnable;
			}
			public PlatoVector<Object> equal(Object intVal2, PlatoVector<Object> intVals) {
			  PlatoVector<Object> returnable = new PlatoVector<Object>();
				for (Object intVal : intVals) {
					returnable.add(((Integer)intVal).equals((Integer)intVal2));
			  } 
				return returnable;
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
	public static PlatoVector<Object> newPlatoVectorRange(Object start, Object end, Object increment) {
  	PlatoVector<Object> returnable = new PlatoVector<Object>();
  	for(int value = (Integer) start; value <= (Integer) end; value += (Integer) increment) {
    	returnable.add(new Integer(value));
    }
    return returnable;
  }
  public static Object vectorAccess(PlatoVector<Object> vec, Integer index) {
  	return vec.get(index-1);
  }
	public static PlatoVector<Object> vectorAccess(PlatoVector<Object> vec, PlatoVector<Object> indexer) {
  	PlatoVector<Object> returnable = new PlatoVector<Object>();
		for (int index = 0; index < vec.size(); index++) {
		  if ((Boolean) indexer.get(index)) {
				returnable.add(vec.get(index));
			}
		}
		return returnable;
  }
	public static PlatoVector<Object> at(PlatoVector<Object> vec, Object val) {
		 PlatoVector<Object> returnable = vec;
		 returnable.add(val);
		 return returnable;
	}
  public static PlatoVector<Object> at(Object val, PlatoVector<Object> vec) {
		 PlatoVector<Object> returnable = vec;
		 returnable.add(val);
		 return returnable;
	}
 public static PlatoVector<Object> at(PlatoVector<Object> vec1, PlatoVector<Object> vec2) {
		 PlatoVector<Object> returnable = vec1;
		 returnable.addAll(vec2);
		 return returnable;
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

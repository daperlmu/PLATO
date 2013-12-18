(* TODO casting here *)
let commonClassString = 
	"public class PlatoCommon { \n
	  public Object cast(Object object, String fromType, String toType) { \n
		  if (fromType.equals(toType)) { \n
				return object;
			} else { \n
			  return null
			}\n
		}\n
	}"
		

let booleanClassString = 
		"public class Booleans { \n
		  public Boolean not(Boolean bool) {\n
			  return !bool;\n
			}\n
		  public Boolean or(Boolean bool1, Boolean bool2) {\n
			  return bool1 || bool2;\n
			}\n
			public Boolean and(Boolean bool1, Boolean bool2) {\n
			  return bool1 && bool2;\n
			}\n
		}"
		
let integerClassString = 
		"public class Integers { \n
		  public Integer negation(Integer integer) {\n
			  return -integer;\n
			}\n
		  public Integer plus(Integer integer1, Integer integer2) {\n
			  return integer1 + integer2;\n
			}\n
		  public Integer minus(Integer integer1, Integer integer2) {\n
			  return integer1 - integer2;\n
			}\n
		  public Integer times(Integer integer1, Integer integer2) {\n
			  return integer1 * integer2;\n
			}\n
		  public Integer divide(Integer integer1, Integer integer2) {\n
			  return integer1 / integer2;\n
			}\n
		  public Integer mod(Integer integer1, Integer integer2) {\n
			  return integer1 % integer2;\n
			}\n
		  public Integer raise(Integer integer1, Integer integer2) {\n
			  return (int) Math.pow(integer1, integer2);\n
			}\n
		  public Boolean lessThan(Integer integer1, Integer integer2) {\n
			  return integer1 < integer2;\n
			}\n
		  public Boolean lessThanOrEqual(Integer integer1, Integer integer2) {\n
			  return integer1 <= integer2;\n
			}\n
		  public Boolean greaterThan(Integer integer1, Integer integer2) {\n
			  return integer1 > integer2;\n
			}\n
		  public Boolean greaterThanOrEqual(Integer integer1, Integer integer2) {\n
			  return integer1 >= integer2;\n
			}\n
		  public Boolean equal(Integer integer1, Integer integer2) {\n
			  return integer1 == integer2;\n
			}\n
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

let groupClassString =
	"import java.util.*;\n
  \n
	public class Groups {\n
	  protected Map<String, String> additionTable;\n
	  protected Map<String, String> additiveInverseList;\n
	  \n
	  public Integer plus(Integer number1, Integer number2) {\n
	    return Integer.parseInt(additionTable.get(Integer.toString(number1) + \",\" + Integer.toString(number2)));\n 
	  }\n
	  \n
	  public Integer minus(Integer number1, Integer number2) {\n
		  return plus(number1, Integer.parseInt(additiveInverseList.get(Integer.toString(number2))));\n
	  }\n
	}\n"

let ringClassString = 
	"import java.util.*;\n
	public class Rings extends Groups {\n
	  protected Map<String, String> multiplicationTable;\n
	  \n
	  public Integer times(Integer number1, Integer number2) {\n
	    return Integer.parseInt(multiplicationTable.get(Integer.toString(number1) + \",\" + Integer.toString(number2)));\n 
	  }\n
	}\n"
	
let fieldClassString = 
	"import java.util.*;\n
	public class Fields extends Rings {\n
	  protected Map<String, String> multiplicitiveInverseList;\n
	  \n
	  public Integer divide(Integer number1, Integer number2) {\n
		  return times(number1, Integer.parseInt(multiplicitiveInverseList.get(Integer.toString(number2))));\n
	  }\n
	}\n"

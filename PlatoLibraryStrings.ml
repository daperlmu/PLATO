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
		  public static Boolean not(Boolean bool) {\n
			  return !bool;\n
			}\n
		  public static Boolean or(Boolean bool1, Boolean bool2) {\n
			  return bool1 || bool2;\n
			}\n
			public static Boolean and(Boolean bool1, Boolean bool2) {\n
			  return bool1 && bool2;\n
			}\n
		}"
		
let integerClassString = 
		"public class Integers { \n
		  public static Integer negation(Integer integer) {\n
			  return -integer;\n
			}\n
		  public static Integer plus(Integer integer1, Integer integer2) {\n
			  return integer1 + integer2;\n
			}\n
		  public static Integer minus(Integer integer1, Integer integer2) {\n
			  return integer1 - integer2;\n
			}\n
		  public static Integer times(Integer integer1, Integer integer2) {\n
			  return integer1 * integer2;\n
			}\n
		  public static Integer divide(Integer integer1, Integer integer2) {\n
			  return integer1 / integer2;\n
			}\n
		  public static Integer mod(Integer integer1, Integer integer2) {\n
			  return integer1 % integer2;\n
			}\n
		  public static Integer raise(Integer integer1, Integer integer2) {\n
			  return (int) Math.pow(integer1, integer2);\n
			}\n
		  public static Boolean lessThan(Integer integer1, Integer integer2) {\n
			  return integer1 < integer2;\n
			}\n
		  public static Boolean lessThanOrEqual(Integer integer1, Integer integer2) {\n
			  return integer1 <= integer2;\n
			}\n
		  public static Boolean greaterThan(Integer integer1, Integer integer2) {\n
			  return integer1 > integer2;\n
			}\n
		  public static Boolean greaterThanOrEqual(Integer integer1, Integer integer2) {\n
			  return integer1 >= integer2;\n
			}\n
		  public static Boolean equal(Integer integer1, Integer integer2) {\n
			  return integer1 == integer2;\n
			}\n
		}"
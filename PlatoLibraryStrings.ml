(* TODO casting here *)
let commonClassString = 
	"public class PlatoCommpn { \n
	  public Object cast(Object object, String fromType, String toType) { \n
		  if (fromType.equals(toType)) { \n
				return object;
			} else { \n
			  return null
			}\n
		}\n
	}"
		

let booleanClassString = 
		"public class PlatoBoolean { \n
		  public Boolean not(Boolean boolean) {\n
			  return !boolean;\n
			}\n
		  public Boolean or(Boolean boolean1, Boolean boolean2) {\n
			  return boolean1 || boolean2;\n
			}\n
			public Boolean and(Boolean boolean1, Boolean boolean2) {\n
			  return boolean1 && boolean2;\n
			}\n
		}"
		
let integerClassString = 
		"public class PlatoInteger { \n
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
			  return Math.pow(integer1, integer2);\n
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
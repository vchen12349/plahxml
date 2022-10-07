import java.lang.reflect.Constructor
import javax.lang.model.`type`.TypeVariable
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, api, classTag}
import scala.reflect.runtime.{universe => ru}
import scala.xml.{Elem, NodeSeq}
import scala.reflect.runtime.universe._

object fromXmlTemplate {
  val mirror = ru.runtimeMirror(getClass.getClassLoader())

  def fromXML[T: TypeTag: ClassTag](/*c: Class[T],*/ elem:NodeSeq  ): T = {
    println(s" --- BEGIN UGH ---")
     val t =  typeTag[T]
    //convert a type tag to a type
    val requestType = t.tpe

    //just pring out the params for case class
    requestType.decls.foreach(a => {


    /**
     * Were setting up all the methods so we an call "apply" on the child xml models in the constructor
     */
    val primCtor = requestType.decls.filter(
      m => m.isMethod && m.asMethod.isPrimaryConstructor).head

    val params = primCtor.typeSignature.paramLists.head

    val tmp = requestType.typeSymbol.asType.asClass
    val cm = mirror.reflectClass(tmp)

    val applyMethodToCall = cm.reflectConstructor(primCtor.asMethod)
//    val res = applyMethodToCall.apply(lst: _*) //a method would would look like smeting like this, but not here.


    //This list buffer is to maintain a list of all of the arguments erquired to create the XML object.
    //So by looping through all the constructors, we'll need to  know exactly what we need to pass in for the
    //apply method.  So in the simpliest case.  It might be zipcode.apply (Option[]String, Option[PostalCode])
    //But those to params need to be added to teh eparamList so that when we actually instantiate the class
    //below we have the values to do so.
    var paramList:ListBuffer[Any] = new ListBuffer();

    /** start loop through the constructor params.  Like WorldpayRequest may have, ZipcDCode, "attributes", Payment, root value, etc */
    //constructor param list - option[string], option[billingaddress]
    params.foreach(
      a => { //constructor param
        val paramName = a.name
        val paramType = a.typeSignature
        //handle scenario where it is option string (or attributes)
        //get some value from xml
        //look up "paramName". hardcode for now

      if (!(a.typeSignature == ru.typeOf[Option[String]])) {
        //Once we get here, we've identified it's an Option[SomeObject] and not a Option[String] which without the
        //Root value  annotation would be an attribute.  We need to decode this SomeObject some more.

        //for option
        println("It is not an option of string" + a.info.typeArgs) //for this constructor a, give me a param list. of say List[Payment]

        //just Payment of reflect.Type  //we want to only take the head, due to currying which will give  you lists of lists, etc.
        val pymt = a.info.typeArgs.head //This will just give us "Payment" type.  It is a representation of an XML object (not a string or attribute) within the child XML objects constructor

        val tstClass =  pymt.typeSymbol.asType.asClass //convert the XML Object's type into a ClassSymbol

        val payConst = pymt.members.filter(a => a.isMethod && a.asMethod.isPrimaryConstructor).head
          val clmir = ru.runtimeMirror(getClass.getClassLoader).reflectClass(tstClass)
        println("class name " +  clmir.symbol.toType) //print's out somehting like BusinessAddress
        val conme = clmir.reflectConstructor(payConst.asMethod) //constructor is BillingAdderss

        //???!1 I don't understand why this apply method and take a different number of arguments then actual object.
        val res = conme.apply(None, None, None, None, None, None, None, None, None, None) // instance of BillingAddress

          val child = res.getClass.getName //his the name as an XML / "ZipCode" (ans an example) to slim down the XML we're going to parse recursively.
         val c = typeToClassTag[T] //we need this to maintain the class type of what we're calling recursively.  Otherwise T would just erasure it.

        //the fromXML call requires 2 implicits.  A TypeTag and a ClassTag .  returning type T won't work
        //due to type erasure.  We kind of need to hack around it.
          val plah = fromXML(/*res.getClass,*/ elem \ {child})(fromTypeToTyeTag(pymt), c)
//        }
        paramList += Some(plah) //Add the element to the param list so that when we actually build the final object. we have the list
      } // end if not string
      else {
        //Start part where it is not a nested XML object like BillingAddres or ZipCode, but just a String which means either
        //a value between xml tags or an attribute.
        //If it has an annotation.  Right now, there is only RootNode.
        //RootNode annotation will only evaluate to the xml value.  not attributes.  so like <xml>VALUE</xml>
        if (a.annotations.size > 0) {
          val ugh = requestType.toString
          val p = (elem).text
          paramList += Some(p)
        }
         else {
          //if it's an annotation, then it is an attribute.
          val attr = s"""@${a.name.toString}"""
          val t = elem \ {
            attr
          }
          if (t.size == 0)
            paramList +=None
            else {
          paramList += Some(t)
          //Against, we add it to the param list so when we are done recursing  we have the param value list all built up.
          }
        }
    } // end else it is a string
    }) //end huge loop around constructor of nested XML object

    //Start recursively creating the XML objects
    println(s"ListBuffer: ${paramList.toList}")
    //Apply the param List as values to the apply method.
    applyMethodToCall.apply(paramList.toList: _*).asInstanceOf[T] //note casting it to type T wouldn't have been possible without passing in as implicits line 93
  }

  def getType[T](clazz: Class[T]) =
    mirror.classSymbol(clazz).toType

  //    ru.runtimeMirror(clazz.getClassLoader).classSymbol(clazz).toType

  def fromTypeToTyeTag[T](tpe: Type): TypeTag[T] =
    TypeTag(mirror, new api.TypeCreator {
      def apply[U <: api.Universe with Singleton](m: api.Mirror[U]) =
        if (m eq mirror) tpe.asInstanceOf[U#Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    })

  def typeToClassTag[T: TypeTag]: ClassTag[T] = {
    ClassTag[T](typeTag[T].mirror.runtimeClass(typeTag[T].tpe))
  }
}

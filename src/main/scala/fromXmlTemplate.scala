import java.lang.reflect.Constructor
import javax.lang.model.`type`.TypeVariable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}
import scala.xml.{Elem, NodeSeq}

object fromXmlTemplate {
  def fromXML(c: Class[_], elem:NodeSeq  ): XMLTextTemplate = {
    val mirror = ru.runtimeMirror(getClass.getClassLoader())
    println(s" --- BEGIN UGH ---")


    println(elem)
    //    val requestType = mirror.classSymbol(c.getClass).toType
    println(getType(c))
    val requestType = getType(c)
//    requestType.decls.foreach(a => println(a))
//    classSymbol.info.decls.foreach(a => println(a))

    //just pring out the params for case class
    requestType.decls.foreach(a => {
      //gets list of all case class constructor param NAMES, like "orderCode", "be"
      if(a.isMethod && a.asMethod.isGetter)
      println("method param field names: " + a.name)
      else
        Unit
    })
    /** end logging */



    val primCtor = requestType.decls.filter(
      m => m.isMethod && m.asMethod.isPrimaryConstructor).head

    println(primCtor)

    val params = primCtor.typeSignature.paramLists.head

    println(params.size)

    val tmp = requestType.typeSymbol.asType.asClass
    val cm = mirror.reflectClass(tmp)
    val applyMethodToCall = cm.reflectConstructor(primCtor.asMethod)
    val lst = List.fill(params.size)(None)
//    val res = applyMethodToCall.apply(lst: _*)

//    println(s"Created class ${createdClass.getClass}")
//    println("asdf" + primCtor.typeSignature.typeArgs)

    /** start loop */
    //constructor param list - option[string], option[billingaddress]
    var paramList:ListBuffer[Any] = new ListBuffer();

    params.foreach(
      a => { //constructor param
        val paramName = a.name
        val paramType = a.typeSignature
      println(a.name)
      println(a.typeSignature)
      println(a.typeSignature == ru.typeOf[Option[String]]) //[Option[_]] doesn't work

        //handle scenario where it is option string (or attributes)
        //get some value from xml
        //look up "paramName". hardcode for now




        //handle Option[BilingAddress]  we want the type
      if (!(a.typeSignature == ru.typeOf[Option[String]])) {
        a.typeSignature.members.foreach(b => {
//          println("444 " + b) //this will print out methods of option
        })
        //for option
        println(a.info.typeArgs) //List(Payment)
        val pymt = a.info.typeArgs.head //just Payment of reflect.Type
//        pymt.members.foreach(a => println("plah" + a)) // all members of Payment

        val tstClass =  pymt.typeSymbol.asType.asClass
        println(tstClass.name)
        //for payment
        val payConst = pymt.members.filter(a => a.isMethod && a.asMethod.isPrimaryConstructor).head
          val clmir = ru.runtimeMirror(getClass.getClassLoader).reflectClass(tstClass)
//          val conme = clmir.reflectConstructor(conscl) //constructor is BillingAdderss
        val conme = clmir.reflectConstructor(payConst.asMethod) //constructor is BillingAdderss
        val res = conme.apply(Some("poo"), None, None, None, None, None, None) // instance of BillingAddress

        //        val res = conme.symbol.info.foreach(a => {
//          println("234" + a.typeArgs)
//        }) // instance of BillingAddress
          println(res.getClass)
//        val plah = if (requestType.toString == elem.head.label)
//          fromXML(res.getClass, elem)
//        else {
          val child = res.getClass.getName
        println("poo: " + child)
          val plah = fromXML(res.getClass, elem \ {child})
//        }
        paramList += Some(plah)

        println(s"End loop ${plah}")
      } // end if not string
      else {
        // if it is string
//        val oc = "@orderCode"
//        println(xmlObj \ "@orderCode")
//        println(xmlObj \ {
//          oc
//        })
        println("It is string")

        println("annotations" + a.annotations)
        if (a.annotations.size > 0) {
          val ugh = requestType.toString
          println("ugh " + ugh)
          val p = (elem).text
          paramList += Some(p)
          println(s"Adding to root paramlist ${p}")

        }
         else {
          val attr = s"""@${a.name.toString}"""
          val t = elem \ {
            attr
          }
          paramList += Some(t)
          println(s"Adding to paramlist ${t}")

        }

        null
      }
    }) //end huge loop around to level object
//    println("End function Created Class " + createdClass.getClass)
    println(s"ListBuffer: ${paramList.toList}")
    applyMethodToCall.apply(paramList.toList: _*).asInstanceOf[XMLTextTemplate]
//    createdClass.asInstanceOf[XMLTextTemplate]
  }

  def getType[T](clazz: Class[T]) =
    ru.runtimeMirror(clazz.getClassLoader).classSymbol(clazz).toType
}

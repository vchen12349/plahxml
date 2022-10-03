import scala.reflect.runtime.{universe => ru}

 trait XMLTextTemplate {

   val instanceSymbol = ru.runtimeMirror(this.getClass.getClassLoader).classSymbol(this.getClass)
   val primCtor = instanceSymbol.info.decls.filter(m => m.isMethod && m.asMethod.isPrimaryConstructor).head
   val instanceMirror = ru.runtimeMirror(this.getClass.getClassLoader).reflect(this)

   val params = primCtor.typeSignature.paramLists.head


   def getEndTag(): String = {
    s"</${this.getClass.getName}>"
  }

  //list of options option[string], option[xmltemplate], none, etc but take only the xmltempaltes
  def children(lst: List[Any]): List[XMLTextTemplate] = {
    val asd = lst.asInstanceOf[List[Option[_]]]
    val optinst = asd.filter(a => a.nonEmpty && a.get.isInstanceOf[XMLTextTemplate])
    val asdf = optinst.map(a => a.get.asInstanceOf[XMLTextTemplate])
    asdf
  }

   def getRootNodeValue:Option[String] = {
     val paramValues = params.map(p => {
       val getterMethod = instanceSymbol.info.decls.filter(m => m.isMethod && m.name == p.name && m.asMethod.isGetter).head

       println("root check 444 " + p.name) //name of the fields
       println("isRoot: " + p.annotations.filter(nm => nm.toString == "RootValue").nonEmpty)
        val isRoot = p.annotations.filter(nm => nm.toString == "RootValue").nonEmpty
       val propValue = if (isRoot)instanceMirror.reflectMethod(getterMethod.asMethod)()
       else None
       //value of the field as option
       println("root checkpropvalue type " + propValue.asInstanceOf[Option[_]].getOrElse("Empty"))
       propValue
     })
     //We got the constructor params and values!
     println(s"prop values ${paramValues}")
     paramValues.head.asInstanceOf[Option[String]]
   }

   //runs the "non value" part of the xml (not the attributes) but the nodes inbetween
   //<>process this part here<>.  runs converToXML on children
  def convertToXML(): String = {
     println("THE ROOT NODE: " + getRootNodeValue)

     val rootNode: Option[String] = getRootNodeValue

    println("params: " + params)

    //paramValues = List of (Option[XMLTEXTTemplateType]) .. well, actually _ , but it will be TEmoplate class
    val paramValues = params.map(p => {
      val getterMethod = instanceSymbol.info.decls.filter(
        m => m.isMethod && m.name == p.name && m.asMethod.isGetter).head
      println("444 " + p.name) //name of the fields
      println("annotations: " + p.annotations)
      val propValue = instanceMirror.reflectMethod(getterMethod.asMethod)()
      //value of the field as option
      println("propvalue type " + propValue.asInstanceOf[Option[_]].getOrElse("Empty"))
      propValue
    })
    //We got the constructor params and values!
    println(s"prop values ${paramValues}")

//    val actualClass = this.getClass
    println("getclass " + this.getClass())
    val mirr: ru.Mirror = ru.runtimeMirror(this.getClass.getClassLoader)
    val methodSymbol = ru.typeOf[XMLTextTemplate].decl(ru.TermName("convertToXML")).asMethod
    println(s"--- ${methodSymbol}")
    println(children(paramValues))
    //paramValues = List of (Option[XMLTEXTTemplateType])
    //All XmlTextTemplate types are considered to be NodeChildren.  Attributes are handled elsewhere
    //loop through those and get all the xml node children and  convert that XMLNode
     //To a children so like
     //<parent><XMLNode></XMLNode></parent> - it will print out the XMLNode section
     //Note this allows for nested method calling.

    val totalString = if (children(paramValues).nonEmpty) {
      println("not empty")
      children(paramValues).foldLeft("")((accum, elem) => {
      val instMirr = mirr.reflect(elem)
      println(s"elem ${elem}")
      //runs the method convertToXML method within this loop using the method
        // represented by methodSymbol and passing in elem as a value if needed.

//      val mm = instMirr.reflectMethod(methodSymbol.asMethod) (elem)
        val mm = instMirr.reflectMethod(methodSymbol.asMethod) ()

      accum + s"${mm}"
    })
    } else rootNode.getOrElse("NOT FOUND") // this is the root value

    getBeginTag() + s"${totalString}" + getEndTag()
  }

  def getBeginTag(): String = {
    s"<${this.getClass.getName} ${addAttributes2}>"
  }

   def addAttributes2(): String = {

     val attributeString = params
       .foldLeft("")((accum, elem) => {
         val getterMethod = instanceSymbol.info.decls.filter(m => m.isMethod && m.name == elem.name && m.asMethod.isGetter).head
         val isRoot = elem.annotations.filter(nm => nm.toString == "RootValue").nonEmpty
         //only get value of property if it doesn't have a root annotation
         val propValue = if (!isRoot) {
           instanceMirror.reflectMethod(getterMethod.asMethod)()
         }
         else None
         //value of the field as option
//         println("blah: root checkpropvalue type " + propValue.asInstanceOf[Option[_]].getOrElse("Empty"))
         val opt = propValue.asInstanceOf[Option[_]]
         if (opt.nonEmpty && !opt.getOrElse("").isInstanceOf[XMLTextTemplate])
           accum + s""" ${elem.name}="${opt.get}""""
         else
           accum
       })
     attributeString
   }

}

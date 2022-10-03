import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.xml.{Elem, XML}
object main {
  def main(args: Array[String]) = {
    val personInterfaceType: ru.MemberScope = ru.typeOf[WorldpayRequest].decls
    val personClassType = ru.typeOf[WorldpayRequest].decls

//    personClassType.foreach { symbol =>
//      println(s"decla${} red member: ${symbol.name}, type signature: ${symbol.typeSignature.toString}")
//    }
//    wr.getClass.getDeclaredMethods.foreach{
//      aa => println("asdf" + aa.toString)
//    }

//    val fields = wr.getClass.getDeclaredFields.foreach( {
//      asdf => {
//        asdf.setAccessible(true)
//        println(s"asdf " + asdf.getName + s" ${asdf.get(wr).asInstanceOf[String]}")
//      }
//    })


    val z = ZipCode(somezip = Some("07974") )
    val ba = BillingAddress(Some("someaddressid"), Some(z))
    val wr = WorldpayRequest(
      orderCode = Some("orderCode123"),
      currency = Some("USD"), stuff = None, be = Some(ba), paymentType = Some(Payment(Some("ECMC"))))

    println(wr.convertToXML())


    val xmlStr =
      """
        |<WorldpayRequest  orderCode="orderCode123" currency="USD"><BillingAddress  addressId="someaddressid"><ZipCode >07974</ZipCode></BillingAddress><Payment >ECMC</Payment></WorldpayRequest>
        |""".stripMargin
     val xmlObj:Elem =    XML.loadString(xmlStr)
    val worldpayRequest = fromXmlTemplate.fromXML(classOf[WorldpayRequest], xmlObj).asInstanceOf[WorldpayRequest]
    println(worldpayRequest)

//    println(wr.doBody(ba))
//    wr.chil

//    println(s"Hello, world ${value}")
  }
}

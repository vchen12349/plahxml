import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.xml.{Elem, XML}
object main {
  def main(args: Array[String]) = {

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
//    val worldpayRequest = fromXmlTemplate.fromXML(classOf[WorldpayRequest], xmlObj).asInstanceOf[WorldpayRequest]
    val worldpayRequest = fromXmlTemplate.fromXML[WorldpayRequest](/*classOf[WorldpayRequest], */
      xmlObj)

    println(worldpayRequest)
    println("curenty" + worldpayRequest.currency.get)
    println(s"zipcode ${worldpayRequest.be.get.zip.get.somezip.get}")
    println(s"Pament type ${worldpayRequest.paymentType.get.paymentType.get}")
  }
}

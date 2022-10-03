
import scala.reflect.runtime.universe.MethodSymbol
import scala.reflect.runtime.universe._
import collection.JavaConverters._

object WorldpayRequest {
  def apply() = new WorldpayRequest(None, None, None, None,  None)
}

case class WorldpayRequest(orderCode: Option[String], currency: Option[String],
                           stuff: Option[String],
                           be: Option[BillingAddress], paymentType: Option[Payment])
extends XMLTextTemplate {

//  override def children(): List[XMLTextTemplate] = {
//    List(BillingAddress(Some("addressId")))
//  }
}

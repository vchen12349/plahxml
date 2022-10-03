//object BillingAddress {
//   def apply() = new BillingAddress(None, None)
//}

case class BillingAddress(addressId: Option[String], zip: Option[ZipCode])
  extends XMLTextTemplate  {

//   def this() = this(None, None)
//  override def children(): List[XMLTextTemplate] = {
//    List(ZipCode(Some("someZipAttribute")))
//  }
}

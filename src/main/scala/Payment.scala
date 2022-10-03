case class Payment(@RootValue paymentType: Option[String])  extends XMLTextTemplate {
  def this() = {
    this(None)
  }
}


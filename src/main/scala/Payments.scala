package poca

import java.util.UUID
import poca.{MyDatabase, QuantityNotSufficientException}
import slick.lifted.Tag

import scala.concurrent.Future
import slick.jdbc.PostgresProfile.api._
import java.util.UUID
import java.sql.Timestamp

import scala.concurrent.Future


case class Payment(paymentId: String, orderId: String, creditCardNumber: String, securityCode: String, cardExpiration: String)


class Payments {

  class PaymentsTable(tag: Tag) extends Table[(String, String, String, String, String)](tag, "payment") {
    def paymentId = column[String]("payment_id", O.PrimaryKey)
    def orderId = column[String]("order_id")
    def creditCardNumber = column[String]("credit_card_number")
    def securityCode = column[String]("security_code")
    def cardExpiration = column[String]("card_expiration")


    def * = (paymentId, orderId, creditCardNumber, securityCode, cardExpiration)
  }

  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  val db = MyDatabase.db
  val payments = TableQuery[PaymentsTable]

    def createPayment(orderId: String, creditCardNumber: String, securityCode: String, cardExpiration: String): Future[Unit] = {
    val id = UUID.randomUUID.toString()
    val newPayment = Payment(paymentId = id, orderId = orderId, creditCardNumber = creditCardNumber, securityCode = securityCode, cardExpiration = cardExpiration)
    val newPaymentTuple: (String, String, String, String, String) = Payment.unapply(newPayment).get

    val dbio: DBIO[Int] = payments += newPaymentTuple
    var resultFuture: Future[Int] = db.run(dbio)
    resultFuture.map(_ => ())
  }


}


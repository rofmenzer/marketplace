package poca

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import com.typesafe.scalalogging.LazyLogging
import slick.jdbc.PostgresProfile.api._
import java.sql.Timestamp

import poca.Migration

class MigrationPayment(db: Database) extends Migration with LazyLogging {

    class CurrentPaymentTable(tag: Tag) extends Table[(String, String, String, String, String)](tag, "payment") {
      def paymentId = column[String]("payment_id", O.PrimaryKey)
      def orderId = column[String]("order_id")
      def creditCardNumber = column[String]("credit_card_number")
      def securityCode = column[String]("security_code")
      def cardExpiration = column[String]("card_expiration")

      def * = (paymentId, orderId, creditCardNumber, securityCode, cardExpiration)
    }

    override def apply(): Unit = {
      implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
      val payment = TableQuery[CurrentPaymentTable]
      val paymentDbio: DBIO[Unit] = payment.schema.createIfNotExists
      val orderCreationFuture: Future[Unit] = db.run(paymentDbio)
      Await.result(orderCreationFuture, Duration.Inf)
      logger.info("Done creating table payment")
    }

}

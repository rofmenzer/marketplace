package poca

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import com.typesafe.scalalogging.LazyLogging
import slick.jdbc.PostgresProfile.api._
import java.sql.Timestamp

import poca.Migration

class MigrationShipping(db: Database) extends Migration with LazyLogging {

    class CurrentShippingTable(tag: Tag) extends Table[(String, String, String, String, String, String,String,String)](tag, "shipping") {
      def shippingId = column[String]("shipping_id", O.PrimaryKey)
      def bookId = column[String]("book_id")
      def address = column[String]("address")
      def zipcode = column[String]("zipcode")
      def mail = column[String]("mail")
      def city = column[String]("city")
      def phone = column[String]("phone")
      def status = column[String]("status")

      def * = (shippingId, bookId, address, zipcode, mail, city,phone,status)
    }

    override def apply(): Unit = {
      implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
      val shipping = TableQuery[CurrentShippingTable]
      val shippingDbio: DBIO[Unit] = shipping.schema.createIfNotExists
      val orderCreationFuture: Future[Unit] = db.run(shippingDbio)
      Await.result(orderCreationFuture, Duration.Inf)
      logger.info("Done creating table shipping")
    }

}

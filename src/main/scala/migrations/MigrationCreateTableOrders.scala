package poca

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import com.typesafe.scalalogging.LazyLogging
import slick.jdbc.PostgresProfile.api._
import java.sql.Timestamp

class MigrationCreateTableOrders(db: Database) extends Migration with LazyLogging {

    class CurrentOrdersTable(tag: Tag) extends Table[(String, String, String, String, Int, String)](tag, "orders") {
        def orderId = column[String]("order_id", O.PrimaryKey)
        def bookId = column[String]("book_id")
        def userId = column[String]("user_id")
        def date = column[String]("date")
        def quantity = column[Int]("quantity")
        def status = column[String]("status")
        def * = (orderId, bookId, userId, date, quantity, status)
    }

    override def apply(): Unit = {
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        val orders = TableQuery[CurrentOrdersTable]
        val orderDbio: DBIO[Unit] = orders.schema.createIfNotExists
        val orderCreationFuture: Future[Unit] = db.run(orderDbio)
        Await.result(orderCreationFuture, Duration.Inf)
        logger.info("Done creating table Orders")

        //update table with new column
        val setVersionRequest = sqlu"ALTER TABLE orders ADD COLUMN IF NOT EXISTS amount Double precision;"
        val setVersionFuture: Future[Int] = db.run(setVersionRequest)
        Await.result(setVersionFuture, Duration.Inf)
        logger.info("Done updating table orders with amount column")
    }
}

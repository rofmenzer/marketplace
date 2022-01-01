
package poca

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import com.typesafe.scalalogging.LazyLogging
import slick.jdbc.PostgresProfile.api._


class MigrationCreateTableBook(db: Database) extends Migration with LazyLogging {

    class CurrentBooksTable(tag: Tag) extends Table[(String, String, String, String, String, String, Double)](tag, "books") {
        def id = column[String]("id", O.PrimaryKey)
        def title = column[String]("title")
        def author = column[String]("author")
        def synopsis = column[String]("synopsis")
        def year = column[String]("year")
        def isbn = column[String]("isbn")
        def price= column[Double]("price")
        def * = (id, title, author, synopsis, year, isbn, price)
    }

    override def apply(): Unit = {
        implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
        val books = TableQuery[CurrentBooksTable]
        val bookDbio: DBIO[Unit] = books.schema.createIfNotExists
        val bookCreationFuture: Future[Unit] = db.run(bookDbio)
        Await.result(bookCreationFuture, Duration.Inf)
        logger.info("Done creating table Books")

        val setVersionRequest = sqlu"ALTER TABLE books ADD COLUMN IF NOT EXISTS quantity INTEGER;"
        val setVersionFuture: Future[Int] = db.run(setVersionRequest)
        Await.result(setVersionFuture, Duration.Inf)
        logger.info("Done updating table Books with quantity column")

    }
}

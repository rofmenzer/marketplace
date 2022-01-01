
package poca

import com.typesafe.scalalogging.LazyLogging
import slick.jdbc.PostgresProfile.api._

import java.util.UUID
import scala.concurrent.Future




case class Book(id: String, title: String, author: String, synopsis: String, year: String, isbn: String, price: Double, quantity: Int)


final case class BookAlreadyExistsException(private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class InconsistenBooktStateException(private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

class Books extends LazyLogging{
  val db = MyDatabase.db

  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  val books = TableQuery[BooksTable]

  def getBookById(id: String): Future[Option[Book]] = {
    val query = books.filter(_.id === id)

    val bookListFuture = db.run(query.result)

    bookListFuture.map((bookList: Seq[(String, String, String, String, String, String, Double, Int)]) => {
      bookList.length match {
        case 0 => None
        case 1 => Some(Book tupled bookList.head)
        case _ => throw new InconsistenBooktStateException(s"id $id is linked to several books in database!")
      }
    })
  }

  def getAllBooks(): Future[Seq[Book]] = {
    val bookListFuture = db.run(books.result)

    bookListFuture.map((bookList: Seq[(String, String, String, String, String, String, Double, Int)]) => {
      bookList.map(Book tupled _)
    })
  }

  def updateBook(id: String, newBook: Book): Future[Int] = {


    db.run(books.filter(_.id === id).map(book => (book.title, book.author, book.synopsis, book.year, book.isbn, book.price, book.quantity))
      .update((newBook.title, newBook.author, newBook.synopsis, newBook.year, newBook.isbn, newBook.price, newBook.quantity)))


  }

  def createBook(title: String, author: String, synopsis: String, year: String, isbn: String, price: Double, quantity: Int): Future[Unit] = {
    val existingBooksFuture = getBookByTitle(title)

    existingBooksFuture.flatMap(existingBooks => {
      if (existingBooks.isEmpty) {
        val id = UUID.randomUUID.toString()
        val newBook = Book(id = id, title = title, author = author, synopsis = synopsis, year = year, isbn = isbn, price = price, quantity = quantity)
        val newBookAsTuple: (String, String, String, String, String, String, Double, Int) = Book.unapply(newBook).get

        val dbio: DBIO[Int] = books += newBookAsTuple
        var resultFuture: Future[Int] = db.run(dbio)

        // We do not care about the Int value
        resultFuture.map(_ => ())
      } else {
        throw new BookAlreadyExistsException(s"A book with title '$title' already exists.")
      }
    })
  }

  def delete(boookId: String): Future[Unit] ={
        val bookDeleteFuture = books.filter(_.id === boookId)
        val action = bookDeleteFuture.delete
        val affectedRowsCount: Future[Int] = db.run(action)
        val sql = action.statements.head
        logger.info("book with id "+boookId+" was deleted")
        logger.info(sql)
        affectedRowsCount.map(_ => ())
    }
  def getBookByTitle(title: String): Future[Option[Book]] = {
    val query = books.filter(_.title === title)

    val bookListFuture = db.run(query.result)

    bookListFuture.map((bookList: Seq[(String, String, String, String, String, String, Double, Int)]) => {
      bookList.length match {
        case 0 => None
        case 1 => Some(Book tupled bookList.head)
        case _ => throw new InconsistenBooktStateException(s"title $title is linked to several books in database!")
      }
    })
  }


  class BooksTable(tag: Tag) extends Table[(String, String, String, String, String, String, Double, Int)](tag, "books") {
    def * = (id, title, author, synopsis, year, isbn, price, quantity)

    def id = column[String]("id", O.PrimaryKey)

    def title = column[String]("title")

    def author = column[String]("author")

    def synopsis = column[String]("synopsis")

    def year = column[String]("year")

    def isbn = column[String]("isbn")

    def price = column[Double]("price")

    def quantity = column[Int]("quantity")
  }


}

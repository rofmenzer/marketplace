package poca

import java.util.UUID
import poca.{Book, BookAlreadyExistsException, BookNotExistException, Books, MyDatabase, Order, QuantityNotSufficientException}
import slick.lifted.Tag

import scala.concurrent.Future
import slick.jdbc.PostgresProfile.api._
import java.util.UUID
import java.sql.Timestamp

import scala.concurrent.Future


case class Shipping(shippingId: String, bookId: String, address: String, zipcode: String, mail: String, city: String, phone: String,status: String)


class Shippings {

  class ShippingsTable(tag: Tag) extends Table[(String, String, String, String, String, String, String, String)](tag, "shipping") {
    def shippingId = column[String]("shipping_id", O.PrimaryKey)

    def bookId = column[String]("book_id")

    def address = column[String]("address")

    def zipcode = column[String]("zipcode")

    def mail = column[String]("mail")

    def city = column[String]("city")

    def phone = column[String]("phone")

    def status = column[String]("status")


    def * = (shippingId, bookId, address, zipcode, mail, city, phone, status)
  }

  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  val db = MyDatabase.db
  val shippings = TableQuery[ShippingsTable]
  var books = new Books()


  def createShipping(bookId: String, address: String, zipcode: String, mail: String, city: String, phone: String, status: String): Future[Unit] = {
    val id = UUID.randomUUID.toString()
    val newShipping = Shipping(shippingId = id, bookId = bookId, address = address, zipcode = zipcode, mail = mail, city = city, phone = phone, status = status)
    val newShippingTuple: (String, String, String, String, String, String, String, String) = Shipping.unapply(newShipping).get

    val dbio: DBIO[Int] = shippings += newShippingTuple
    var resultFuture: Future[Int] = db.run(dbio)
    resultFuture.map(_ => ())
  }

  def getShippingsByBookId(shippingId: String): Future[Seq[Shipping]] = {
    val query = shippings.filter(_.shippingId === shippingId)

    val shippingListFuture = db.run(query.result)

    shippingListFuture.map((shippingList: Seq[(String, String, String, String, String, String, String, String)]) => {
      shippingList.map(Shipping tupled _)

    })
  }

    def getAllShippings(): Future[Seq[Shipping]] = {

    val shippingListFuture = db.run(shippings.result)

    shippingListFuture.map((shippingList: Seq[(String, String, String, String, String, String, String, String)]) => {
      shippingList.map(Shipping tupled _)

    })
  }
  
  
    def validateShipment(bookId: String): Future[Unit] ={
      val newStatus = "En cours de livraison"
      val updateShipmentStatusRequest: DBIO[Int] = sqlu"update shipping set status = ${newStatus} where book_id = ${bookId};"

      val updateShipmentStatusFuture: Future[Int] = db.run(updateShipmentStatusRequest)

      //Await.result(updateShipmentStatusFuture, Duration.Inf)
      updateShipmentStatusFuture.map(_ => ())


  }

}


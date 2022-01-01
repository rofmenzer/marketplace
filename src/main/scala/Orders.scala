package poca

import scala.concurrent.Future
import slick.jdbc.PostgresProfile.api._
import java.util.UUID
import java.sql.Timestamp

case class Order(orderId: String, bookId: String, userId: String, date: String, quantity: Int, status: String, amount: Double)

final case class OrdersAlreadyExistsException(private val message: String="", private val cause: Throwable=None.orNull)
    extends Exception(message, cause) 
final case class InconsistenOrderstStateException(private val message: String="", private val cause: Throwable=None.orNull)
    extends Exception(message, cause) 
final case class BookNotExistException(private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class InconsistenOrdertStateException(private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class QuantityNotSufficientException(private val message: String = "", private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


class Orders {
    class OrdersTable(tag: Tag) extends Table[(String, String, String, String, Int, String, Double)](tag, "orders") {
        def orderId = column[String]("order_id", O.PrimaryKey)
        def bookId = column[String]("book_id")
        def userId = column[String]("user_id")
        def date = column[String]("date")
        def quantity = column[Int]("quantity")
        def status = column[String]("status")
        def amount = column[Double]("amount")
        def * = (orderId, bookId, userId, date, quantity, status, amount)
    }

    implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
    val db = MyDatabase.db
    val orders = TableQuery[OrdersTable]
    var books = new Books()



    def createOrder(bookId: String, userId: String, date: String, quantity: Int): Future[Unit] = {
            val id = UUID.randomUUID.toString()
            var amount = 0.0
            val status = "idle"

            books.getBookById(bookId).map(foundBooks => {

              if(!foundBooks.isEmpty){

                foundBooks.map(book => {
                  if(book.quantity > 0){
                      // check if an order already exist --> increment quantity only else create new order                  
                      getOrdersByBookIdAndStatus(bookId, "idle").map(items => {
                        if(items.isEmpty){
                            amount = book.price * quantity.toDouble
                  
                            val newOrder = Order(orderId = id, bookId = bookId, userId = userId, date = date, quantity = quantity, status = status, amount = amount)
                            val newOrderAsTuple: (String, String, String, String, Int, String, Double) = Order.unapply(newOrder).get

                            val dbio: DBIO[Int] = orders += newOrderAsTuple
                            var resultFuture: Future[Int] = db.run(dbio)
                            resultFuture.map(_ => ())
                        // there is an order so we incriment quantity
                        }else{
                            var newQuantity = items(0).quantity + 1
                            var newAmount = (items(0).amount / items(0).quantity) * newQuantity                       
                            val query = sqlu"UPDATE ORDERS SET quantity = ${newQuantity}, amount = ${newAmount} WHERE order_id = ${items(0).orderId}"
                            var resultFuture: Future[Int] = db.run(query)                                              
                            resultFuture.map(_ => ())
                        }
                      })                      
                      // update book quantity
                      //val newQuantity = book.quantity - 1
                      //books.updateBook(book.id, new Book(book.id, book.title, book.author, book.synopsis, book.year, book.isbn, book.price, newQuantity))
                    }else{
                      throw new QuantityNotSufficientException(s"The book with id '$bookId' is not available.")
                      
                    }
                  })

                } else{
                    throw new BookNotExistException(s"A book with id '$bookId' does not exist.")
                }
            })


    }
    
    
    
  def getAllOrders(): Future[Seq[Order]] = {
    val orderListFuture = db.run(orders.result)

    orderListFuture.map((orderList: Seq[(String, String, String, String, Int, String, Double)]) => {
      orderList.map(Order tupled _)
    })
  }
  
  
  def getOrdersByUserId(userId: String): Future[Seq[Order]] = {
    val status1 = "idle"
    val status2 = "valide"
    val query = orders.filter(x => (x.userId === userId && (x.status === status1 || x.status === status2) ))

    val orderListFuture = db.run(query.result)

    orderListFuture.map((orderList: Seq[(String, String, String, String, Int, String, Double)]) => {
      orderList.map(Order tupled _)
        
    })
  }
  
  def getOrdersByBookId(bookId: String): Future[Seq[Order]] = {
    val query = orders.filter(_.bookId === bookId)

    val orderListFuture = db.run(query.result)

    orderListFuture.map((orderList: Seq[(String, String, String, String, Int, String, Double)]) => {
      orderList.map(Order tupled _)
        
    })
  }

    def getOrdersByBookIdAndStatus(bookId: String, status: String): Future[Seq[Order]] = {
    val query = orders.filter(x => (x.bookId === bookId && x.status === status))
    
    //val query1 = sqlu"SELECT * FROM ORDERS WHERE book_id = ${bookId} AND status = ${status};" 

    val orderListFuture = db.run(query.result)

    orderListFuture.map((orderList: Seq[(String, String, String, String, Int, String, Double)]) => {
      orderList.map(Order tupled _)
        
    })
  }


  
  def deleteOrder(orderId: String): Future[Unit] ={
        val orderDeleteFuture = orders.filter(_.orderId === orderId)
        val action = orderDeleteFuture.delete
        val affectedRowsCount: Future[Int] = db.run(action)
        val sql = action.statements.head
        
        affectedRowsCount.map(_ => ())
    }  


  def getOrdersById(orderId: String): Future[Option[Order]] = {
    val query = orders.filter(_.orderId === orderId)

    val orderListFuture = db.run(query.result)

    orderListFuture.map((orderList: Seq[(String, String, String, String, Int, String, Double)]) => {
//      orderList.map(Order tupled _)
      orderList.length match {
        case 0 => None
        case 1 => Some(Order tupled orderList.head)
        case _ => throw new InconsistenBooktStateException(s"id $orderId is linked to several books in database!")
      }
    })
  }

  def validateOrder(orderId: String): Future[Unit] ={
      val newStatus = "valide"
      val updateOrderStatusRequest: DBIO[Int] = sqlu"update orders set status = ${newStatus} where order_id = ${orderId};"

      val updateOrderStatusFuture: Future[Int] = db.run(updateOrderStatusRequest)

      //Await.result(updateOrderStatusFuture, Duration.Inf)
      updateOrderStatusFuture.map(_ => {
        getOrdersById(orderId).map(foundOrders => {
            if(!foundOrders.isEmpty){
                foundOrders.map(order => {
                    val updateQuantityRequest: DBIO[Int] = sqlu"update books set quantity = quantity - 1 where id = ${order.bookId};"

                    val updateQuantityFuture: Future[Int] = db.run(updateQuantityRequest)

                    updateQuantityFuture.map(_ => ())
                })
              }

        })
      })


  }
  
  
    def cancelOrder(orderId: String): Future[Unit] ={
      val oldStatus = "idle"
      val newStatus = "canceled"
      val updateOrderStatusRequest: DBIO[Int] = sqlu"update orders set status = ${newStatus} where order_id = ${orderId} and status = ${oldStatus};"

      val updateOrderStatusFuture: Future[Int] = db.run(updateOrderStatusRequest)

      //Await.result(updateOrderStatusFuture, Duration.Inf)
      updateOrderStatusFuture.map(_ => ())

  }
  

}


package poca

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.LazyLogging
import poca.TwirlMarshaller._
import scala.concurrent.Future


class Routes(users: Users, books: Books, orders: Orders, shippings: Shippings, payments: Payments) extends LazyLogging {
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global
  val routes: Route =
    concat(
      path("") {
        get {
          complete(getHomePage)
        }
      },
      path("hello") {
        get {
          complete(getHello)
        }
      },
      path("signup") {
        get {
          complete(getSignup)
        }
      },
      path("login") {
        get {
          complete(getLogin)
        }
      },
      path("register") {
        (post & formFieldMap) { fields =>
          complete(register(fields))
        }
      },
      path("users") {
        get {
          complete(getUsers)
        }
      },
      path("user" / "books") {
        get {
          complete(getBooksUser)
        }
      },
      path("admin" / "books") {
        get {
          complete(getBooks)
        }
      },

      path("admin" / "orders") {
        get {
          complete(getOrders)
        }
      },


      path("admin" / "orders" / Segment) { orderId => {
        delete {
          complete(deleteOrder(orderId))
        }
      }
      },
      path("admin" / "orders" / "validate" / Segment) { orderId => {
        get {
          complete(validateOrder(orderId))
        }
      }
      },

      path("user" / "orders" / Segment) { userId => {
        get {
          complete(getOrdersByUserId(userId))
        }
      }
      },

      path("user" / "orders" / "cancel" / Segment) { orderId => {
        get {
          complete(cancelOrder(orderId))
        }
      }
      },

      path("admin" / "shipment" / "validate" / Segment) { bookId => {
        get {
          complete(validateShipment(bookId))
        }
      }
      },
      path("user" / "orders" / "payment" / Segment) { id =>
        get {
          complete(getBookPaymentForm(id))
        }
      },


      path("user" / "books" / "details" / Segment) { id => {
        get {
          complete(getBookDetails(id))
        }
      }
      },
      path("user" / "books" / "order") {
        (post & formFieldMap) { fields =>
          complete(orderBook(fields))
        }

      },
      path("user" / "orders" / "invoice" / Segment) { id =>
        get {
          complete(getInvoice(id))
        }
      },
      path("user" / "books" / "order" / "shipping") {
        (post & formFieldMap) { fields =>
          complete(addShipping(fields))
        }

      },
      path("user" / "books" / "order" / "payment") {
        (post & formFieldMap) { fields =>
          complete(addPayment(fields))
        }

      },

      path("admin" / "books" / "add") {
        get {
          complete(getbooksform)
        }
      },
      path("admin" / "books" / "update" / Segment) { id =>
        get {
          complete(getBooksUpdateForm(id))
        }
      },
      path("user" / "orders" / "shipping" / Segment) { id =>
        get {
          complete(getBookShippingForm(id))
        }
      },
      path("admin" / "books" / Segment) { id => {
        delete {
          complete(deleteBook(id))
        }
      }
      },


      path("books" / "add") {
        (post & formFieldMap) { fields =>
          complete(addBook(fields))
        }
      },
      path("books" / "update") {
        {
          (post & formFieldMap) { fields =>
            complete(updateBook(fields))
          }
        }

      }
      ,
    )

  def getHomePage() = {
    logger.info("I got a request for home page.")
    html.home()
  }

  def getHello() = {
    logger.info("I got a request to greet.")
    HttpEntity(
      ContentTypes.`text/html(UTF-8)`,
      "<h1>Say hello to akka-http</h1>"
    )
  }

  def getSignup() = {
    logger.info("I got a request for signup.")
    html.signup()
  }

  def getLogin() = {
    logger.info("I got a request for login.")
    html.login()
  }

  def register(fields: Map[String, String]): Future[HttpResponse] = {
    logger.info("I got a request to register.")

    fields.get("username") match {
      case Some(username) => {
        val userCreation: Future[Unit] = users.createUser(username = username)

        userCreation.map(_ => {
          HttpResponse(
            StatusCodes.OK,
            entity = s"Welcome '$username'! You've just been registered to our great marketplace.",
          )
        }).recover({
          case exc: UserAlreadyExistsException => {
            HttpResponse(
              StatusCodes.OK,
              entity = s"The username '$username' is already taken. Please choose another username.",
            )
          }
        })
      }
      case None => {
        Future(
          HttpResponse(
            StatusCodes.BadRequest,
            entity = "Field 'username' not found."
          )
        )
      }
    }
  }

  def getUsers() = {
    logger.info("I got a request to get user list.")

    val userSeqFuture: Future[Seq[User]] = users.getAllUsers()

    userSeqFuture.map(userSeq => html.users(userSeq))
  }

  def getbooksform() = {
    logger.info("I got a request for adding a book form.")
    html.addBooks();
  }

  def getBookShippingForm(orderId: String) = {
    logger.info("I got a request for shipping a book form.")
    val orderFuture: Future[Option[Order]] = orders.getOrdersById(orderId)
    orderFuture.map(option => option.map(order => {
      val shippingSeqFuture: Future[Seq[Shipping]] = shippings.getAllShippings()

      shippingSeqFuture.map(shippingSeq => html.shipping(order, shippingSeq))

    }))

  }

  def getBookPaymentForm(orderId: String) = {
    logger.info("I got a request for  payment a book form.")
    val orderFuture: Future[Option[Order]] = orders.getOrdersById(orderId)
    orderFuture.map(option => option.map(order => html.paymentBook(order)))
  }
  def getInvoice(bookId: String) = {
    logger.info("I got a request for  invoice a book .")
    val bookFuture: Future[Option[Book]] = books.getBookById(bookId)
    bookFuture.map(option => option.map(book => html.invoice(book)))
  }
  def getBooksUpdateForm(bookId: String) = {
    logger.info("I got a request for updating a book form.")
    val bookFuture: Future[Option[Book]] = books.getBookById(bookId)
    bookFuture.map(option => option.map(book => html.updateBooks(book)))
  }

  def getBooks() = {
    logger.info("I got a request to get book list.")

    val bookSeqFuture: Future[Seq[Book]] = books.getAllBooks()

    bookSeqFuture.map(bookSeq => html.books(bookSeq))
  }

  def getOrders() = {
    logger.info("I got a request to get order list.")

    val orderSeqFuture: Future[Seq[Order]] = orders.getAllOrders()

    val shippingSeqFuture: Future[Seq[Shipping]] = shippings.getAllShippings()

    shippingSeqFuture.map(shippingSeq => {
      orderSeqFuture.map(orderSeq => html.orders(orderSeq, shippingSeq))
    })
  }


  def getOrdersByUserId(userId: String) = {
    logger.info("I got a request to get user orders with userId " + userId)

    val orderFuture: Future[Seq[Order]] = orders.getOrdersByUserId(userId)

    orderFuture.map(orderSeq => html.userOrders(orderSeq))
  }


  def getBooksUser() = {
    logger.info("I got a request to get book list.")

    val bookSeqFuture: Future[Seq[Book]] = books.getAllBooks()

    bookSeqFuture.map(bookSeq => html.booksUser(bookSeq))
  }

  def getBookDetails(bookId: String) = {
    logger.info("I got a request to get book details with id " + bookId)

    val bookFuture: Future[Option[Book]] = books.getBookById(bookId)

    bookFuture.map(option => option.map(book => html.bookDetails(book)))
  }

  def addBook(fields: Map[String, String]): Future[HttpResponse] = {
    logger.info("I got a request to register a book.")
    var auteur, synopsi, year, isbn: String = ""
    var price: Double = 0
    var quantity: Int = 0
    fields.get("author") match {

      case Some(author) => {
        auteur = author
      }
      case None => {
        logger.info("Book without author")
      }

    }
    fields.get("synopsis") match {

      case Some(synopsis) => {
        synopsi = synopsis
      }
      case None => {
        logger.info("Book without synopsis")
      }

    }
    fields.get("year") match {

      case Some(yr) => {
        year = yr
      }
      case None => {
        logger.info("Book without year")
      }

    }
    fields.get("isbn") match {

      case Some(isb) => {
        isbn = isb
      }
      case None => {
        logger.info("Book without isbn")
      }

    }
    fields.get("price") match {

      case Some(pricex) => {
        price = pricex.toDouble
      }
      case None => {
        logger.info("Book without price")
      }

    }
    fields.get("quantity") match {

      case Some(quantityx) => {
        quantity = quantityx.toInt
      }
      case None => {
        logger.info("Book without quantity")
      }

    }

    fields.get("title") match {
      case Some(title) => {
        val bookCreation: Future[Unit] = books.createBook(title = title, author = auteur, synopsis = synopsi, year = year, isbn = isbn, price = price, quantity = quantity)

        bookCreation.map(_ => {
          HttpResponse(
            StatusCodes.OK,
            entity = s"The book with the title '$title' have been created and added to our marketplace.",
          )
        }).recover({
          case exc: BookAlreadyExistsException => {
            HttpResponse(
              StatusCodes.OK,
              entity = s"The title '$title' is already taken. Please choose another title.",
            )
          }
        })
      }
      case None => {
        Future(
          HttpResponse(
            StatusCodes.BadRequest,
            entity = "Field 'title' not found."
          )
        )
      }
    }
  }

  def updateBook(fields: Map[String, String]): Future[HttpResponse] = {
    logger.info("I got a request to update a book.")
    var id, title, auteur, synopsi, year, isbn: String = ""
    var price: Double = 0
    var quantity: Int = 0

    fields.get("id") match {

      case Some(idx) => {
        id = idx
      }
      case None => {
        logger.info("Book without id")
      }

    }

    fields.get("title") match {

      case Some(titlex) => {
        title = titlex
      }
      case None => {
        logger.info("Book without title")
      }

    }

    fields.get("author") match {

      case Some(author) => {
        auteur = author
      }
      case None => {
        logger.info("Book without author")
      }

    }
    fields.get("synopsis") match {

      case Some(synopsis) => {
        synopsi = synopsis
      }
      case None => {
        logger.info("Book without synopsis")
      }

    }
    fields.get("year") match {

      case Some(yr) => {
        year = yr
      }
      case None => {
        logger.info("Book without year")
      }

    }
    fields.get("isbn") match {

      case Some(isb) => {
        isbn = isb
      }
      case None => {
        logger.info("Book without isbn")
      }

    }
    fields.get("price") match {

      case Some(pricex) => {
        price = pricex.toDouble
      }
      case None => {
        logger.info("Book without price")
      }

    }
    fields.get("quantity") match {

      case Some(quantityx) => {
        quantity = quantityx.toInt
      }
      case None => {
        logger.info("Book without quantity")
      }

    }


    val bookUpdateFuture: Future[Int] = books.updateBook(id, Book(id = id, title = title, author = auteur, synopsis = synopsi, year = year, isbn = isbn, price = price, quantity = quantity))

    bookUpdateFuture.map(_ => {
      HttpResponse(
        StatusCodes.OK,
        entity = s"The book with the id '$id' have been updated ",
      )
    })


  }

  def deleteBook(idFields: String): Future[HttpResponse] = {
    logger.info("I got a request to delete book from list.")

    val bookDeleteFuture: Future[Unit] = books.delete(idFields)

    bookDeleteFuture.map(_ => {
      HttpResponse(
        StatusCodes.OK,
        entity = s"The book with the id '$idFields' have been deleted from our marketplace.",
      )
    })
  }

  def orderBook(fields: Map[String, String]): Future[HttpResponse] = {
    logger.info("I got a request to order a book.")
    var userId: String = "0"
    var date: String = ""
    var quantity: Int = 0
    var amount: Double = 0

    fields.get("quantity") match {

      case Some(quantityx) => {
        quantity = quantityx.toInt
      }
      case None => {
        logger.info("Order without quantity")
      }

    }

    fields.get("bookId") match {
      case Some(bookId) => {
        val orderCreation: Future[Unit] = orders.createOrder(bookId = bookId, userId = userId, date = date, quantity = quantity)

        orderCreation.map(_ => {
          HttpResponse(
            StatusCodes.OK,
            entity = s"The order with the bookId '$bookId' have been created and added to our marketplace.",
          )
        }).recover({
          case exc: BookNotExistException => {
            HttpResponse(
              StatusCodes.OK,
              entity = s"A book with id '$bookId' does not exist.",
            )
          }
          case exc: QuantityNotSufficientException => {
            HttpResponse(
              StatusCodes.OK,
              entity = s"The book with id '$bookId' is not available.",
            )

          }
        })
      }
      case None => {
        Future(
          HttpResponse(
            StatusCodes.BadRequest,
            entity = "Field 'bookId' not found."
          )
        )
      }
    }
  }


  def deleteOrder(orderId: String): Future[HttpResponse] = {
    logger.info("I got a request to delete order from list.")

    val orderDeleteFuture: Future[Unit] = orders.deleteOrder(orderId)

    orderDeleteFuture.map(_ => {
      HttpResponse(
        StatusCodes.OK,
        entity = s"The order with the id '$orderId' have been deleted.",

      )
    })
  }

  def validateOrder(orderId: String): Future[HttpResponse] = {
    logger.info("I got a request to validate the order " + orderId)

    val orderValidationFuture: Future[Unit] = orders.validateOrder(orderId)

    orderValidationFuture.map(_ => {
      HttpResponse(
        StatusCodes.OK,
        entity = s"The order with the id '$orderId' have been validated.",

      )
    })

  }

  def addShipping(fields: Map[String, String]): Future[HttpResponse] = {
    logger.info("I got a request to ship a book.")
    //    logger.info(fields)
    var address, orderId, email, city, zip, phone: String = ""
    fields.get("orderId") match {

      case Some(bid) => {
        orderId = bid
      }
      case None => {
        logger.info("Shpment without bookid")
      }

    }
    fields.get("address") match {

      case Some(adress) => {
        address = adress
      }
      case None => {
        logger.info("Order without address")
      }

    }
    fields.get("email") match {

      case Some(mail) => {
        email = mail
      }
      case None => {
        logger.info("Order without mail")
      }

    }
    fields.get("city") match {

      case Some(cit) => {
        city = cit
      }
      case None => {
        logger.info("Order without city")
      }

    }
    fields.get("zip") match {

      case Some(zi) => {
        zip = zi
      }
      case None => {
        logger.info("Order without zip code")
      }

    }
    fields.get("phonenumber") match {
      case Some(phone) => {
        val shippingCreation: Future[Unit] = shippings.createShipping(bookId = orderId, address = address, zipcode = zip, mail = email, city = city, phone = phone, status = "idle")

        shippingCreation.map(_ => {
          HttpResponse(
            StatusCodes.OK,
            entity = s"The Shippings have been created and added to our marketplace.",
          )
        }).recover({
          case exc: BookAlreadyExistsException => {
            HttpResponse(
              StatusCodes.OK,
              entity = s"error with shp create method",
            )
          }
        })
      }
      case None => {
        Future(
          HttpResponse(
            StatusCodes.BadRequest,
            entity = "Field 'title' not found."
          )
        )
      }
    }
  }

  def cancelOrder(orderId: String): Future[HttpResponse] = {
    logger.info("I got a request to cancel the order " + orderId)

    val orderCancelFuture: Future[Unit] = orders.cancelOrder(orderId)

    orderCancelFuture.map(_ => {
      HttpResponse(
        StatusCodes.OK,
        entity = s"The order with the id '$orderId' have been canceled.",

      )
    })

  }


  def validateShipment(bookId: String): Future[HttpResponse] = {
    logger.info("I got a request to validate the shipment for one order ")

    val shipmentValidationFuture: Future[Unit] = shippings.validateShipment(bookId)

    shipmentValidationFuture.map(_ => {
      HttpResponse(
        StatusCodes.OK,
        entity = s"The shipment have been validated.",

      )
    })

  }


    def addPayment(fields: Map[String, String]): Future[HttpResponse] = {
    logger.info("I got a request to make a payment.")
    //    logger.info(fields)
    var orderId, creditCardNumber, securityCode, cardExpiration : String = ""
    
    fields.get("creditCardNumber") match {

      case Some(creditCardNbr) => {
        creditCardNumber = creditCardNbr
      }
      case None => {
        logger.info("Order without creditCardNumber")
      }

    }
    fields.get("securityCode") match {

      case Some(secCode) => {
        securityCode = secCode
      }
      case None => {
        logger.info("Order without securityCode")
      }

    }
    fields.get("cardExpiration") match {

      case Some(cardExp) => {
        cardExpiration = cardExp
      }
      case None => {
        logger.info("Order without cardExpiration")
      }

    }
    fields.get("orderId") match {
      case Some(orderid) => {
        val paymentCreation: Future[Unit] = payments.createPayment(orderId = orderid, creditCardNumber = creditCardNumber, securityCode = securityCode, cardExpiration = cardExpiration)

        paymentCreation.map(_ => {
          HttpResponse(
            StatusCodes.OK,
            entity = s"The Payments have been created",
          )
        })
      } 
      case None => {
        Future(
          HttpResponse(
            StatusCodes.BadRequest,
            entity = "Field 'orderId' not found."
          )
        )
      }
    }
  }

}

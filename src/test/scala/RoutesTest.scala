import scala.concurrent.Future
import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.http.scaladsl.model.{HttpRequest, StatusCodes, ContentTypes, FormData, HttpMethods}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalamock.scalatest.MockFactory
import poca.{MyDatabase, Users, User, Books, Book, Order, Orders, Shipping, Shippings, Payment, Payments, UserAlreadyExistsException, BookNotExistException, QuantityNotSufficientException, Routes}


class RoutesTest extends AnyFunSuite with Matchers with MockFactory with ScalatestRouteTest {

    // the Akka HTTP route testkit does not yet support a typed actor system (https://github.com/akka/akka-http/issues/2036)
    // so we have to adapt for now
    lazy val testKit = ActorTestKit()
    implicit def typedSystem = testKit.system
    override def createActorSystem(): akka.actor.ActorSystem =
        testKit.system.classicSystem

    test("Route GET /hello should say hello") {
        var mockUsers = mock[Users]
        val routesUnderTest = new Routes(mockUsers, null, null, null, null).routes

        val request = HttpRequest(uri = "/hello")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

            entityAs[String] should ===("<h1>Say hello to akka-http</h1>")
        }
    }

    test("Route GET /signup should returns the signup page") {
        var mockUsers = mock[Users]
        val routesUnderTest = new Routes(mockUsers, null, null, null, null).routes

        val request = HttpRequest(uri = "/signup")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }
    test("Route GET /login should returns the login page") {
        var mockUsers = mock[Users]
        val routesUnderTest = new Routes(mockUsers, null, null,null, null).routes

        val request = HttpRequest(uri = "/login")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }

    test("Route POST /register should create a new user") {
        var mockUsers = mock[Users]
        (mockUsers.createUser _).expects("toto").returning(Future(())).once()

        val routesUnderTest = new Routes(mockUsers, null, null,null, null).routes

        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/register",
            entity = FormData(("username", "toto")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)

            entityAs[String] should ===("Welcome 'toto'! You've just been registered to our great marketplace.")
        }
    }

    test("Route POST /register should warn the user when username is already taken") {
        var mockUsers = mock[Users]
        (mockUsers.createUser _).expects("toto").returns(Future({
            throw new UserAlreadyExistsException("")
        })).once()

        val routesUnderTest = new Routes(mockUsers, null, null,null, null).routes

        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/register",
            entity = FormData(("username", "toto")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)

            entityAs[String] should ===("The username 'toto' is already taken. Please choose another username.")
        }
    }

    test("Route GET /users should display the list of users") {
        var mockUsers = mock[Users]
        val userList = List(
            User(username="riri", userId="id1"),
            User(username="fifi", userId="id2"),
            User(username="lulu", userId="id2")
        )
        (mockUsers.getAllUsers _).expects().returns(Future(userList)).once()

        val routesUnderTest = new Routes(mockUsers, null, null,null, null).routes

        val request = HttpRequest(uri = "/users")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }

    // Book routes tests
    test("Route GET /amdin/books should display the list of books") {
        var mockBooks = mock[Books]
        val bookList = List(
            Book(id="id1", title="title1", author="auth1", synopsis="syn1", year="year1", isbn="isbn1", price=5.0, quantity = 10),
            Book(id="id2", title="title2", author="auth2", synopsis="syn2", year="year2", isbn="isbn2", price=5.0, quantity = 11),
            Book(id="id3", title="title3", author="auth3", synopsis="syn3", year="year3", isbn="isbn3", price=5.0, quantity = 12)
        )
        (mockBooks.getAllBooks _).expects().returns(Future(bookList)).once()

        val routesUnderTest = new Routes(null, mockBooks, null,null, null).routes

        val request = HttpRequest(uri = "/admin/books")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }
    test("Route GET /admin/books/add should return add book page") {
        var mockBooks = mock[Books]
        val routesUnderTest = new Routes(null,mockBooks, null,null, null).routes
        val request = HttpRequest(uri = "/admin/books/add")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }
    test("Route POST /admin/books/add should create a new book") {
        var mockBooks = mock[Books]

        var book = Book(id="id1", title="title1", author="auth1", synopsis="syn1", year="year1", isbn="isbn1", price=5.0, quantity = 10)
        (mockBooks.createBook _).expects("title1","auth1","syn1","year1","isbn1", 5.0,10).returning(Future(())).once()

        val routesUnderTest = new Routes(null,mockBooks, null,null, null).routes
        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/books/add",
            entity = FormData(("title","title1"),("author","auth1"),("synopsis","syn1"),("year","year1"), ("isbn","isbn1"), ("price","5.0"),("quantity","10")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)

        }
    }

    // User Book routes tests
    test("Route GET /user/books should display the list of books") {
        var mockBooks = mock[Books]
        val bookList = List(
            Book(id="id1", title="title1", author="auth1", synopsis="syn1", year="year1", isbn="isbn1", price=5.0, quantity = 10),
            Book(id="id2", title="title2", author="auth2", synopsis="syn2", year="year2", isbn="isbn2", price=5.0, quantity = 11),
            Book(id="id3", title="title3", author="auth3", synopsis="syn3", year="year3", isbn="isbn3", price=5.0, quantity = 12)
        )
        (mockBooks.getAllBooks _).expects().returns(Future(bookList)).once()

        val routesUnderTest = new Routes(null, mockBooks, null,null, null).routes

        val request = HttpRequest(uri = "/user/books")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }

    test("Route GET user/books/details/id1 should display the list of books") {
        var mockBooks = mock[Books]
        val bookList = List(
            Book(id="id1", title="title1", author="auth1", synopsis="syn1", year="year1", isbn="isbn1",price=5.0, quantity = 10),
            Book(id="id2", title="title2", author="auth2", synopsis="syn2", year="year2", isbn="isbn2",price=5.0, quantity = 11),
            Book(id="id3", title="title3", author="auth3", synopsis="syn3", year="year3", isbn="isbn3",price=5.0, quantity = 12)
        )

        (mockBooks.getBookById _).expects("id1").returns(Future(Option(Book(id="id1", title="B1", author="A1", synopsis="S1", year="Y1", isbn="IS1", price=5.0, quantity = 10)))).once()

        val routesUnderTest = new Routes(null, mockBooks, null,null, null).routes

        val request = HttpRequest(uri = "/user/books/details/id1")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)
        }
    }


    // order routes
    test("Route POST /user/books/order should create a new order") {
        var mockBooks = mock[Books]
        var mockOrders = mock[Orders]

        var book = Book(id="id1", title="title1", author="auth1", synopsis="syn1", year="year1", isbn="isbn1", price=5.0, quantity = 10)

        var order = Order(orderId="id1", bookId="id1", userId="0", date="", quantity = 2, status="idle", amount = 10.0)
        (mockOrders.createOrder _).expects("id1", "0", "", 2).returning(Future(())).once()

        val routesUnderTest = new Routes(null,mockBooks, mockOrders,null, null).routes
        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/user/books/order",
            entity = FormData(("bookId","id1"),("quantity","2")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)

        }
    }

    test("Route POST /user/books/order should not create a new order with book id not exists") {
        var mockBooks = mock[Books]
        var mockOrders = mock[Orders]

        var book = Book(id="id1", title="title1", author="auth1", synopsis="syn1", year="year1", isbn="isbn1", price=5.0, quantity = 10)

        var order = Order(orderId="id6", bookId="id6", userId="0", date="", quantity = 2, status="idle", amount = 10.0)
        (mockOrders.createOrder _).expects("id6", "0", "", 2).returning(Future({
            throw new BookNotExistException("")
        })).once()

        val routesUnderTest = new Routes(null,mockBooks, mockOrders,null, null).routes
        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/user/books/order",
            entity = FormData(("bookId","id6"),("quantity","2")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)
            entityAs[String] should ===("A book with id 'id6' does not exist.")

        }
    }

    // order routes
    test("Route POST /user/books/order should not create a new order with book qunaity = 0") {
        var mockBooks = mock[Books]
        var mockOrders = mock[Orders]

        var book = Book(id="id1", title="title1", author="auth1", synopsis="syn1", year="year1", isbn="isbn1", price=5.0, quantity = 0)

        var order = Order(orderId="id1", bookId="id1", userId="0", date="", quantity = 1, status="idle", amount = 10.0)
        (mockOrders.createOrder _).expects("id1", "0", "", 1).returning(Future({
            throw new QuantityNotSufficientException("")
        })).once()

        val routesUnderTest = new Routes(null,mockBooks, mockOrders,null, null).routes
        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/user/books/order",
            entity = FormData(("bookId","id1"),("quantity","1")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)
            entityAs[String] should ===("The book with id 'id1' is not available.")

        }
    }

    test("Route GET / should return home page") {
        var mockUsers = mock[Users]
        val routesUnderTest = new Routes(mockUsers, null,null,null, null).routes

        val request = HttpRequest(uri = "/")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }


    // Order routes tests
    test("Route GET /amdin/orders should display the list of orders") {
        var mockOrders = mock[Orders]
        var mockShipment = mock[Shippings]
        
        val orderList = List(
            Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="In progress", amount=50.0),
            Order(orderId="id2", bookId="book2", userId="user2", date="date2", quantity=2, status="In progress", amount=100.0),
            Order(orderId="id3", bookId="book3", userId="user3", date="date3", quantity=1, status="In progress", amount=50.0),
        )
        (mockOrders.getAllOrders _).expects().returns(Future(orderList)).once()
        
        val shippingList = List(Shipping(shippingId="id1", bookId="book1", address="addr1", zipcode="zip1", mail="mail1", city="city1", phone="phone1",status="status1"))
        
        (mockShipment.getAllShippings _).expects().returns(Future(shippingList)).once()

        val routesUnderTest = new Routes(null,null,mockOrders,mockShipment, null).routes

        val request = HttpRequest(uri = "/admin/orders")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)

        }
    }
    
    
    test("Route GET /user/orders/user1 should display the list of orders of user1") {
        var mockOrdersByUserId = mock[Orders]
        val orderList = List(
            Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="idle", amount=50.0),
            Order(orderId="id2", bookId="book2", userId="user1", date="date2", quantity=2, status="idle", amount=100.0),
            Order(orderId="id3", bookId="book3", userId="user3", date="date3", quantity=1, status="idle", amount=50.0)
        )

        (mockOrdersByUserId.getOrdersByUserId _).expects("user1").returns(Future(Seq(Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="In progress", amount=50.0),
            Order(orderId="id2", bookId="book2", userId="user1", date="date2", quantity=2, status="In progress", amount=100.0)
            ))).once()

        val routesUnderTest = new Routes(null, null, mockOrdersByUserId,null, null).routes

        val request = HttpRequest(uri = "/user/orders/user1")
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/html(UTF-8)`)
        }
    }
    
    
      test("Route DELETE /amdin/orders/orderId should delete order with id orderId") {
        var mockDeleteOrder = mock[Orders]
        val orderList = List(
            Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="In progress", amount=50.0),
            Order(orderId="id2", bookId="book2", userId="user2", date="date2", quantity=2, status="In progress", amount=100.0),
            Order(orderId="id3", bookId="book3", userId="user3", date="date3", quantity=1, status="In progress", amount=50.0),
        )
        (mockDeleteOrder.deleteOrder _).expects("id1").returns(Future(Seq(Order(orderId="id2", bookId="book2", userId="user2", date="date2", quantity=2, status="In progress", amount=100.0),
            Order(orderId="id3", bookId="book3", userId="user3", date="date3", quantity=1, status="In progress", amount=50.0)
            ))).once()

        val routesUnderTest = new Routes(null,null,mockDeleteOrder,null, null).routes

        
        val request = HttpRequest(
            method = HttpMethods.DELETE,
            uri = "/admin/orders/id1",
            entity = FormData(("orderId","id1")).toEntity
        )
        
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)
            
            entityAs[String] should ===("The order with the id 'id1' have been deleted.")

        }
    }
    

    test("Route DELETE /amdin/orders//validate/orderId should validate order with id orderId") {
        var mockOrders = mock[Orders]
        Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="idle", amount=50.0)
        
        (mockOrders.validateOrder _).expects("id1").returns(
             Future(Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="valide", amount=50.0))           
            ).once()

        val routesUnderTest = new Routes(null,null,mockOrders,null, null).routes

        
        val request = HttpRequest(
            method = HttpMethods.GET,
            uri = "/admin/orders/validate/id1",
        )
        
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)
            
            entityAs[String] should ===("The order with the id 'id1' have been validated.")

        }
    }
    
    
    test("Route GET /user/orders/cancel/orderId should cancel order with id orderId") {
        var mockOrders = mock[Orders]
        Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="idle", amount=50.0)
        
        (mockOrders.cancelOrder _).expects("id1").returns(
             Future(Order(orderId="id1", bookId="book1", userId="user1", date="date1", quantity=1, status="canceled", amount=50.0))           
            ).once()

        val routesUnderTest = new Routes(null,null,mockOrders, null, null).routes

        
        val request = HttpRequest(
            method = HttpMethods.GET,
            uri = "/user/orders/cancel/id1",
        )
        
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)
            
            entityAs[String] should ===("The order with the id 'id1' have been canceled.")

        }
    }
    
    
    
    
       test("Route GET /amdin/shipment/validate/bookId should validate shipment with id bookId") {
        var mockShipment = mock[Shippings] 
        Shipping(shippingId="id1", bookId="book1", address="addr1", zipcode="zip1", mail="mail1", city="city1", phone="phone1",status="idle")
        
        (mockShipment.validateShipment _).expects("book1").returns(
             Future(Shipping(shippingId="id1", bookId="book1", address="addr1", zipcode="zip1", mail="mail1", city="city1", phone="phone1",status="En cours de livraison"))          
            ).once()

        val routesUnderTest = new Routes(null,null,null, mockShipment, null).routes

        
        val request = HttpRequest(
            method = HttpMethods.GET,
            uri = "/admin/shipment/validate/book1",
        )
        
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)
            
            entityAs[String] should ===("The shipment have been validated.")

        }
    }


    test("Route POST /user/books/order/shipping should create a new shippement") {
        val mockShipp = mock[Shippings]

        var shippings = Shipping(shippingId="id1", bookId="o1", address="adresse1", zipcode="zipcode1", mail="mail1", city="city1", phone="phonenumber",status="status1")
        (mockShipp.createShipping _).expects("o1","address","zip","email","city", "phonenumber","idle").returning(Future(())).once()

        val routesUnderTest = new Routes(null,null, null,mockShipp,null).routes
        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/user/books/order/shipping",
            entity = FormData(("orderId","o1"),("address","address"),("zip","zip"),("email","email"), ("city","city"), ("phonenumber","phonenumber")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)

        }
    }

    test("Route POST /user/orders/payment/SEGMENT should create a new payment") {
        val mockPay = mock[Payments]

        var payments = Payment(paymentId="id1", orderId="o1", creditCardNumber="11111", securityCode="123", cardExpiration="0208")
        (mockPay.createPayment _).expects("o1","11111","123","0208").returning(Future(())).once()

        val routesUnderTest = new Routes(null,null, null, null, mockPay).routes
        val request = HttpRequest(
            method = HttpMethods.POST,
            uri = "/user/books/order/payment",
            entity = FormData(("orderId","o1"),("creditCardNumber","11111"),("securityCode","123"),("cardExpiration","0208")).toEntity
        )
        request ~> routesUnderTest ~> check {
            status should ===(StatusCodes.OK)

            contentType should ===(ContentTypes.`text/plain(UTF-8)`)

        }
    }

    
}

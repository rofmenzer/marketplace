
import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers}
import org.slf4j.LoggerFactory
import poca._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}


class DatabaseTest extends AnyFunSuite with Matchers with BeforeAndAfterAll with BeforeAndAfterEach with LazyLogging {
  val rootLogger: Logger = LoggerFactory.getLogger("com").asInstanceOf[Logger]
  rootLogger.setLevel(Level.INFO)
  val slickLogger: Logger = LoggerFactory.getLogger("slick").asInstanceOf[Logger]
  slickLogger.setLevel(Level.INFO)
  implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global


  // In principle, mutable objets should not be shared between tests, because tests should be independent from each other. However for performance the connection to the database should not be recreated for each test. Here we prefer to share the database.
  override def beforeAll() {
    val isRunningOnCI = sys.env.getOrElse("CI", "") != ""
    val configName = if (isRunningOnCI) "myTestDBforCI" else "myTestDB"
    val config = ConfigFactory.load().getConfig(configName)
    MyDatabase.initialize(config)
  }

  override def afterAll() {
    MyDatabase.db.close
  }

  override def beforeEach() {
    val resetSchema = sqlu"drop schema public cascade; create schema public;"
    val resetFuture: Future[Int] = MyDatabase.db.run(resetSchema)
    Await.result(resetFuture, Duration.Inf)
    new RunMigrations(MyDatabase.db)()
  }

  test("Users.createUser should create a new user") {
    val users: Users = new Users()

    val createUserFuture: Future[Unit] = users.createUser("toto")
    Await.ready(createUserFuture, Duration.Inf)

    // Check that the future succeeds
    createUserFuture.value should be(Some(Success(())))

    val getUsersFuture: Future[Seq[User]] = users.getAllUsers()
    var allUsers: Seq[User] = Await.result(getUsersFuture, Duration.Inf)

    allUsers.length should be(1)
    allUsers.head.username should be("toto")
  }

  test("Users.createUser returned future should fail if the user already exists") {
    val users: Users = new Users()

    val createUserFuture: Future[Unit] = users.createUser("toto")
    Await.ready(createUserFuture, Duration.Inf)

    val createDuplicateUserFuture: Future[Unit] = users.createUser("toto")
    Await.ready(createDuplicateUserFuture, Duration.Inf)

    createDuplicateUserFuture.value match {
      case Some(Failure(exc: UserAlreadyExistsException)) => {
        exc.getMessage should equal("A user with username 'toto' already exists.")
      }
      case _ => fail("The future should fail.")
    }
  }

  test("Users.getUserByUsername should return no user if it does not exist") {
    val users: Users = new Users()

    val createUserFuture: Future[Unit] = users.createUser("toto")
    Await.ready(createUserFuture, Duration.Inf)

    val returnedUserFuture: Future[Option[User]] = users.getUserByUsername("somebody-else")
    val returnedUser: Option[User] = Await.result(returnedUserFuture, Duration.Inf)

    returnedUser should be(None)
  }

  test("Users.getUserByUsername should return a user") {
    val users: Users = new Users()

    val createUserFuture: Future[Unit] = users.createUser("toto")
    Await.ready(createUserFuture, Duration.Inf)

    val returnedUserFuture: Future[Option[User]] = users.getUserByUsername("toto")
    val returnedUser: Option[User] = Await.result(returnedUserFuture, Duration.Inf)

    returnedUser match {
      case Some(user) => user.username should be("toto")
      case None => fail("Should return a user.")
    }
  }

  test("Users.getAllUsers should return a list of users") {
    val users: Users = new Users()

    val createUserFuture: Future[Unit] = users.createUser("riri")
    Await.ready(createUserFuture, Duration.Inf)

    val createAnotherUserFuture: Future[Unit] = users.createUser("fifi")
    Await.ready(createAnotherUserFuture, Duration.Inf)

    val returnedUserSeqFuture: Future[Seq[User]] = users.getAllUsers()
    val returnedUserSeq: Seq[User] = Await.result(returnedUserSeqFuture, Duration.Inf)

    returnedUserSeq.length should be(2)
  }

  // tests for books
  test("Books.getAllBooks should return a list of books") {
    val books: Books = new Books()

    val returnedBookSeqFuture: Future[Seq[Book]] = books.getAllBooks()
    val returnedBookSeq: Seq[Book] = Await.result(returnedBookSeqFuture, Duration.Inf)

    returnedBookSeq.length should be(0)
  }


  test("Books.getBookById should return no book if it does not exist") {
    val books: Books = new Books()


    val createBookFuture: Future[Unit] = books.createBook("B1", "A1", "S1", "Y1", "IS1", 5.0, 10)


    Await.ready(createBookFuture, Duration.Inf)

    val returnedBookFuture: Future[Option[Book]] = books.getBookById("-1")
    val returnedBook: Option[Book] = Await.result(returnedBookFuture, Duration.Inf)

    returnedBook should be(None)
  }


  test("Books.updupdateBookate should updateBook book if it exist") {


    val books: Books = new Books()

    val createBookFuture: Future[Unit] = books.createBook("B1", "A1", "S1", "Y1", "IS1", 5.0, 10)

    Await.ready(createBookFuture, Duration.Inf)

    val returnedBookFuture: Future[Option[Book]] = books.getBookByTitle("B1")

    val returnedBook: Option[Book] = Await.result(returnedBookFuture, Duration.Inf)

    returnedBook match {
      case Some(book) => books.updateBook(book.id, Book(book.id, "B2", "A2", "S2", "Y2", "IS2", 5.0, 10))
      case None => fail("Should return a book.")
    }

    val returnedBookFutureAfterUpdate: Future[Option[Book]] = books.getBookByTitle("B2")

    val returnedBookAfterUpdate: Option[Book] = Await.result(returnedBookFutureAfterUpdate, Duration.Inf)

    returnedBookAfterUpdate match {
      case Some(book) => book.title should be("B2")
      case None => fail("Should return a book.")
    }


  }

  /*test("Books.getBookById should return a book") {
      val books: Books = new Books()

      val createBookFuture: Future[Unit] = books.createBook("B1", "A1", "S1", "Y1", "IS1")
      Await.ready(createBookFuture, Duration.Inf)

      val returnedBookFuture: Future[Option[Book]] = books.getBookById("toto")
      val returnedBook: Option[Book] = Await.result(returnedBookFuture, Duration.Inf)

      returnedBook match {
          case Some(book) => book.bookname should be("toto")
          case None => fail("Should return a book.")
      }
  }*/

  // Test for orders
  test("Orders.getAllOrders should return a list of orders") {
    val orders: Orders = new Orders()

    val returnedOrderSeqFuture: Future[Seq[Order]] = orders.getAllOrders()
    val returnedOrderSeq: Seq[Order] = Await.result(returnedOrderSeqFuture, Duration.Inf)

    returnedOrderSeq.length should be(0)
  }

  test("Orders.createOrder should fail if the book does not exist") {
    val orders: Orders = new Orders()

    val createOrderFuture: Future[Unit] = orders.createOrder("id1", "", "", 2)
    Await.ready(createOrderFuture, Duration.Inf)

    createOrderFuture.value match {
      case Some(Failure(exc: BookNotExistException)) => {
        exc.getMessage should equal("A book with id 'id1' does not exist.")
      }
      case _ => fail("The future should fail.")
    }

  }

  test("Orders.createOrder should fail if the book quantity = 0") {
    val orders: Orders = new Orders()

    val books: Books = new Books()

    val createBookFuture: Future[Unit] = books.createBook("B1", "A1", "S1", "Y1", "IS1", 10.0, 0)
    Await.ready(createBookFuture, Duration.Inf)

    val bookFuture: Future[Option[Book]] = books.getBookByTitle("b1")
    Await.ready(bookFuture, Duration.Inf)

    bookFuture.map(option => option.map(book => {
      val createOrderFuture: Future[Unit] = orders.createOrder(book.id, "", "", 2)
      Await.ready(createOrderFuture, Duration.Inf)

      createOrderFuture.value match {
        case Some(Failure(exc: QuantityNotSufficientException)) => {
          exc.getMessage should equal("The book with id 'id1' is not available.")
        }
        case _ => fail("The future should fail.")
      }

    }))


  }


  test("Orders.getOrdersByUserId should return list empty if user does not exist") {
    val orders: Orders = new Orders()

    val createOrderFuture: Future[Unit] = orders.createOrder("book1", "", "", 1)


    val returnedOrderFuture: Future[Seq[Order]] = orders.getOrdersByUserId("0")
    val returnedOrder: Seq[Order] = Await.result(returnedOrderFuture, Duration.Inf)

    returnedOrder.length should be(0)
  }

  test("Orders.getOrdersByBookId should return list empty if book does not exist") {
    val orders: Orders = new Orders()

    val createOrderFuture: Future[Unit] = orders.createOrder("book1", "", "", 1)


    val returnedOrderFuture: Future[Seq[Order]] = orders.getOrdersByBookId("-1")
    val returnedOrder: Seq[Order] = Await.result(returnedOrderFuture, Duration.Inf)

    returnedOrder.length should be(0)
  }


  test("Orders.getOrdersByBookIdAndStatus should return list empty if book does not exist or status not equal") {
    val orders: Orders = new Orders()

    val createOrderFuture: Future[Unit] = orders.createOrder("-1", "", "", 1)


    val returnedOrderFuture: Future[Seq[Order]] = orders.getOrdersByBookIdAndStatus("-1", "x")
    val returnedOrder: Seq[Order] = Await.result(returnedOrderFuture, Duration.Inf)

    returnedOrder.length should be(0)
  }
  


    test("Users.getAllShippings should return a list of shippings") {
    val shippings: Shippings = new Shippings()

    val createShippingFuture: Future[Unit] = shippings.createShipping("b1", "adr1", "000", "m1", "c1", "p1", "s1")
    Await.ready(createShippingFuture, Duration.Inf)

    val createAnotherShippingFuture: Future[Unit] = shippings.createShipping("b2", "adr2", "000", "m2", "c2", "p2", "s2")
    Await.ready(createAnotherShippingFuture, Duration.Inf)

    val returnedShippingSeqFuture: Future[Seq[Shipping]] = shippings.getAllShippings()
    val returnedShippingSeq: Seq[Shipping] = Await.result(returnedShippingSeqFuture, Duration.Inf)

    returnedShippingSeq.length should be(2)
  }
  
  
  test("Shippings.validateShipment should validate a shipment for order") {
    val shippings: Shippings = new Shippings()

    val createShippingFuture: Future[Unit] = shippings.createShipping("b1", "adr1", "000", "m1", "c1", "p1", "idle")
    Await.ready(createShippingFuture, Duration.Inf)
    
    val validateShippingFuture: Future[Unit] = shippings.validateShipment("b1")
    Await.ready(validateShippingFuture, Duration.Inf)
    
    val returnedShippingSeqFuture: Future[Seq[Shipping]] = shippings.getAllShippings()
    val returnedShippingSeq: Seq[Shipping] = Await.result(returnedShippingSeqFuture, Duration.Inf)
    
    
    returnedShippingSeq.head.status should be("En cours de livraison")
    
    
  }
  
  
  test("Shippings.getShippingsByBookId should return list empty if book_id does not exist") {
    val shippings: Shippings = new Shippings()

    val createShippingFuture: Future[Unit] = shippings.createShipping("b1", "adr1", "000", "m1", "c1", "p1", "idle")
    Await.ready(createShippingFuture, Duration.Inf)

    val returnedShippingFuture: Future[Seq[Shipping]] = shippings.getShippingsByBookId("b2")
    val returnedShipping: Seq[Shipping] = Await.result(returnedShippingFuture, Duration.Inf)

    returnedShipping.length should be(0)
  }
  
  
  
  test("Shippings.createShipping should return list empty if the book does not exist") {
    val shippings: Shippings = new Shippings()

    val createShippingFuture: Future[Unit] = shippings.createShipping("", "adr1", "000", "m1", "c1", "p1", "idle")
    Await.ready(createShippingFuture, Duration.Inf)

    val returnedShippingSeqFuture: Future[Seq[Shipping]] = shippings.getAllShippings()
    val returnedShippingSeq: Seq[Shipping] = Await.result(returnedShippingSeqFuture, Duration.Inf)
    
    
    returnedShippingSeq.length should be(1)

  }


}

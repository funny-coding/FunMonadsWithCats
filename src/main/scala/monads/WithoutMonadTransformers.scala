package monads

import scala.concurrent.Future

object WithoutMonadTransformers {
  case class User(name: String)
  case class Address(city: String)

  def getUser(name: String): Future[User] = Future.successful(User("Nader"))

  def getAddress(user: User): Future[Address] = Future.successful(Address("Paris"))

  def getCity: Future[String] =
    for {
      user <- getUser("Nader")
      address <- getAddress(user)
    } yield address.city

}

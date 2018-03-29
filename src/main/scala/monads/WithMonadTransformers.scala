package monads

import cats.data.OptionT

import scala.concurrent.Future

object WithMonadTransformers {
  case class User(name: String)
  case class Address(city: String)

  def getUser(name: String): Future[Option[User]] = Future.successful(Some(User("Nader")))

  def getAddress(user: User): Future[Option[Address]] = Future.successful(Some(Address("Paris")))

  def getCityWithNoMonadTransformers: Future[Option[String]] =
    for {
      maybeUser <- getUser("Nader")
      maybeCity <- maybeUser match {
        case Some(user) => getAddress(user).map(_.map(_.city))
        case None => Future.successful(None)
      }
    } yield maybeCity


  def getCityWithMonadT: Future[Option[String]] =
    (for {
      user <- OptionT(getUser("Nader"))
      address <- OptionT(getAddress(user))
    } yield address.city).value
}

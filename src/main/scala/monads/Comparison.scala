package monads

import cats.data.{EitherT, OptionT}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._


import scala.concurrent.Future

object SuperNaive {
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

object Humm {
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

object GettinMessy {
  case class User(name: String)
  case class MyError(msg: String)

  def existUser(id: String): Future[Option[User]] = {
    Future.successful(Some(User(id)))
  }

  def canBeModified(u: User): Future[Boolean] = {
    Future.successful(true)
  }

  def modify(u: User): Future[User] = {
    Future.successful(u)
  }

  def modifyUser(u: User): Future[Either[MyError, User]] = {
    existUser("YOLO").flatMap { maybeUser =>
      maybeUser match {
        case Some(user) => canBeModified(user).flatMap { modificationAccepted =>
          if (modificationAccepted)
            modify(user).map(Right(_))
          else {
            Future.successful(Left(MyError("Cant update User")))
          }
        }
        case None => Future.successful(Left(MyError("Cant find User")))
      }
    }
  }

}

object Better {
  case class User(name: String)
  case class MyError(msg: String)

  type ResultT[F[_], A] = EitherT[F, MyError, A]
  type FutureResult[A] = ResultT[Future, A]

  def existUser(name: String): FutureResult[User] = EitherT.fromEither {
    if (name == "god") Right(User("god"))
    else Left(MyError("haha you can't, you ain't god"))
  }

  def canBeModified(u: User): FutureResult[User] = EitherT.fromEither {
    if (u.name == "god") Right(u)
    else Left(MyError("hahah can't modify, you ain't god"))
  }

  def modify(u: User): FutureResult[User] = EitherT {
    if(u.name == "god") Future.successful(Right(u))
    else Future.successful(Left(MyError("haha can't modify, you ain't god")))
  }

  def modifUser(u: User): Future[Either[MyError, User]] = {
    (for {
      user <- existUser(u.name)
      _ <- canBeModified(user)
      modifiedUser <- modify(user)
    } yield modifiedUser).value
  }

}






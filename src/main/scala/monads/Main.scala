package monads

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object Main extends App {

  val results = Await.result(
    Future.sequence(
      List(
        SuperNaive.getCity,
        Humm.getCityWithMonadT,
        GettinMessy.modify(GettinMessy.User("god")),
        Better.modifUser(Better.User("god"))
      )),
    1.second
  )

  results.zipWithIndex.foreach {
    case (res, i) => println(s"Example ${i + 1}: $res")
  }

}
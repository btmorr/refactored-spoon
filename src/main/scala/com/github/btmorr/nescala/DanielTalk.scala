package com.github.btmorr.nescala

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.future._
import org.atnos.eff.interpret._

object DanielTalk extends App {
	// final case class Username(value: String)
	// final case class Email(value: String)
	// final case class User(username: Username, email: Email)

	// sealed trait UserDSL[A]
	// object UserDSL {
	// 	final case class Save(user: User) extends UserDSL[Unit]
	// 	final case class Get(username: Username) extends UserDSL[Option[User]]
	// }

	// object UserService {
	// 	// UserDSL is a member of STACK (the stack is actually more of a set of things)
	// 	type _user[STACK] = UserDSL |= STACK

	// 	def save[STACK : _user](user: User): Eff[STACK, Unit] = 
	// 		send(UserDSL.Save(user))

	// 	def get[STACK : _user](username: Username): Eff[STACK, Option[User]] = 
	// 		send(UserDSL.Get(username))

	// }

	// final case class Metric(value: String)

	// sealed trait MetricsDSL[A]
	// object MetricsDSL {
	// 	type _user[STACK] = UserDSL |= STACK

	// 	final case class Send(metric: Metric) extends MetricsDSL[Unit]

	// 	def send[STACK : _metrics](metric: Metric): Eff[STACK, Unit] = 
	// 		send(MetricsDSL.Send(metric))
	// }

	// object UserAndMetricsService {
	// 	type _user[STACK] = UserDSL |= STACK
	// 	type _metrics[STACK] = MetricsDSL |= STACK

	// 	def sendUserAndMetrics[STACK : _user : _metrics](
	// 		user: User


	// 	)
	// }

	// object UserServiceFutureInterpreter {
	// 	var map = Map[Username, User]()

	// 	def get(username: Username): Future[Option[User]] = 
	// 		Future(map.get(username))

	// 	def save(user: User): Future[Unit] = 
	// 		Future({ map = map + (user.username -> user)})

	// 	def runUser[STACK_WITH_USER_AND_FUTURE, STACK_WITH_FUTURE, A](
	// 		effect: Eff[STACK_WITH_USER_AND_FUTURE, A])(implicit
	// 		user: Member.Aux[UserDSL, STACK_WITH_USER_AND_FUTURE, STACK_WITH_FUTURE]
	// 		futureProof: _future[STACK_WITH_FUTURE]
	// 		): Eff[STACK_WITH_FUTURE, A] = {
	// 			translate(effects)(new Translate[UserDSL, STACK_WITH_FUTURE]) {
	// 				def apply[X](dsl: UserDSL[X]): Eff[STACK_WITH_FUTURE, X] = dsl match {
	// 					case Save(user) => future.fromFuture(save(user))
	// 					case Get(username) => future.fromFuture(get(username))
	// 				}
	// 			}
	// 		}
	// }

	// object Main extends App {
	// 	implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

	// 	import UserService._

	// 	val user = User(Username("joe"), Email("joe@joes.com"))

	// 	def program[USER_STACK: _user]: Eff[USER_STACK, Option[User]] = for {
	// 		_ <- save(user)
	// 		optionUser <- get(user.username)
	// 		_ <- save(optionUser.get.copy(username = Username("Hi!")))
	// 		updatedUser <- get(Username("Hi!"))
	// 	} yield updatedUser

	// 	type MyStack = Fx2[UserDSL, TimedFuture]

	// 	val updatedUser = UserServiceFutureInterpreter.runUser(program[MyStack]).runSequential

	// 	println(Await(future, 2.seconds))
	// }
}

// Check out FreeStyle from 47 degrees

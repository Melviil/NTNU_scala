import scala.concurrent.duration._
import org.scalatest.FunSuite
import akka.actor._
import scala.concurrent._
import akka.pattern.ask
import akka.util.Timeout

import scala.util.Random

object TestHelper {

  def createBank(bankId: String): (ActorRef, Bank) = {
    val bankRef: ActorRef = BankManager.createBank(bankId)
    implicit val timeout = Timeout(5 seconds)
    val bank = Await.result(ask(bankRef, IdentifyActor).mapTo[Bank], 10 seconds)

    (bankRef, bank)
  }

  def createBankAccount(bankId: String, amount: Double): (ActorRef, Account) = {
    val bank: ActorRef = BankManager.findBank(bankId)
    implicit val timeout = Timeout(5 seconds)
    val accountRef = Await.result(ask(bank, CreateAccountRequest(amount)).mapTo[ActorRef], 10 seconds)
    val account = Await.result(ask(accountRef, IdentifyActor).mapTo[Account], 10 seconds)

    (accountRef, account)
  }

  def waitUntilAllTransactionsAreCompleted(accounts: List[Account]): Unit = {
    var completed = false
    while (!completed) {
      Thread.sleep(500)
      var completedNow = true
      accounts.foreach(a => {
        completedNow = completedNow && a.allTransactionsCompleted
      })
      completed = completedNow
    }
  }
}





class Test16 extends FunSuite {

  test("Transaction to a non-existing bank should fail, and account balance should not be affected and transaction list should hold correct status information.") {
    val bank1: ActorRef = BankManager.createBank("1600")

    val (accountRef1, account1) = TestHelper.createBankAccount("1600", 1000)

    account1.transferTo("99998888", 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1))

    account1.getTransactions.foreach(t => {
      assert(t.isCompleted)
      assert(!t.isSuccessful)
    })

    assert(account1.getBalanceAmount == 1000)

  }
}

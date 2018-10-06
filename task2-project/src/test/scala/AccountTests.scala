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


class Test01 extends FunSuite {

  test("Add new bank") {
    val bankRef: ActorRef = BankManager.createBank("2001")
    implicit val timeout = Timeout(5 seconds)
    val bank: Bank = Await.result(ask(bankRef, IdentifyActor).mapTo[Bank], 10 seconds)
    assert(bank.bankId == "2001")
  }

}


class Test02 extends FunSuite {

  test("Add new bank account") {
    val bank: ActorRef = BankManager.createBank("2002")
    val (accountRef, account) = TestHelper.createBankAccount("2002", 1000)
    assert(account.accountId == "1001" && account.getBalanceAmount == 1000)
  }

}

class Test03 extends FunSuite {

  test("Valid transaction within same bank, accounts should have correct balance.") {
    val bank: ActorRef = BankManager.createBank("2003")
    val (accountRef1, account1) = TestHelper.createBankAccount("2003", 1000)
    val (accountRef2, account2) = TestHelper.createBankAccount("2003", 1000)

    implicit val timeout = Timeout(5 seconds)

    account1.transferTo(account2.accountId, 200)

    TestHelper.waitUntilAllTransactionsAreCompleted(List(account1, account2))
    assert(account1.getBalanceAmount == 800 && account2.getBalanceAmount == 1200)
  }
}

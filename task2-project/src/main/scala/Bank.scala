import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1000)

    def createAccount(initialBalance: Double): ActorRef = {
        // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
        BankManager.createAccount(accountCounter.incrementAndGet().toString, bankId, initialBalance)
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        // Use BankManager to look up an account with ID accountId
        Option(BankManager.findAccount(bankId, accountId))
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        // Use BankManager to look up a different bank with ID bankId
        Option(BankManager.findBank(bankId))
    }

    override def receive = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance)// Create a new account
        case GetAccountRequest(id) => findAccount(id) // Return account
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt => {
          t.transaction.receiptReceived = true
          val isInternal = t.toAccountNumber.length <= 4
          val toBankId = if (isInternal) bankId else t.toAccountNumber.substring(0, 4)
           if ( toBankId == bankId){
               findAccount(t.toAccountNumber takeRight 4).get ! t // if the account is from my bank I send the transaction to it
           }else{
             findOtherBank(toBankId).get ! t // otherwise I send it to the corresponding bank
           }
        }

      //  case msg => ???
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout = new Timeout(5 seconds)
        val isInternal = t.to.length <= 4
        val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
        val toAccountId = if (isInternal) t.to else t.to.substring(4)
        val transactionStatus = t.status
        if ( toBankId == bankId){
            try{
              var account = findAccount(toAccountId)  // if the account is from my bank I send the transaction to it
              account.get ! t // if the account is from my bank I send the transaction to it
            }catch{
              case e: Exception => t.status = TransactionStatus.FAILED
              sender ! t
            }
        }else{
          try{
            var bank = findOtherBank(toBankId)  // otherwise I send it to the corresponding bank
            bank.get ! t // if the account is from my bank I send the transaction to it
          }catch{
            case e: Exception => t.status = TransactionStatus.FAILED
            sender ! t
          }
        // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
        // HINT: Make use of the variables that have been defined above.
      }
    }
}

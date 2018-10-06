import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        // Should return a list of all Transaction-objects stored in transactions
        transactions.values.toList
    }

    def allTransactionsCompleted: Boolean = {
        // Should return whether all Transaction-objects in transactions are completed
        var completed:Boolean = false
        if ( transactions.values.toList.isEmpty){
          completed = true
        }else{
          for ( transaction <- transactions.values.toList){
            if ( transaction.isCompleted){
              completed = true
            }
          }
        }
        completed
    }

    def withdraw(amount: Double): Unit = {
    	if ( balance.amount < amount){
    		throw new NoSufficientFundsException()
    	}
    	else if ( amount < 0) {
    		throw new IllegalAmountException()
    	}else{
    		this.synchronized {
				balance.amount = balance.amount - amount
			}
    	}
    }

    def deposit(amount: Double): Unit = {
    	if ( amount < 0 ){
			throw new IllegalAmountException()
    	}else{
    		this.synchronized {
    			balance.amount = balance.amount + amount
    		}
    	}
    }

    def getBalanceAmount: Double = {
    	balance.amount
    }

    def sendTransactionToBank(t: Transaction): Unit = {
        // Should send a message containing t to the bank of this account
        var bank = BankManager.findBank(bankId)
        bank ! t

    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)
            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }
        t
    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive = {
    case IdentifyActor => sender ! this

		case TransactionRequestReceipt(to, transactionId, transaction) => {
      for ( transaction <- getTransactions){
        if ( transaction.id == transactionId){
          transaction.status = TransactionStatus.SUCCESS
        }
      }
		}

		case BalanceRequest => sender ! getBalanceAmount // Should return current balance

		case t: Transaction => {
      try {
          deposit(t.amount)
          val tr = new TransactionRequestReceipt(toAccountNumber = t.from, transactionId = t.id, transaction = t)
          sender ! tr
      } catch {
          case _: NoSufficientFundsException | _: IllegalAmountException =>
              t.status = TransactionStatus.FAILED
      }

		}

		//case msg => ???
    }


}

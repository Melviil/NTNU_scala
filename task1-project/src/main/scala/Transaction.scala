import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

    val queue = new scala.collection.mutable.Queue[Transaction]

    // Remove and return the first element from the queue
    def pop: Transaction = queue.dequeue

    // Return whether the queue is empty
    def isEmpty: Boolean = queue.isEmpty

    // Add new element to the back of the queue
    def push(t:  Transaction ): Unit = {
        queue += t
    } 

    // Return the first element from the queue without removing it
    def peek: Transaction = queue.front

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = queue.toIterator

}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {
      var status = TransactionStatus.SUCCESS

      def doTransaction() = {
         try { 
            from.withdraw(amount)
            to.deposit(amount)
          } catch {
            case e: Exception => status = TransactionStatus.FAILED
            var transaction = transactionsQueue.peek
            if ( allowedAttemps > 1){
               transactionsQueue.pop
               transactionsQueue.push(transaction)
               println("marche pas")
            }
          }
        var transaction = transactionsQueue.peek
        transactionsQueue.pop
        transaction.status = status
        processedTransactions.push(transaction)
        println("remove transaction to queue")
      }

      if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction()
          }
      } else to synchronized {
          from synchronized {
            doTransaction()
          }
      }
      // Extend this method to satisfy requirements.

    }
}

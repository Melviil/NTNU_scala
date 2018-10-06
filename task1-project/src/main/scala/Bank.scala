import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = generateAccountId // TODO
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = 2 // TODO

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        println("addTransactionToQueue")
        
        val t = new Transaction( transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
            transactionsQueue push t
    }

    // Hint: use a counter 
    var lastId = 0;
    def generateAccountId: Int = {
        this.synchronized{
            lastId = lastId + 1
            lastId + 1
        }
    }

    private def processTransactions: Unit = 3 // TODO

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }


    def getProcessedTransactionsAsList: List[Transaction] = {
        while (transactionsQueue.iterator.toList.size > 0){
             this.synchronized{
                println(transactionsQueue.iterator.toList.size)
                transactionsQueue.peek run
            }
        }
        processedTransactions.iterator.toList
    }

}

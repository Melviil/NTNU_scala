import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId


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

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}

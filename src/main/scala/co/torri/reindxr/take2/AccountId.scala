package co.torri.reindxr.take2

sealed abstract case class AccountId(value: String)

object AccountId {
  def apply(id: String): AccountId = new AccountId(id) {}
}
package nubank

import java.time.LocalDate
import scala.io.Source
import scala.annotation.tailrec

case class InvoiceEntry(date: LocalDate, category: String, title: String, amount: Amount)

object Invoice {
  val RewardsCategory = "Ajuste"
  val RewardsCreditTitle = "Rewards - Crédito de compra apagada"
  val RewardsSubscriptionCanceledTitle = "Rewards - Estorno da assinatura"
  private val AjusteDeIofTitleRE = """IOF de "(.+)"""".r

  def parseCsv(filePath: String): Seq[InvoiceEntry] = {
    val lines = Source.fromFile(filePath).getLines()
    val header = lines.next()
    assert(header == "date,category,title,amount")

    val entries = for {
      line <- lines.toSeq
      Array(dateStr, category, title, amountStr) = line.split(",", 4)
      if category != "Pagamento" || title != "Pagamento recebido"
    } yield {
      val date = LocalDate.parse(dateStr)
      val amount = parseAmount(amountStr)
      InvoiceEntry(date, category, title, amount)
    }

    mergeIOF(entries)
  }

  private def parseAmount(amountStr: String): Amount = {
    val centavosStr = amountStr.split('.') match {
      case Array(intPart) => intPart + "00"
      case Array(intPart, fracPart) if fracPart.length == 1 => intPart + fracPart + "0"
      case Array(intPart, fracPart) if fracPart.length == 2 => intPart + fracPart
    }
    Amount(centavosStr.toInt)
  }

  @tailrec
  private def mergeIOF(entries: Seq[InvoiceEntry]): Seq[InvoiceEntry] = {
    def removeIofEntry(es: Seq[InvoiceEntry]) = {
      val index = es.indexWhere { entry =>
        entry.category == "Ajuste" && AjusteDeIofTitleRE.findFirstIn(entry.title).isDefined
      }

      if (index >= 0) {
        val iofEntry = entries(index)
        val entriesWithoutIofEntry = entries.take(index) ++ entries.drop(index + 1)
        Some((entriesWithoutIofEntry, iofEntry))
      } else {
        None
      }
    }

    def updateTransactionEntry(es: Seq[InvoiceEntry], title: String, date: LocalDate, iofAmount: Amount) = {
      val index = es.indexWhere { entry =>
        entry.title == title && entry.date == date
      }
      assert(index >= 0)

      val entry = es(index)
      val updatedEntry = entry.copy(amount = entry.amount + iofAmount)

      val updatedEntries = es.updated(index, updatedEntry)
      updatedEntries
    }

    val updatedEntries = for {
      (entriesWithoutIofEntry, iofEntry) <- removeIofEntry(entries)
      transactionTitle = iofEntry.title match { case AjusteDeIofTitleRE(title) => title }
    } yield {
      updateTransactionEntry(entriesWithoutIofEntry, transactionTitle, iofEntry.date, iofEntry.amount)
    }

    updatedEntries match {
      case Some(es) => mergeIOF(es)
      case None => entries
    }
  }
}

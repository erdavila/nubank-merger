package nubank

import Utils.WithMapFold
import scala.annotation.tailrec

object Main {

  private case class FormattedEntry(date: String, title: String, amount: String, subTotal: String, mark: Boolean)

  def main(args: Array[String]): Unit = {
    val Array(invoiceFile, historyFile) = args.take(2)
    val invoice = Invoice.parseCsv(invoiceFile)
    val history = History.parse(historyFile, 2018)

    Merger.merge(invoice, history) match {
      case Some(newInvoice) =>
        val subTotaledEntries = subTotalInvoice(newInvoice)
        val formattedEntries = formatEntries(subTotaledEntries)
        val (titleLen, amountLen, subTotalLen) = getColumnsLengths(formattedEntries)

        printEntries(titleLen, amountLen, subTotalLen)(formattedEntries)
        if (formattedEntries.exists { _.mark }) {
          println()
          println("* history entry was not found")
        }

      case None =>
        println("NO DATA!!!")
    }

    println()
  }

  private def subTotalInvoice(invoice: Seq[MergedEntry]): Seq[(MergedEntry, Amount)] = {
    val (entries, _) = invoice.mapFold(Amount(0)) { (subTotal, entry) =>
      val newSubTotal = subTotal + entry.amount
      val subTotaledEntry = (entry, newSubTotal)
      (subTotaledEntry, newSubTotal)
    }
    entries
  }

  private def formatEntries(entries: Seq[(MergedEntry, Amount)]): Seq[FormattedEntry] = {
    entries map { case (entry, subTotal) =>
      val formattedDate = entry.date.toString
      val formattedTitle = entry.title
      val formattedAmount = format(entry.amount)
      val formattedSubTotal = format(subTotal)
      FormattedEntry(formattedDate, formattedTitle, formattedAmount, formattedSubTotal, !entry.withHistoryEntry)
    }
  }

  private def getColumnsLengths(entries: Seq[FormattedEntry]): (Int, Int, Int) = {
    val titleLen = entries.map { _.title.length }.max
    val amountLen = entries.map { _.amount.length }.max
    val subTotalLen = entries.map { _.subTotal.length }.max
    (titleLen, amountLen, subTotalLen)
  }

  @tailrec
  private def printEntries(titleLen: Int, amountLen: Int, subTotalLen: Int)(entries: Seq[FormattedEntry]): Unit = {
    entries match {
      case Seq(entry, tail@_*) =>
        println(
            Seq(
                entry.date,
                padRight(entry.title, titleLen),
                padLeft(entry.amount, amountLen),
                padLeft(entry.subTotal, subTotalLen)
            ).mkString("  ") + { if (entry.mark) " *" else "" }
        )
        printEntries(titleLen, amountLen, subTotalLen)(tail)

      case Seq() =>
    }
  }

  private def padRight(str: String, len: Int): String = str.padTo(len, ' ')

  private def padLeft(str: String, len: Int): String = (" " * (len - str.length)) + str

  private def format(amount: Amount): String = {
    @tailrec
    def digitGroups(i: Int, groups: Seq[String] = Seq.empty): Seq[String] = {
      if (i >= 1000) {
        val group = "%03d".format(i % 1000)
        digitGroups(i / 1000, group +: groups)
      } else {
        i.toString +: groups
      }
    }

    val negative = amount.centavos < 0
    val centavos = math.abs(amount.centavos)

    val fracPart = centavos % 100
    val intPart = centavos / 100

    "R$ " + (if (negative) "-" else "") + digitGroups(intPart).mkString(".") + "," + "%02d".format(fracPart)
  }
}

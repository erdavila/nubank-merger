package nubank

import java.time.LocalDate

case class MergedEntry(date: LocalDate, title: String, amount: Amount, withHistoryEntry: Boolean = true)

object Merger {

  private type SolvedEntry = Either[HistoryEntry, MergedEntry]

  private implicit val mergedEntryOrdering = Ordering.by { e: MergedEntry => e.date.toEpochDay }

  def merge(invoice: Seq[InvoiceEntry], history: Seq[HistoryEntry]): Option[Seq[MergedEntry]] = {
    merge(invoice, history map { Left(_) }, Seq.empty)
      .map { case (solvedEntries, unmergedInvoiceEntries) => (trimSolvedEntries(solvedEntries), unmergedInvoiceEntries) }
      .find { case (solvedEntries, unmergedInvoiceEntries) =>
        val unmergedHistoryEntriesCount = solvedEntries.count { _.isLeft }
        val unmergedInvoiceEntriesCount = unmergedInvoiceEntries.length
        unmergedHistoryEntriesCount <= unmergedInvoiceEntriesCount
      }
      .map { case (solvedEntries, unmergedInvoiceEntries) =>
        val mergedEntries = solvedEntries.collect { case Right(mergedEntry) => mergedEntry }
        assert(mergedEntries.sorted == mergedEntries)

        val unmergedEntries = unmergedInvoiceEntries.map { entry => MergedEntry(entry.date, entry.title, entry.amount, withHistoryEntry = false) }

        (mergedEntries ++ unmergedEntries).sorted
      }
  }

  private def merge(invoice: Seq[InvoiceEntry], solvedEntries: Seq[SolvedEntry], unmerged: Seq[InvoiceEntry]): Iterator[(Seq[SolvedEntry], Seq[InvoiceEntry])] = {
    invoice match {
      case Seq(invEntry, invTail@_*) =>
        val matchingEntries = for {
          (Left(histEntry), i) <- solvedEntries.zipWithIndex
          if entriesMatch(invEntry, histEntry)
        } yield (histEntry, i)

        if (matchingEntries.isEmpty) {
          merge(invTail, solvedEntries, unmerged :+ invEntry)
        } else {
          for {
            (histEntry, i) <- matchingEntries.toIterator
            mergedEntry = mergeEntries(invEntry, histEntry)
            newSolvedEntries = solvedEntries.updated(i, Right(mergedEntry))
            solution <- merge(invTail, newSolvedEntries, unmerged)
          } yield solution
        }

      case Seq() =>
        Iterator((solvedEntries, unmerged))
    }
  }

  private def entriesMatch(invoiceEntry: InvoiceEntry, historyEntry: HistoryEntry): Boolean = {
    def invoiceEntryNotEarlyThanHistoryEntry = !(invoiceEntry.date isBefore historyEntry.date)

    def sameContent = {
      Some(invoiceEntry.amount) == historyEntry.amount &&
      invoiceEntry.title == historyEntry.title
    }

    def rewardsCredit = {
      Some(invoiceEntry.amount) == historyEntry.amount &&
      invoiceEntry.category == Invoice.RewardsCategory &&
      invoiceEntry.title == Invoice.RewardsCreditTitle &&
      historyEntry.category == History.RewardsCategory &&
      History.RewardsCreditTitleRE.findFirstIn(historyEntry.title).isDefined
    }

    def rewardsSubscriptionCancelation = {
      invoiceEntry.category == Invoice.RewardsCategory &&
      invoiceEntry.title == Invoice.RewardsSubscriptionCanceledTitle &&
      historyEntry.amount.isEmpty &&
      historyEntry.category == History.RewardsCategory &&
      historyEntry.title == History.RewardsSubscriptionCanceledTitle
    }

    invoiceEntryNotEarlyThanHistoryEntry && (sameContent || rewardsCredit || rewardsSubscriptionCancelation)
  }

  private def mergeEntries(invoiceEntry: InvoiceEntry, historyEntry: HistoryEntry): MergedEntry = {
    MergedEntry(
        date = historyEntry.date,
        title = invoiceEntry.title,
        amount = invoiceEntry.amount
    )
  }

  private def trimSolvedEntries(solution: Seq[SolvedEntry]): Seq[SolvedEntry] = {
    solution
      .dropWhile { _.isLeft }
      .reverse.dropWhile { _.isLeft }.reverse
  }
}

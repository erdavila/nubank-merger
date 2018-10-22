package nubank

import java.time.LocalDate
import java.time.MonthDay
import scala.annotation.tailrec
import scala.io.Source

case class HistoryEntry(category: String, title: String, amount: Option[Amount], date: LocalDate)

object History {

  val RewardsCategory = "Nubank Rewards"
  val RewardsCreditTitleRE = """Você apagou a compra .+ e recebeu um crédito de (R\$ .+)""".r
  val RewardsSubscriptionCanceledTitle = "Assinatura cancelada"

  class Parser(lines: Iterator[String]) {

    private val numberedLines = lines.zipWithIndex map { case (s, n) => (s, n + 1) }

    case class HistoryRawEntry(category: String, title: String, amount: Option[Amount], date: MonthDay)

    private object Skippable {
      private val SkippableCategories =
        Map(
            "Fatura paga" -> 1,
            "Pagamento recebido" -> 2,
            "Novo dispositivo autorizado" -> 2,
            "Fatura fechada" -> 3,
            "Limite alterado" -> 2,
        )

      def unapply(category: String): Option[Int] =
        SkippableCategories.get(category) orElse {
          val RE = """\d+ Parcelas Antecipadas""".r
          Option(category) collect { case RE() => 2 }
        }
    }

    private object Rewards {
      def unapply(category: String): Option[String] = if (category == RewardsCategory) Some(category) else None
    }

    private object Regular {
      private val Categories = Set[String](
        "casa",
        "educação",
        "eletrônicos",
        "lazer",
        "outros",
        "restaurante",
        "saúde",
        "serviços",
        "supermercado",
        "transporte",
        "vestuário",
      )

      def unapply(category: String): Option[String] = Categories.find(_ == category)
    }

    private val parseMonth: String => Int =
      Seq(
          "JAN", "FEV", "MAR", "ABR",
          "MAI", "JUN", "JUL", "AGO",
          "SET", "OUT", "NOV", "DEZ"
      ).zipWithIndex.toMap.mapValues { _ + 1 }

    @tailrec
    final def parse(accumulator: Seq[HistoryRawEntry] = Seq.empty): Seq[HistoryRawEntry] = {
      if (numberedLines.hasNext) {
        val entry = numberedLines.next() match {
          case (Skippable(numLines), _) => skip(numLines)
          case (Rewards(category), _) => parseRewards(category)
          case (Regular(category), _) => parseRegular(category)
          case (line, n) => sys.error(s"""Unrecognized line "$line" at $n""")
        }
        parse(entry.toSeq ++ accumulator)
      } else {
        accumulator
      }
    }

    private def skip(numLines: Int): None.type = {
      for (_ <- 1 to numLines) { numberedLines.next() }
      None
    }

    private def parseRewards(category: String): Option[HistoryRawEntry] = {
      val (title, _) = numberedLines.next()
      val amount = title match {
        case RewardsSubscriptionCanceledTitle => None
        case RewardsCreditTitleRE(amountStr) => Some(-parseAmount(amountStr))
      }
      val (dateStr, _) = numberedLines.next()
      val date = parseMonthDay(dateStr)

      val entry = HistoryRawEntry(category, title, amount, date)
      Some(entry)
    }

    private def parseRegular(category: String): Option[HistoryRawEntry] = {
      val (title, _) = numberedLines.next()
      val amount = parseAmount(numberedLines.next() match { case (line, _) => line })

      val dateStr = {
        val (line, _) = numberedLines.next()
        if (isForeignCurrency(line)) {
          numberedLines.next() match { case (ln, _) => ln }
        } else {
          line
        }
      }
      val date = parseMonthDay(dateStr)

      val entry = HistoryRawEntry(category, title, Some(amount), date)
      Some(entry)
    }

    private def parseAmount(str: String): Amount = {
      val Prefix = "R$ "
      assume(str startsWith Prefix, str)
      val centavos = str
        .drop(Prefix.length)
        .replace(".", "")
        .replace(",", "")
        .toInt
      Amount(centavos)
    }

    private def isForeignCurrency(line: String) = line.startsWith("GBP ")

    private def parseMonthDay(dateStr: String): MonthDay = {
      val Array(dayOfMonthStr, monthStr) = dateStr.split(' ')

      val month = parseMonth(monthStr)
      val dayOfMonth = dayOfMonthStr.toInt

      MonthDay.of(month, dayOfMonth)
    }
  }

  def parse(filePath: String, year: Int): Seq[HistoryEntry] = {
    val lines = Source.fromFile(filePath, "UTF-8").getLines()
    val parser = new Parser(lines)
    val rawEntries = parser.parse()

    val rawEntriesHead = rawEntries.head
    val entriesHead = HistoryEntry(
        category = rawEntriesHead.category,
        title = rawEntriesHead.title,
        amount = rawEntriesHead.amount,
        date = rawEntriesHead.date.atYear(year)
    )

    val (_, entries) = rawEntries.tail.foldLeft((entriesHead.date, Seq.empty[HistoryEntry])) { case ((prevDate, entries), rawEntry) =>
      val curDate = {
        val dt = rawEntry.date.atYear(prevDate.getYear)
        if (dt isBefore prevDate) {
          rawEntry.date.atYear(prevDate.getYear + 1)
        } else {
          dt
        }
      }

      val newEntry = HistoryEntry(
          category = rawEntry.category,
          title = rawEntry.title,
          amount = rawEntry.amount,
          date = curDate
      )

      val newEntries = entries :+ newEntry
      (curDate, newEntries)
    }

    entries
  }
}

package eurosim

import scala.util.Random

trait GameSimulator {
  def simulate(first: Team, second: Team): GameResult
  def simulateNoDraw(first: Team, second: Team): GameResultNoDraw
}

object EloSimulator {
  // Taken from: http://www.eloratings.net/europe.html
  val elo: Map[Team, Int] = Map(
    Team("Germany") -> 2037,
    Team("Spain") -> 1977,
    Team("France") -> (1947 + 100),
    Team("England") -> 1934,
    Team("Belgium") -> 1896,
    Team("Portugal") -> 1885,
    Team("Netherlands") -> 1880,
    Team("Italy") -> 1844,
    Team("Ukraine") -> 1797,
    Team("Turkey") -> 1797,
    Team("Croatia") -> 1794,
    Team("Austria") -> 1766,
    Team("Poland") -> 1762,
    Team("Switzerland") -> 1753,
    Team("Ireland") -> 1751,
    Team("Russia") -> 1747,
    Team("Romania") -> 1738,
    Team("Sweden") -> 1735,
    Team("Czech Republic") -> 1728,
    Team("Bosnia and Herzegovina") -> 1727,
    Team("Slovakia") -> 1715,
    Team("Denmark") -> 1703,
    Team("Serbia") -> 1701,
    Team("Scotland") -> 1696,
    Team("Hungary") -> 1671,
    Team("Iceland") -> 1647,
    Team("Wales") -> 1638,
    Team("Norway") -> 1608,
    Team("Bulgaria") -> 1590,
    Team("Slovenia") -> 1589,
    Team("Greece") -> 1580,
    Team("Northern Ireland") -> 1578,
    Team("Albania") -> 1578,
    Team("Belarus") -> 1553,
    Team("Finland") -> 1544,
    Team("Montenegro") -> 1529,
    Team("Estonia") -> 1470,
    Team("Armenia") -> 1460,
    Team("Georgia") -> 1450,
    Team("Latvia") -> 1442,
    Team("Northern Cyprus") -> 1413,
    Team("Macedonia") -> 1400,
    Team("Lithuania") -> 1397,
    Team("Cyprus") -> 1395,
    Team("Azerbaijan") -> 1387,
    Team("Kosovo") -> 1383,
    Team("Kazakhstan") -> 1378,
    Team("Moldova") -> 1328,
    Team("Luxembourg") -> 1218,
    Team("Faroe Islands") -> 1211,
    Team("Malta") -> 1207,
    Team("Liechtenstein") -> 1167,
    Team("Gibraltar") -> 1079,
    Team("Andorra") -> 931,
    Team("Greenland") -> 884,
    Team("San Marino") -> 861,
    Team("Monaco") -> 733,
    Team("Vatican") -> 577
  )
}

class EloSimulator extends GameSimulator {

  private val random: Random = new java.util.Random

  private def getWe(first: Team, second: Team) = {
    import EloSimulator.elo
    val dr: Double = elo(first) - elo(second)

    1.0 / (math.pow(10, (0 - dr) / 400) + 1)
  }

  override def simulate(first: Team, second: Team): GameResult = {
    // Based on http://www.inf.fu-berlin.de/inst/ag-ki/rojas_home/documents/Betreute_Arbeiten/Bachelor-Dormagen.pdf (p19)
    // Set a = 0 to not allow any draws ;)
    val a = 1.0 / 3.0
    val c = 0.28

    val We = getWe(first, second)
    val Pdraw = a * math.exp(0 - math.pow(We - 0.5, 2) / (2 * c * c))
    // roll a random number to check if its a draw
    if(random.nextDouble() < Pdraw) {
      if (Main.debug) {
        System.out.format("%s vs %s = %s(%f)\n", first, second, Draw, Pdraw: java.lang.Double)
      }
      Draw
    } else {
      simulateNoDraw(first, second)
    }
  }

  override def simulateNoDraw(first: Team, second: Team): GameResultNoDraw = {
    val We = getWe(first, second)
    val result = if (random.nextDouble() < We) Win else Loss
    if(Main.debug) {
      System.out.format("%s vs %s = %s(%f)\n", first, second, result, We: java.lang.Double)
    }
    result
  }
}
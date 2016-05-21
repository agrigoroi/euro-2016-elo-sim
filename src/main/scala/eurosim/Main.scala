package eurosim

import java.io.{File, PrintWriter}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import java.{util => ju}

import scala.util.Random._
import scala.util.Random

class Runner {
  private val simulator: GameSimulator = new EloSimulator
  private val random: Random = new java.util.Random

  def generateGroupStage: GroupStage = {
    GroupStage(
      Group(Team("France"), Team("Romania"), Team("Albania"), Team("Switzerland")),
      Group(Team("England"), Team("Russia"), Team("Wales"), Team("Slovakia")),
      Group(Team("Germany"), Team("Ukraine"), Team("Poland"), Team("Northern Ireland")),
      Group(Team("Spain"), Team("Czech Republic"), Team("Turkey"), Team("Croatia")),
      Group(Team("Belgium"), Team("Italy"), Team("Ireland"), Team("Sweden")),
      Group(Team("Portugal"), Team("Iceland"), Team("Austria"), Team("Hungary"))
    )
  }

  def simulateGroupStage(groupStage: GroupStage): SimulatedGroupStage = {
    def simulateGroup(group: Group): SimulatedGroup = {
      val teams = List(group.team1, group.team2, group.team3, group.team4)
      // I am sure there is some way to generate the games from the team list, but i am
      // too tired to try and figure out, so instead lets write all 6 game manually
      val games = List(
        (group.team1, group.team2),
        (group.team1, group.team3),
        (group.team1, group.team4),
        (group.team2, group.team3),
        (group.team2, group.team4),
        (group.team3, group.team4)
      )
      val gameResults = games.map({
        case (team1, team2) => (team1, team2, simulator.simulate(team1, team2))
      })
      val teamWithPoints = teams.map(team => {
        (team, gameResults.foldLeft(0)((points, gr) =>
          if (gr._1 == team) {
            points + gr._3.points
          } else if (gr._2 == team) {
            points + gr._3.opponentPoints
          } else {
            points
          }
        ))
      })
      // If two teams have the same amount of points, we need to use the result of the direct match
      // However, I am too lazy to do that; so instead we will sort them by random if points are equal
      val sortedTeams = teamWithPoints
        .map(twp => (twp._1, twp._2, random.nextDouble())) // Zip with a random double between 0 and 1
        .sortBy(t => t._2 + t._3) // Sort by number of points and use the random double between 0 and 1 as the tiebreaker
        .map(t => (t._1, t._2)) // Get rid of the random double between 0 and 1
        .reverse

      //noinspection ZeroIndexToHead
      SimulatedGroup(sortedTeams(0)._1, sortedTeams(1)._1, sortedTeams(2)._1, sortedTeams(3)._1, sortedTeams(2)._2)
    }

    val groupA = simulateGroup(groupStage.groupA)
    val groupB = simulateGroup(groupStage.groupB)
    val groupC = simulateGroup(groupStage.groupC)
    val groupD = simulateGroup(groupStage.groupD)
    val groupE = simulateGroup(groupStage.groupE)
    val groupF = simulateGroup(groupStage.groupF)
    SimulatedGroupStage(groupA, groupB, groupC, groupD, groupE, groupF)
  }

  def generateLast16(groupStage: SimulatedGroupStage): Last16 = {
    val groupA = groupStage.groupA
    val groupB = groupStage.groupB
    val groupC = groupStage.groupC
    val groupD = groupStage.groupD
    val groupE = groupStage.groupE
    val groupF = groupStage.groupF

    val groups = List(groupStage.groupA, groupStage.groupB, groupStage.groupC, groupStage.groupD, groupStage.groupE, groupStage.groupF)
    val luckyThirdPlaceGroups = groups
      .zip(List("A", "B", "C", "D", "E", "F"))
      .map(g => (g._2, g._1.thirdPoint, random.nextDouble()))
      .sortBy(g => g._2 + g._3)
      .take(4)
      .map(g => g._1)
      .sortBy(identity)

    // Based on the table from: https://en.wikipedia.org/wiki/UEFA_Euro_2016#Knockout_phase
    // TODO: double check this monster case condition....
    val (groupAOpponent, groupBOpponent, groupCOpponent, groupDOpponent) = luckyThirdPlaceGroups match {
      case List("A", "B", "C", "D") => (groupC.third, groupD.third, groupA.third, groupB.third)
      case List("A", "B", "C", "E") => (groupC.third, groupA.third, groupB.third, groupE.third)
      case List("A", "B", "C", "F") => (groupC.third, groupA.third, groupB.third, groupF.third)
      case List("A", "B", "D", "E") => (groupD.third, groupA.third, groupB.third, groupE.third)
      case List("A", "B", "D", "F") => (groupD.third, groupA.third, groupB.third, groupF.third)
      case List("A", "B", "E", "F") => (groupE.third, groupA.third, groupB.third, groupF.third)
      case List("A", "C", "D", "E") => (groupC.third, groupD.third, groupA.third, groupE.third)
      case List("A", "C", "D", "F") => (groupC.third, groupD.third, groupA.third, groupF.third)
      case List("A", "C", "E", "F") => (groupC.third, groupA.third, groupF.third, groupE.third)
      case List("A", "D", "E", "F") => (groupD.third, groupA.third, groupF.third, groupE.third)
      case List("B", "C", "D", "E") => (groupC.third, groupD.third, groupB.third, groupE.third)
      case List("B", "C", "D", "F") => (groupC.third, groupD.third, groupB.third, groupF.third)
      case List("B", "C", "E", "F") => (groupE.third, groupC.third, groupB.third, groupF.third)
      case List("B", "D", "E", "F") => (groupE.third, groupD.third, groupB.third, groupF.third)
      case List("C", "D", "E", "F") => (groupC.third, groupD.third, groupF.third, groupE.third)
    }

    Last16(
      PlayoffGame(groupA.second, groupC.second),
      PlayoffGame(groupD.first, groupDOpponent),
      PlayoffGame(groupB.first, groupBOpponent),
      PlayoffGame(groupF.first, groupE.second),
      PlayoffGame(groupC.first, groupCOpponent),
      PlayoffGame(groupE.first, groupD.second),
      PlayoffGame(groupA.first, groupAOpponent),
      PlayoffGame(groupB.second, groupF.second)
    )
  }

  private def simulatePlayoffGame(game: PlayoffGame): SimulatedPlayoffGame = {
    simulator.simulateNoDraw(game.teamA, game.teamB) match {
      case Win => SimulatedPlayoffGame(game.teamA, game.teamB)
      case Loss => SimulatedPlayoffGame(game.teamB, game.teamA)
    }
  }

  def simulateLast16(last16: Last16): QuarterFinals = {
    val result1 = simulatePlayoffGame(last16.game1)
    val result2 = simulatePlayoffGame(last16.game2)
    val result3 = simulatePlayoffGame(last16.game3)
    val result4 = simulatePlayoffGame(last16.game4)
    val result5 = simulatePlayoffGame(last16.game5)
    val result6 = simulatePlayoffGame(last16.game6)
    val result7 = simulatePlayoffGame(last16.game7)
    val result8 = simulatePlayoffGame(last16.game8)

    QuarterFinals(
      PlayoffGame(result1.winner, result2.winner),
      PlayoffGame(result3.winner, result4.winner),
      PlayoffGame(result5.winner, result6.winner),
      PlayoffGame(result7.winner, result8.winner)
    )
  }

  def simulateQuarterFinals(quarterFinals: QuarterFinals): SemiFinals = {
    val result1 = simulatePlayoffGame(quarterFinals.game1)
    val result2 = simulatePlayoffGame(quarterFinals.game2)
    val result3 = simulatePlayoffGame(quarterFinals.game3)
    val result4 = simulatePlayoffGame(quarterFinals.game4)

    SemiFinals(
      PlayoffGame(result1.winner, result2.winner),
      PlayoffGame(result3.winner, result4.winner)
    )
  }

  def simulateSemiFinals(semiFinals: SemiFinals): Finals = {
    val result1 = simulatePlayoffGame(semiFinals.game1)
    val result2 = simulatePlayoffGame(semiFinals.game2)

    Finals(
      PlayoffGame(result1.winner, result2.winner),
      PlayoffGame(result1.loser, result2.loser)
    )
  }

  def simulateFinals(finals: Finals): (Team, Team, Team, Team) = {
    val result = simulatePlayoffGame(finals.bigFinal)
    val thirdPlaceResult = simulatePlayoffGame(finals.smallFinal)

    (result.winner, result.loser, thirdPlaceResult.winner, thirdPlaceResult.loser)
  }
}

object statistics {

  def exportCSV(gs: GroupStage) = {
    val pw = new PrintWriter(new File("out.csv"))
    //    pw.write("Test")
    val sb = new StringBuilder
    sb.append("Team,Elo,Group,4th place,3rd place,2nd place,1st place,Last16,Quarter finals,Semi finals,Final,Winner\n")
    import scala.collection.JavaConverters._
    count.groupLast.entrySet().asScala.map(_.getKey).foreach(team => {
      val name = team.name
      val group: Char = ('A' + List(gs.groupA, gs.groupB, gs.groupC, gs.groupD, gs.groupE, gs.groupF)
        .zipWithIndex
        .find(e => List(e._1.team1, e._1.team2, e._1.team3, e._1.team4).contains(team))
        .get._2).toChar
      val elo = EloSimulator.elo(team)
      val total: Double = Option(count.groupLast.get(team)).getOrElse(atomicZero).get
      val group4th = Option(count.groupLast.get(team)).getOrElse(atomicZero).get / total
      val group3rd = Option(count.groupThird.get(team)).getOrElse(atomicZero).get / total
      val group2nd = Option(count.groupSecond.get(team)).getOrElse(atomicZero).get / total
      val group1st = Option(count.groupFirst.get(team)).getOrElse(atomicZero).get / total
      val last16 = Option(count.last16.get(team)).getOrElse(atomicZero).get / total
      val quarterFinals = Option(count.quarterFinals.get(team)).getOrElse(atomicZero).get / total
      val semiFinals = Option(count.semiFinals.get(team)).getOrElse(atomicZero).get / total
      val finals = Option(count.finals.get(team)).getOrElse(atomicZero).get / total
      val winner = Option(count.winner.get(team)).getOrElse(atomicZero).get / total
      sb.append(s"$name,$elo,$group,$group4th,$group3rd,$group2nd,$group1st,$last16,$quarterFinals,$semiFinals,$finals,$winner\n")
    })
    pw.write(sb.toString)
    pw.close()
  }

  def log() = {
    import scala.collection.JavaConverters._
    def logMap(map: ju.Map[Team, AtomicInteger]): Unit = {
      map.entrySet().asScala.map(e => (e.getKey, e.getValue.get())).toList.sortBy(_._2).foreach(e => {
        System.out.println(s"    ${e._1}: ${e._2}")
      })
    }

    System.out.println("Printing stats data: \n")
    System.out.println("  Group Last: ")
    logMap(count.groupLast)
    println()

    System.out.println("  Group Third: ")
    logMap(count.groupThird)
    println()

    System.out.println("  Group Second: ")
    logMap(count.groupSecond)
    println()

    System.out.println("  Group First: ")
    logMap(count.groupFirst)
    println()

    System.out.println("  Last 16: ")
    logMap(count.last16)
    println()

    System.out.println("  Quarter Finals: ")
    logMap(count.quarterFinals)
    println()

    System.out.println("  Semi Finals: ")
    logMap(count.semiFinals)
    println()

    System.out.println("  Final: ")
    logMap(count.finals)
    println()

    System.out.println("  Winner: ")
    logMap(count.winner)
    println()
  }

  private object count {
    val groupFirst: ju.Map[Team, AtomicInteger] = new ConcurrentHashMap[Team, AtomicInteger]()
    val groupSecond: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
    val groupThird: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
    val groupLast: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
    val last16: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
    val quarterFinals: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
    val semiFinals: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
    val finals: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
    val winner: ju.Map[Team, AtomicInteger] = new ju.HashMap[Team, AtomicInteger]()
  }

  private def atomicOne = new AtomicInteger(1)

  private def atomicZero = new AtomicInteger(0)

  private def addCount(map: ju.Map[Team, AtomicInteger], team: Team): Unit = {
    Option(map.putIfAbsent(team, atomicOne)).getOrElse(atomicZero).incrementAndGet()
  }

  def saveGroupStage(sgs: SimulatedGroupStage): Unit = {
    List(sgs.groupA, sgs.groupB, sgs.groupC, sgs.groupD, sgs.groupE, sgs.groupF)
      .foreach(group => {
        addCount(count.groupFirst, group.first)

        addCount(count.groupSecond, group.first)
        addCount(count.groupSecond, group.second)

        addCount(count.groupThird, group.first)
        addCount(count.groupThird, group.second)
        addCount(count.groupThird, group.third)

        addCount(count.groupLast, group.first)
        addCount(count.groupLast, group.second)
        addCount(count.groupLast, group.third)
        addCount(count.groupLast, group.last)
      })
  }

  private def addPlayOffGame(map: ju.Map[Team, AtomicInteger], g: PlayoffGame) = {
    addCount(map, g.teamA)
    addCount(map, g.teamB)
  }

  def saveLast16(last16: Last16): Unit = {
    List(last16.game1, last16.game2, last16.game3, last16.game4, last16.game5, last16.game6, last16.game7, last16.game8)
      .foreach(addPlayOffGame(count.last16, _))
  }

  def saveQuarterFinals(quarterFinals: QuarterFinals): Unit = {
    List(quarterFinals.game1, quarterFinals.game2, quarterFinals.game3, quarterFinals.game4)
      .foreach(addPlayOffGame(count.quarterFinals, _))
  }

  def saveSemiFinals(semiFinals: SemiFinals): Unit = {
    List(semiFinals.game1, semiFinals.game2).foreach(addPlayOffGame(count.semiFinals, _))
  }

  def saveFinals(finals: Finals): Unit = {
    addPlayOffGame(count.finals, finals.bigFinal)
  }

  def saveWinner(winner: Team): Unit = {
    addCount(count.winner, winner)
  }
}

object Main {
  private val runner = new Runner
  val debug = false

  def main(args: Array[String]): Unit = {
    for(i <- 1 to 100000) {
      simulate(i.toString)
    }
    statistics.log()
    // Need to pass group stage so that the group is populated correctly
    statistics.exportCSV(runner.generateGroupStage)

  }

  def simulate(index: String): Unit = {
    val groupStage = runner.generateGroupStage
    val sgs = runner.simulateGroupStage(groupStage)
    statistics.saveGroupStage(sgs)
    val last16 = runner.generateLast16(sgs)
    statistics.saveLast16(last16)
    val quarterFinals = runner.simulateLast16(last16)
    statistics.saveQuarterFinals(quarterFinals)
    val semiFinals = runner.simulateQuarterFinals(quarterFinals)
    statistics.saveSemiFinals(semiFinals)
    val finals = runner.simulateSemiFinals(semiFinals)
    statistics.saveFinals(finals)
    val (winner, finalist, third, forth) = runner.simulateFinals(finals)
    statistics.saveWinner(winner)
    if(debug) {
      println(groupStage)
      println(sgs)
      println(last16)
      println(quarterFinals)
      println(semiFinals)
      println(finals)
    }
    System.out.format("run_id %s => 1st: %s\t2nd: %s\t3rd: %s\t4th: %s\n", index, winner, finalist, third, forth)
  }
}
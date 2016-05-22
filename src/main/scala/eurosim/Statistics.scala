package eurosim

import java.io.{File, PrintWriter}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import java.{util => ju}
import scala.collection.JavaConverters._

/**
  * Created by Alexandru on 22/05/2016.
  */
object Statistics {

  def exportCSV(gs: GroupStage) = {
    val pw = new PrintWriter(new File("out.csv"))
    //    pw.write("Test")
    val sb = new StringBuilder
    sb.append("Team,Elo,Group,4th place,3rd place,2nd place,1st place,Last16,Quarter finals,Semi finals,Final,Winner\n")
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

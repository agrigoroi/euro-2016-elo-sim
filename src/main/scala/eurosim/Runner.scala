package eurosim

import scala.util.Random

/**
  * Created by Alexandru on 22/05/2016.
  */
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

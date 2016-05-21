package eurosim

case class Team(name: String) {
  override def toString: String = name
}

case class Group(team1: Team, team2: Team, team3: Team, team4: Team) {
  override def toString: String = String.format("Group: %s %s %s %s", team1, team2, team3, team4)
}

case class SimulatedGroup(
  first: Team,
  second: Team,
  third: Team,
  last: Team,
  thirdPoint: Int
) {
  override def toString: String = {
    String.format("Group result: \n\t1st:%s\n\t2nd:%s\n\t3rd:%s (%d)\n\t4th:%s",
      first, second, third, thirdPoint: java.lang.Integer, last)
  }
}

case class GroupStage(groupA: Group, groupB: Group, groupC: Group, groupD: Group, groupE: Group, groupF: Group) {
  override def toString: String = {
    String.format("%s\n%s\n%s\n%s\n%s\n%s\n", groupA, groupB, groupC, groupD, groupE, groupF)
  }
}

case class SimulatedGroupStage(groupA: SimulatedGroup, groupB: SimulatedGroup, groupC: SimulatedGroup, groupD: SimulatedGroup, groupE: SimulatedGroup, groupF: SimulatedGroup) {
  override def toString: String = {
    String.format("%s\n%s\n%s\n%s\n%s\n%s\n", groupA, groupB, groupC, groupD, groupE, groupF)
  }
}

case class PlayoffGame(teamA: Team, teamB: Team) {
  override def toString: String = String.format("%s vs %s", teamA, teamB)
}
case class SimulatedPlayoffGame(winner: Team, loser: Team) {
  override def toString: String = String.format("%s won %s", winner, loser)
}

case class Last16(game1: PlayoffGame, game2: PlayoffGame, game3: PlayoffGame, game4: PlayoffGame, game5: PlayoffGame,
  game6: PlayoffGame, game7: PlayoffGame, game8: PlayoffGame) {
  override def toString: String = {
    String.format("Last 16:\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s", game1, game2, game3, game4, game5, game6, game7, game8)
  }
}
case class QuarterFinals(game1: PlayoffGame, game2: PlayoffGame, game3: PlayoffGame, game4: PlayoffGame) {
  override def toString: String = {
    String.format("Quarerfinals:\n\t%s\n\t%s\n\t%s\n\t%s", game1, game2, game3, game4)
  }
}
case class SemiFinals(game1: PlayoffGame, game2: PlayoffGame) {
  override def toString: String = {
    String.format("Semifinals:\n\t%s\n\t%s", game1, game2)
  }
}
case class Finals(bigFinal: PlayoffGame, smallFinal: PlayoffGame) {
  override def toString: String = {
    String.format("Final:%s\nThird place game:%s", bigFinal, smallFinal)
  }
}
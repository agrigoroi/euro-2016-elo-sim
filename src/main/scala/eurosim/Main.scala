package eurosim


object Main {
  private val runner = new Runner
  val debug = false

  def main(args: Array[String]): Unit = {
    for(i <- 1 to 1000000) {
      simulate(i.toString)
    }
    Statistics.log()
    // Need to pass group stage so that the group is populated correctly
    Statistics.exportCSV(runner.generateGroupStage)

  }

  def simulate(index: String): Unit = {
    val groupStage = runner.generateGroupStage
    val sgs = runner.simulateGroupStage(groupStage)
    Statistics.saveGroupStage(sgs)
    val last16 = runner.generateLast16(sgs)
    Statistics.saveLast16(last16)
    val quarterFinals = runner.simulateLast16(last16)
    Statistics.saveQuarterFinals(quarterFinals)
    val semiFinals = runner.simulateQuarterFinals(quarterFinals)
    Statistics.saveSemiFinals(semiFinals)
    val finals = runner.simulateSemiFinals(semiFinals)
    Statistics.saveFinals(finals)
    val (winner, finalist, third, forth) = runner.simulateFinals(finals)
    Statistics.saveWinner(winner)
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
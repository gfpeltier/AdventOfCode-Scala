package aoc2021

object Day12 {

  val smallCave = raw"([a-z]+)".r
  val bigCave = raw"([A-Z]+)".r

  sealed trait Cave {}

  object Cave {
    def apply(name: String): Cave = name match {
      case "start" => StartCave()
      case "end" => EndCave()
      case smallCave(cName) => SmallCave(cName)
      case bigCave(cName) => BigCave(cName)
    }
  }

  case class StartCave() extends Cave

  case class EndCave() extends Cave

  case class SmallCave(name: String) extends Cave

  case class BigCave(name: String) extends Cave

  type Graph = Map[Cave, Set[Cave]]

  def parseGraph(lines: Seq[String]): Graph =
    lines.map(l => (l.split('-')(0), l.split('-')(1)))
      .foldLeft(Map.empty[Cave, Set[Cave]])((g, edge) =>
        g.updatedWith(Cave(edge._1)) {
          case Some(sibs) => Some(sibs + Cave(edge._2))
          case None => Some(Set(Cave(edge._2)))
        }.updatedWith(Cave(edge._2)) {
          case Some(sibs) => Some(sibs + Cave(edge._1))
          case None => Some(Set(Cave(edge._1)))
        }
      )

  def calcRoutes(graph: Graph): Set[List[Cave]] = {
    def calcRoutes(currCave: Cave, route: List[Cave], routes: Set[List[Cave]]): Set[List[Cave]] = {
      currCave match {
        case _: StartCave => routes
        case endCave: EndCave => routes + (route :+ endCave)
        case smallCave: SmallCave =>
          if (!route.contains(smallCave)) {
            graph(smallCave).map(c => calcRoutes(c, route :+ smallCave, routes)).reduce(_.union(_))
          } else
            routes
        case bigCave: BigCave => graph(bigCave).map(c => calcRoutes(c, route :+ bigCave, routes)).reduce(_.union(_))
      }
    }

    graph(StartCave())
      .map(c => calcRoutes(c, List(StartCave()), Set.empty[List[Cave]]))
      .reduce(_.union(_))
  }

  def part1(lines: Seq[String]): Int = calcRoutes(parseGraph(lines)).size

  def calcRoutes2(graph: Graph): Set[List[Cave]] = {
    def calcRoutes(
      currCave: Cave,
      route: List[Cave],
      routes: Set[List[Cave]],
      smRevisited: Boolean
    ): Set[List[Cave]] = {
      currCave match {
        case _: StartCave => routes
        case endCave: EndCave => routes + (route :+ endCave)
        case smallCave: SmallCave =>
          if (!route.contains(smallCave)) {
            graph(smallCave).map(c => calcRoutes(c, route :+ smallCave, routes, smRevisited)).reduce(_.union(_))
          } else if(!smRevisited) {
            graph(smallCave).map(c => calcRoutes(c, route :+ smallCave, routes, true)).reduce(_.union(_))
          }else
            routes
        case bigCave: BigCave =>
          graph(bigCave).map(c => calcRoutes(c, route :+ bigCave, routes, smRevisited)).reduce(_.union(_))
      }
    }

    graph(StartCave())
      .map(c => calcRoutes(c, List(StartCave()), Set.empty[List[Cave]], false))
      .reduce(_.union(_))
  }

  def part2(lines: Seq[String]): Int = calcRoutes2(parseGraph(lines)).size

}

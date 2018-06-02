object Kakuro extends App {
  case class Approach1(total: Int, addend: Int, secure: Set[Int] = Set.empty) {
    //create list of candidates
    def list: List[Int] = (1 to 9).toList
    //get all existent combinations of size addend
    def combinations: Iterator[List[Int]] = list.combinations(addend)
    //get only the ones that sum total
    def candidates: Iterator[List[Int]] = combinations.filter(item => item.sum == total)
    //get only the ones that have a number in their secure set
    def securedCandidates: Iterator[List[Int]] = candidates.filter(item => secure.subsetOf(item.toSet))
    //return them in a list
    def result: List[List[Int]] = securedCandidates.toList

  }

  case class ApproachInline(total: Int, addend: Int, secure: Set[Int] = Set.empty) {
    def result: List[List[Int]] = (1 to 9).toList.combinations(addend).filter(item => item.sum == total).filter(item=>secure.subsetOf(item.toSet)).toList
  }

  val approach1 = Approach1(10, 3, Set(7))
  println(approach1.result)
  val approachInline = ApproachInline(10,3, Set(7))
  println(approachInline.result)
}

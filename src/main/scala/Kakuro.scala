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
    def comb: List[List[Int]] = (1 to 9).toList.combinations(addend).filter(item => item.sum == total).filter(item=>secure.subsetOf(item.toSet)).toList
  }

  case class Spaces(total: Int, addends: Int, secure: Set[Int] = Set.empty) {
    def comb: List[List[Int]] = (1 to 9).toList.combinations(addends).filter(item => item.sum == total).filter(item=>secure.subsetOf(item.toSet)).toList
  }
}

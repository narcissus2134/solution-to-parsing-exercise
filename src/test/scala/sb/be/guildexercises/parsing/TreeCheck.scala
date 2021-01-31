package sb.be.guildexercises.parsing

import org.scalacheck._
import Gen._ , Prop.forAll

object TreeCheck extends Properties("Tree") {

  def genValue: Gen[String] = stringOfN(5, alphaNumChar)

  def genLeaf: Gen[Tree] = genValue.map(Tree(_,Nil))

  def genNode(maxDepth: Int): Gen[Tree] = for {
    value    <- genValue
    children <- listOf(lzy(genTree(maxDepth - 1)))
  } yield Tree(value, children)

  def genTree(maxDepth: Int): Gen[Tree] = if(maxDepth > 0) oneOf(genLeaf, lzy(genNode(maxDepth - 1))) else genLeaf

  def finiteTrees: Gen[Tree] = sized{ size =>
    val maxDepth = math.ceil(math.log(size)).intValue()
    genTree(maxDepth)
  }

  property("formatted trees should be parsed back to original") = forAll(finiteTrees) { t: Tree =>
    val str = Tree.format(t)
    Tree.parse(str).exists(t.==)
  }
}

package sb.be.guildexercises.parsing

final case class Tree(value: String, children: List[Tree])
object Tree {
  def format(tree: Tree): String = ???
  def parse(input: String): Option[Tree] = ???
}

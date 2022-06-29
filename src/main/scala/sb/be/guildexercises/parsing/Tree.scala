package sb.be.guildexercises.parsing

import cats.data.StateT

final case class Tree(value: String, children: List[Tree])

object Tree {



  def format(tree: Tree): String =
    tree.value + "[" + tree.children.map(format).foldLeft("")(_ + _) + "]"

  def parse(input: String): Option[Tree] = {

    val initial: Stack[Tree, Unit] = StateT.pure()

    input
      .foldLeft (initial) ( (acc, c) => acc flatMap (_ => readChar(c)) )
      .runS (List(emptyTree, root))
      .flatMap {
        case List(Tree("", Nil), Tree("root", List(x))) => Some(x) //band-aid; not happy with this
        case _                                          => None    //too many [, or too few ]
      }
  }






  private def emptyTree: Tree = Tree("", Nil)
  private def root: Tree      = Tree("root", Nil)

  private type Stack[A, B] = StateT[Option, List[A], B]

  private implicit class TreeOps (tree: Tree) {

    def appendChar(c: Char): Tree = {
      Tree(tree.value + c, tree.children)
    }

    def appendChild(x: Tree): Tree = {
      Tree(tree.value, tree.children ::: List(x))
    }
  }

  private def readChar: Char => Stack[Tree, Unit] = {
    case c if c.isLetterOrDigit => appendToTopTree(c)
    case '['                    => increaseStack
    case ']'                    => decreaseStack
    case _                      => StateT setF None
  }

  private def push[A](x: A): Stack[A, Unit] = StateT modify (x :: _)

  private def pop[A]: Stack[A, A] = StateT {
    case Nil     => None
    case x :: xs => Some((xs, x))
  }

  private def guard[A]: Boolean => Stack[A, Unit] = {
    case true  => StateT pure ()
    case false => StateT setF None
  }

  private def appendToTopTree(c: Char): Stack[Tree, Unit] = for {
    x <- pop
    _ <- push(x appendChar c)
  } yield ()

  private def increaseStack: Stack[Tree, Unit] = for {
    _ <- push(emptyTree)
  } yield ()

  private def decreaseStack: Stack[Tree, Unit] = for {
    x <- pop
    y <- pop
    z <- pop
    _ <- guard(x == emptyTree)
    _ <- push(z appendChild y)
    _ <- push(x)
  } yield ()

}


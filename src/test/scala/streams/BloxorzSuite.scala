package streams

import org.junit._
import org.junit.Assert.assertEquals

import Bloxorz._

class BloxorzSuite {
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  @Test def `terrain test`: Unit =
    new Level1 {
      println(terrain(Pos(-1,-1)), terrain(Pos(-1,0)), terrain(Pos(-1,1)), terrain(Pos(-1,2)), terrain(Pos(-1,3)), terrain(Pos(-1,4)), terrain(Pos(-1,5)), terrain(Pos(-1,6)), terrain(Pos(-1,7)), terrain(Pos(-1,8)), terrain(Pos(-1,9)), terrain(Pos(-1,10)))
      println(terrain(Pos(0,-1)), terrain(Pos(0,0)), terrain(Pos(0,1)), terrain(Pos(0,2)), terrain(Pos(0,3)), terrain(Pos(0,4)), terrain(Pos(0,5)), terrain(Pos(0,6)), terrain(Pos(0,7)), terrain(Pos(0,8)), terrain(Pos(0,9)), terrain(Pos(0,10)))
      println(terrain(Pos(1,-1)), terrain(Pos(1,0)), terrain(Pos(1,1)), terrain(Pos(1,2)), terrain(Pos(1,3)), terrain(Pos(1,4)), terrain(Pos(1,5)), terrain(Pos(1,6)), terrain(Pos(1,7)), terrain(Pos(1,8)), terrain(Pos(1,9)), terrain(Pos(1,10)))
      println(terrain(Pos(2,-1)), terrain(Pos(2,0)), terrain(Pos(2,1)), terrain(Pos(2,2)), terrain(Pos(2,3)), terrain(Pos(2,4)), terrain(Pos(2,5)), terrain(Pos(2,6)), terrain(Pos(2,7)), terrain(Pos(2,8)), terrain(Pos(2,9)), terrain(Pos(2,10)))
      println(terrain(Pos(3,-1)), terrain(Pos(3,0)), terrain(Pos(3,1)), terrain(Pos(3,2)), terrain(Pos(3,3)), terrain(Pos(3,4)), terrain(Pos(3,5)), terrain(Pos(3,6)), terrain(Pos(3,7)), terrain(Pos(3,8)), terrain(Pos(3,9)), terrain(Pos(3,10)))
      println(terrain(Pos(4,-1)), terrain(Pos(4,0)), terrain(Pos(4,1)), terrain(Pos(4,2)), terrain(Pos(4,3)), terrain(Pos(4,4)), terrain(Pos(4,5)), terrain(Pos(4,6)), terrain(Pos(4,7)), terrain(Pos(4,8)), terrain(Pos(4,9)), terrain(Pos(4,10)))
      println(terrain(Pos(5,-1)), terrain(Pos(5,0)), terrain(Pos(5,1)), terrain(Pos(5,2)), terrain(Pos(5,3)), terrain(Pos(5,4)), terrain(Pos(5,5)), terrain(Pos(5,6)), terrain(Pos(5,7)), terrain(Pos(5,8)), terrain(Pos(5,9)), terrain(Pos(5,10)))
      println(terrain(Pos(6,-1)), terrain(Pos(6,6)), terrain(Pos(6,1)), terrain(Pos(6,2)), terrain(Pos(6,3)), terrain(Pos(6,4)), terrain(Pos(6,5)), terrain(Pos(6,6)), terrain(Pos(6,7)), terrain(Pos(6,8)), terrain(Pos(6,9)), terrain(Pos(6,10)))
    }

  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  @Test def `findChar test`: Unit =
    new Level1 {
      println(startPos)
      println(goal)
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }

  @Test def `neighborsWithHistory test`: Unit =
    new Level1 {
      println(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toList)
    }

  @Test def `newNeighborsOnly test1`: Unit =
    new Level1 {
      println(newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).to(LazyList),
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      ).toList)
    }

  @Test def `newNeighborsOnly test2`: Unit =
    new Level1 {
      println(newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).to(LazyList),
        Set(Block(Pos(2,1),Pos(3,1)), Block(Pos(1,1),Pos(1,1)))
      ).toList)
    }

  @Test def `from test`: Unit =
    new Level1 {
      from(LazyList((startBlock, List())), Set()).toList.foreach(println)
    }

  @Test def `solution test`: Unit =
    new Level1 {
      println(solve(solution))
    }

  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }

  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

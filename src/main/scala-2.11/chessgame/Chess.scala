package chessgame

object Chess extends Moves {
    val p = "Z1"
    val xOpt = p(0) match {
        case 'A' => Some(N1)
        case 'B' => Some(N2)
        case 'C' => Some(N3)
        case 'D' => Some(N4)
        case 'E' => Some(N5)
        case 'F' => Some(N6)
        case 'G' => Some(N7)
        case 'H' => Some(N8)
        case _ => None
    }
    val yOpt = p(1) match {
        case '1' => Some(N1)
        case '2' => Some(N2)
        case '3' => Some(N3)
        case '4' => Some(N4)
        case '5' => Some(N5)
        case '6' => Some(N6)
        case '7' => Some(N7)
        case '8' => Some(N8)
        case _ => None
    }

    for {
        x <- xOpt
        y <- yOpt
    } yield Pos(x, y)


    def moves(piece: Piece, pos: Pos, color: Color, board: Map[Pos, (Piece, Color)]): List[Pos] = {
        piece match {
            case Queen =>
                allDirs.flatMap(followPositions(pos, _, Nil))
            case King =>
                allDirs.flatMap(f => f(pos))
            case Rook =>
                straightDirs.flatMap(followPositions(pos, _, Nil))
            case Bishop =>
                angleDirs.flatMap(followPositions(pos, _, Nil))
            case Pawn =>
                val direction: Pos => Option[Pos] =
                    color match {
                        case Black => _.bottom
                        case White => _.top
                    }
                PawnMoves.moves(pos, board, color, direction)
            case Knight =>
                List(
                    follow(_.left, _.left, _.top)(pos),
                    follow(_.left, _.left, _.bottom)(pos),
                    follow(_.right, _.right, _.top)(pos),
                    follow(_.right, _.right, _.bottom)(pos),
                    follow(_.top, _.top, _.left)(pos),
                    follow(_.top, _.top, _.right)(pos),
                    follow(_.bottom, _.bottom, _.left)(pos),
                    follow(_.bottom, _.bottom, _.right)(pos)
                ).flatten
        }
    }
}

trait Moves {
    val angleDirs: List[Pos => Option[Pos]] = List(
        p => for {p1 <- p.left; p2 <- p1.top} yield p2,
        p => for {p1 <- p.left; p2 <- p1.bottom} yield p2,
        p => for {p1 <- p.right; p2 <- p1.top} yield p2,
        p => for {p1 <- p.right; p2 <- p1.bottom} yield p2
    )

    val straightDirs: List[Pos => Option[Pos]] = List(
        p => p.right,
        p => p.left,
        p => p.top,
        p => p.bottom
    )

    val allDirs: List[Pos => Option[Pos]] = angleDirs ++ straightDirs

    def followPositions(pos: Pos, f: Pos => Option[Pos], acc: List[Pos]): List[Pos] =
        f(pos) match {
            case None => acc
            case Some(nextPos) => followPositions(nextPos, f, nextPos :: acc)
        }

    def follow(moves: (Pos => Option[Pos])*)(p: Pos): Option[Pos] = {
        def rec(posOpt: Option[Pos], moves: (Pos => Option[Pos])*): Option[Pos] =
            posOpt match {
                case None => None
                case Some(pos) =>
                    if (moves.isEmpty) Some(pos)
                    else rec(moves.head(pos), moves.tail: _*)
            }
        rec(Some(p), moves: _*)
    }
}

object PawnMoves extends Moves {
    def moves(pos: Pos, board: Map[Pos, (Piece, Color)], currentColor: Color, direction: (Pos) => Option[Pos]): List[Pos] = {
        val pawnLine = currentColor match {
            case White => N7
            case Black => N2
        }

        def validMoves: List[Pos] = {
            val moves =
                if (pos.y != pawnLine) List(follow(direction)_)
                else List(follow(direction)_, follow(direction, direction)_)
            val movedPositions = moves.flatMap(_(pos))
            movedPositions.filter(board.get(_).isEmpty)
        }

        def validAttacks: List[Pos] = {
            val attacks = List(follow(direction, _.left)_, follow(direction, _.right)_)
            val attackedPositions = attacks.flatMap(_(pos))
            attackedPositions.filter { p =>
                board.get(p) match {
                    case Some(color) if currentColor.opponent == color => true
                    case _ => false
                }
            }
        }

        validMoves ::: validAttacks
    }
}

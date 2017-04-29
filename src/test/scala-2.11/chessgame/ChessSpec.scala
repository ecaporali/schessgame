package chessgame

import org.specs2.Specification

class ChessSpec extends Specification {
    def is = s2"""
        Pawn moves spec
        pawn should be able to move two square or one square front if moving for the first time $moves
        pawn should ...
    """

    def moves = {
        true must_== true
    }

}

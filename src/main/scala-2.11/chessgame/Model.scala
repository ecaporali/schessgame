package chessgame

sealed trait Piece
case object Knight extends Piece
case object Queen extends Piece
case object King extends Piece
case object Rook extends Piece
case object Bishop extends Piece
case object Pawn extends Piece

case class Pos(x: N, y: N) {
    def left: Option[Pos] = x.prev.map(Pos(_, y))
    def right: Option[Pos] = x.next.map(Pos(_, y))
    def top: Option[Pos] = y.prev.map(Pos(x, _))
    def bottom: Option[Pos] = y.next.map(Pos(x, _))
    def leftTop: Option[Pos] = newPos(x.prev, y.prev)
    def rightTop: Option[Pos] = newPos(x.next, y.prev)
    def leftBottom: Option[Pos] = newPos(x.prev, y.next)
    def rightBottom: Option[Pos] = newPos(x.next, y.next)
    private def newPos(newX: Option[N], newY: Option[N]) =
        for {
            x <- newX
            y <- newY
        } yield Pos(x, y)
}

sealed trait N {
    def prev: Option[N]
    def next: Option[N]
}

case object N1 extends N {
    override def prev: Option[N] = None
    override def next: Option[N] = Some(N2)
}
case object N2 extends N {
    override def prev: Option[N] = Some(N1)
    override def next: Option[N] = Some(N3)
}
case object N3 extends N {
    override def prev: Option[N] = Some(N2)
    override def next: Option[N] = Some(N4)
}
case object N4 extends N {
    override def prev: Option[N] = Some(N3)
    override def next: Option[N] = Some(N5)
}
case object N5 extends N {
    override def prev: Option[N] = Some(N4)
    override def next: Option[N] = Some(N6)
}
case object N6 extends N {
    override def prev: Option[N] = Some(N5)
    override def next: Option[N] = Some(N7)
}
case object N7 extends N {
    override def prev: Option[N] = Some(N6)
    override def next: Option[N] = Some(N8)
}
case object N8 extends N {
    override def prev: Option[N] = Some(N7)
    override def next: Option[N] = None
}

sealed trait Color {
    def opponent: Color
}

case object Black extends Color {
    def opponent = White
}

case object White extends Color {
    def opponent = Black
}
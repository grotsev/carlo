package kz.greetgo.dtlang

/**
  * Created by den on 12.07.16.
  */
sealed abstract class DtType {}

case class Bool(bool: Boolean) extends DtType
case class Num(num: BigDecimal) extends DtType
case class Str(str: String) extends DtType
case class Dat(dat: BigDecimal) extends DtType

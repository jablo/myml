package org.zapto.jablo.myml

case object True extends Const {
  override def ==(c: Const): Const = c match {
    case True  => True
    case False => False
  }
  override def !=(c: Const): Const = c match {
    case True  => False
    case False => True
  }
  override def &&(c: Const): Const = c match {
    case True  => True
    case False => False
  }
  override def ||(c: Const): Const = c match {
    case True  => True
    case False => True
  }
  override def unary_! : Const = False
  override def infix: String = "true"
}

case object False extends Const {
  override def ==(c: Const): Const = c match {
    case False => True
    case True  => False
  }
  override def !=(c: Const): Const = c match {
    case False => False
    case True  => True
  }
  override def &&(c: Const): Const = c match {
    case False => False
    case True  => False
  }
  override def ||(c: Const): Const = c match {
    case True  => True
    case False => False
  }
  override def unary_! : Const = True
  override def infix: String = "false"
}


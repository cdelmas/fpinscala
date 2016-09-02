package fpinscala.propertybasedtesting

/**
  * Created by c.delmas on 22/07/2016.
  */
trait Prop {
  def check: Boolean
}

object Prop {
  def &&(p:Prop): Prop = new Prop {
    override def check: Boolean = this.check && p.check
  }
}

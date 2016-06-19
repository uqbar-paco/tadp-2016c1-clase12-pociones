package tadp

object Clase {

  /**
    * suerte, convencimiento, fuerza
    */
  type Niveles = (Int, Int, Int)
  type Persona = (String, Niveles)

  val personas = List(
    ("Harry", (11, 5, 4)),
    ("Ron", (6, 4, 6)),
    ("Hermione", (8, 12, 2)),
    ("Draco", (7, 9, 6))
  )

  type Efecto = Niveles => Niveles

  def mapNiveles(f: Int => Int, niveles: Niveles): Niveles =
    (f(niveles._1), f(niveles._2), f(niveles._3))

  def mapNiveles2(f: Int => Int, niveles: Niveles): Niveles =
    niveles match {
      case (a, b, c) => (f(a), f(b), f(c))
    }

  def mapNiveles3(f: Int => Int, niveles: Niveles): Niveles = {
    val (a, b, c) = niveles
    (f(a), f(b), f(c))
  }

  def mapNiveles4(f: Int => Int)(niveles: Niveles): Niveles = {
    val (a, b, c) = niveles
    (f(a), f(b), f(c))
  }

  def duplica(niveles: Niveles): Niveles =
    mapNiveles(_ * 2, niveles)

  val duplica1: Efecto = duplica _

  val duplica2: Efecto = niveles =>
    mapNiveles(_ * 2, niveles)

  val duplica3: Efecto = mapNiveles(_ * 2, _)

  object duplica4 extends (Niveles => Niveles) {
    def apply(niveles: Niveles): Niveles = duplica2(niveles)
  }

  duplica4((1, 2, 3))

  val duplica5: Efecto = duplica4

  val duplica6: Efecto = mapNiveles4(_ * 2)
  val alMenos7_2: Efecto = mapNiveles4(_.max(7))

  def alMenos7(niveles: Niveles): Niveles =
    mapNiveles(_.max(7), niveles)


  val toList: Niveles => List[Int] = { niveles =>
    val (a, b, c) = niveles
    List(a, b, c)
  }

  val sumaTodos: Niveles => Int =
    toList.andThen(_.sum)

  def max(niveles: Niveles) =
    toList(niveles).reduce(_ max _)

  def max2(niveles: Niveles) = {
    val (a, b, c) = niveles
    a max b max c
  }

  def foldNiveles(niveles: Niveles)(f: (Int, Int) => Int) =
    toList(niveles).reduce(f)


  def min(niveles: Niveles) = toList(niveles).min

  val niveles: Persona => Niveles = persona._2
  
  val sumaTodos: Persona=>Int =
    sumaTodos.compose(niveles)


}

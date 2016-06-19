val x = (1, 2, 3)
val y = Tuple3.unapply(x)

case class MiPersona(nombre: String, apellido: String)

MiPersona.apply("hola", "chau")
MiPersona.unapply(MiPersona("hola", "chau"))


def saluda(saludo: String)(p: MiPersona) =
  saludo + p.nombre

val buenosDias = saluda("buenos dias") _
buenosDias.apply(MiPersona("pedro", "perez"))

def m(entero: Int): Int = entero + 1

List(1, 2, 3).map(m)


def m2(a: Int, b: Int, c: Int): Int = 1

val ppp = (m2 _).curried
ppp(1)(2)(3)

(m2 _).tupled

def giles(list: List[Int])(f: Int => Int): Int = {
  f(list(0))
}

giles(List(1,2,3)) { elemento =>
  elemento + 1
}







import scala.::

object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal
  case class Dog(override val name: String) extends Animal



  case class Shelter[T <: Animal](animal_list: List[T]) {
    def +[A >: T <: Animal](new_animal: A): Shelter[A] ={
      val new_shelter = new Shelter[A](new_animal :: animal_list)
      new_shelter
    }

    def ++[A <: Animal](another_shelter: Shelter[A]): Shelter[Animal] ={
      val new_shelter = Shelter(another_shelter.animal_list ::: animal_list)
      new_shelter
    }

    def getNames: List[String] = {
      var animals_names = List[String]()
      for (animal: Animal <- animal_list)
        animals_names = animal.name :: animals_names
      animals_names
    }

    def feed(food: Food[T]): List[String] ={
      var res: List[String] = List[String]()
      for (animal <- animal_list) res :: List(animal)
      res
    }
  }



  trait Food [-T <: Animal]{
    val food_name: String
    def feed(animal: T): String = animal.name + " eats " + food_name
  }

  case object Meat extends Food[Animal]{
    override val food_name = "meat"
  }

  case object Milk extends Food[Cat] {
    override val food_name: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override val food_name: String = "bread"
  }
}

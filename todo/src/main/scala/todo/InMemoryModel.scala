package todo

import cats.implicits.*
import scala.collection.mutable
import todo.data.*
import scala.concurrent.ExecutionContext
import todo.InMemoryModel.defaultTasks

/**
 * The InMemoryModel is a Model that stores all the tasks in RAM, and hence they
 * are lost when the server restarts.
 *
 * You should modify this file.
 */

object InMemoryModel extends Model:
  /* These are the tasks the application starts with. You can change these if you want. */
  val defaultTasks = List(
    Id(0) -> Task(State.completedNow, "Complete Effective Scala Week 2", None, List(Tag("programming"), Tag("scala"))),
    Id(1) -> Task(State.Active, "Complete Effective Scala Week 3", Some("Finish the todo list exercise"), List(Tag("programming"), Tag("scala"), Tag("encapsulation"), Tag("sbt"))),
    Id(2) -> Task(State.Active, "Make a sandwich", Some("Cheese and salad or ham and tomato?"), List(Tag("food"), Tag("lunch")))
  )

  /* Every Task is associated with an Id. Ids must be unique. */
  private val idGenerator = IdGenerator(Id(3))

  /* The idStore stores the association between Ids and Tasks. We use a
   * LinkedHashMap so we can access elements in insertion order. We need to keep
   * a stable order so the UI doesn't jump around, which would be confusing to
   * the user.
   *
   * Note that this data structure is not safe to use with concurrent access.
   * This doesn't matter in this case study, but in a real situation it would be
   * a problem. In a future week we'll learn the techniques to address this. */
  private val idStore: mutable.LinkedHashMap[Id, Task] =
    mutable.LinkedHashMap.from(defaultTasks)

  // Crear una nueva tarea y devolver el Id asociado
  def create(task: Task): Id = {
    val id = idGenerator.nextId()
    idStore += (id -> task)  // Agrega la tarea al idStore con el nuevo Id
    id
  }
  
  // Leer la tarea asociada con el Id proporcionado
  def read(id: Id): Option[Task] =
    idStore.get(id)

  // Marcar una tarea como completada y devolver la tarea actualizada
  def complete(id: Id): Option[Task] = {
    idStore.get(id).map { task =>
      val updatedTask = task.copy(state = State.completedNow)
      idStore.update(id, updatedTask)
      updatedTask
    }
  }

  // Actualizar la tarea con el Id dado usando la función proporcionada
  def update(id: Id)(f: Task => Task): Option[Task] =
    idStore.updateWith(id) {
      case Some(task) => Some(f(task))
      case None => None
    }

  // Eliminar la tarea con el Id dado y devolver true si fue eliminada
  def delete(id: Id): Boolean =
    idStore.remove(id).isDefined

  // Devolver todas las tareas ordenadas por Id
  def tasks: Tasks = {
    Tasks(idStore.toList)
  }

  // Devolver todas las etiquetas en uso
  def tags: Tags = {
    Tags(idStore.values.flatMap(_.tags).toList.distinct)
  }

  // Devolver todas las tareas que tienen la etiqueta proporcionada
  def tasks(tag: Tag): Tasks = {
    Tasks(idStore.filter { case (_, task) => task.tags.contains(tag) }.toList)
  }

  // Limpiar todas las tareas del idStore
  def clear(): Unit =
    idStore.clear()
